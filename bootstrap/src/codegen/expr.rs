//! Expression compilation

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_expr_with_type(expr, None)
    }

    /// Compile an expression with an optional expected type for literal coercion
    pub(crate) fn compile_expr_with_type(&mut self, expr: &Expr, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match expr {
            Expr::Literal(lit, _) => self.compile_literal_with_type(lit, expected_type),
            Expr::Ident(name, _) => {
                // Check for use-after-move
                if self.moved_vars.contains(name) {
                    return Err(CodegenError::BorrowError(
                        format!("use of moved value: '{}'", name)
                    ));
                }
                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
                let val = self.builder.build_load(var_info.ty, var_info.ptr, name).unwrap();
                Ok(val)
            }
            Expr::Binary { op, left, right, .. } => {
                self.compile_binary(*op, left, right)
            }
            Expr::Unary { op, operand, .. } => {
                self.compile_unary(*op, operand)
            }
            Expr::Call { func, type_args, args, .. } => {
                self.compile_call(func, type_args, args)
            }
            Expr::StructInit { name, generics, fields, .. } => {
                self.compile_struct_init(name, generics, fields)
            }
            Expr::Field { object, field, .. } => {
                self.compile_field_access(object, field)
            }
            Expr::MethodCall { receiver, method, args, .. } => {
                // Check if this is an enum variant constructor (e.g., Option.Some(42))
                if let Expr::Ident(name, _) = receiver.as_ref() {
                    if let Some(enum_info) = self.enum_types.get(name).cloned() {
                        return self.compile_enum_variant_constructor(&enum_info, name, method, args);
                    }

                    // Check if this is a module function call (e.g., math.add())
                    if self.module_items.contains_key(name) {
                        return self.compile_module_function_call(name, method, args);
                    }

                    // Check if this is a static method call on a struct type (e.g., IntArray.new())
                    if self.struct_types.contains_key(name) || self.type_methods.contains_key(name) {
                        return self.compile_static_method_call(name, method, args);
                    }

                    // Check for array .len() method
                    if method == "len" && args.is_empty() {
                        if let Some(var_info) = self.variables.get(name) {
                            if var_info.ty.is_array_type() {
                                let array_ty = var_info.ty.into_array_type();
                                let len = array_ty.len();
                                return Ok(self.context.i64_type().const_int(len as u64, false).into());
                            }
                            // Check for slice .len() method
                            if var_info.ty.is_struct_type() {
                                let struct_ty = var_info.ty.into_struct_type();
                                // Slices are structs with 2 fields: { ptr, len }
                                if struct_ty.count_fields() == 2 {
                                    // Load the slice value
                                    let slice_val = self.builder.build_load(struct_ty, var_info.ptr, "slice").unwrap();
                                    // Extract length from slice (field 1)
                                    let len_val = self.builder
                                        .build_extract_value(slice_val.into_struct_value(), 1, "slice_len")
                                        .unwrap();
                                    return Ok(len_val);
                                }
                            }
                        }
                    }
                }
                // Try to resolve as a method call on a type
                self.compile_method_call(receiver, method, args)
            }
            Expr::Ref { operand, .. } => {
                // &expr - get address of the operand
                self.compile_ref(operand, false)
            }
            Expr::RefMut { operand, .. } => {
                // ~expr - get mutable address of the operand
                self.compile_ref(operand, true)
            }
            Expr::Deref { operand, .. } => {
                // *expr - dereference a pointer
                self.compile_deref(operand)
            }
            Expr::ArrayInit { elements, .. } => {
                // Extract element type from expected array type if available
                let elem_type = match expected_type {
                    Some(Type::Array(inner, _)) => Some(inner.as_ref()),
                    _ => None,
                };
                self.compile_array_init_with_type(elements, elem_type)
            }
            Expr::Index { array, index, .. } => {
                self.compile_index(array, index)
            }
            Expr::Try { operand, .. } => {
                self.compile_try_operator(operand)
            }
            Expr::Block(block) => {
                // Compile all statements in the block
                self.compile_block(block)?;
                // Return a dummy value for blocks (the last statement's value)
                Ok(self.context.i64_type().const_zero().into())
            }
            _ => {
                // TODO: implement other expressions
                Err(CodegenError::NotImplemented("expression type".to_string()))
            }
        }
    }

    pub(crate) fn compile_literal(&mut self, lit: &Literal) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_literal_with_type(lit, None)
    }

    /// Compile a literal with an optional expected type for coercion
    pub(crate) fn compile_literal_with_type(&mut self, lit: &Literal, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match lit {
            Literal::Int(n) => {
                // Coerce integer literal to expected type if specified
                let int_type = match expected_type {
                    Some(Type::I8) => self.context.i8_type(),
                    Some(Type::I16) => self.context.i16_type(),
                    Some(Type::I32) => self.context.i32_type(),
                    Some(Type::U8) => self.context.i8_type(),
                    Some(Type::U16) => self.context.i16_type(),
                    Some(Type::U32) => self.context.i32_type(),
                    Some(Type::U64) => self.context.i64_type(),
                    _ => self.context.i64_type(), // Default to i64
                };
                let val = int_type.const_int(*n as u64, true);
                Ok(val.into())
            }
            Literal::Float(n) => {
                let val: BasicValueEnum<'ctx> = match expected_type {
                    Some(Type::F32) => self.context.f32_type().const_float(*n).into(),
                    _ => self.context.f64_type().const_float(*n).into(), // Default to f64
                };
                Ok(val)
            }
            Literal::Bool(b) => {
                let val = self.context.bool_type().const_int(*b as u64, false);
                Ok(val.into())
            }
            Literal::String(s) => {
                // Create string as &[u8] slice (fat pointer: {ptr, len})
                let global = self.builder.build_global_string_ptr(s, "str").unwrap();
                let ptr = global.as_pointer_value();
                let len = self.context.i64_type().const_int(s.len() as u64, false);

                // Build slice struct type { ptr, len }
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let len_type = self.context.i64_type();
                let slice_type = self.context.struct_type(&[ptr_type.into(), len_type.into()], false);

                // Build the slice value
                let slice_val = slice_type.const_named_struct(&[ptr.into(), len.into()]);
                Ok(slice_val.into())
            }
        }
    }

    pub(crate) fn compile_binary(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Handle assignment specially - don't compile LHS yet for assignments
        if matches!(op, BinOp::Assign) {
            let rhs = self.compile_expr(right)?;

            if let Expr::Ident(name, _) = left {
                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
                self.builder.build_store(var_info.ptr, rhs).unwrap();
                return Ok(rhs);
            }

            // Handle field assignment (e.g., p.x = val)
            if let Expr::Field { object, field, .. } = left {
                let field_ptr = self.compile_field_ptr(object, field)?;
                self.builder.build_store(field_ptr, rhs).unwrap();
                return Ok(rhs);
            }

            return Err(CodegenError::InvalidAssignment);
        }

        let lhs = self.compile_expr(left)?;
        let rhs = self.compile_expr(right)?;

        // Handle pointer comparisons
        if lhs.is_pointer_value() || rhs.is_pointer_value() {
            // Convert both to pointers for comparison (int 0 becomes null pointer)
            let lhs_ptr = if lhs.is_pointer_value() {
                lhs.into_pointer_value()
            } else {
                // Convert int to null pointer for comparison
                self.context.ptr_type(AddressSpace::default()).const_null()
            };
            let rhs_ptr = if rhs.is_pointer_value() {
                rhs.into_pointer_value()
            } else {
                // Convert int to null pointer for comparison
                self.context.ptr_type(AddressSpace::default()).const_null()
            };

            let result = match op {
                BinOp::Eq => self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    self.builder.build_ptr_to_int(lhs_ptr, self.context.i64_type(), "lhs_int").unwrap(),
                    self.builder.build_ptr_to_int(rhs_ptr, self.context.i64_type(), "rhs_int").unwrap(),
                    "eq"
                ).unwrap(),
                BinOp::Ne => self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    self.builder.build_ptr_to_int(lhs_ptr, self.context.i64_type(), "lhs_int").unwrap(),
                    self.builder.build_ptr_to_int(rhs_ptr, self.context.i64_type(), "rhs_int").unwrap(),
                    "ne"
                ).unwrap(),
                _ => return Err(CodegenError::NotImplemented(format!("pointer binary op {:?}", op))),
            };

            return Ok(result.into());
        }

        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();

        let result = match op {
            BinOp::Add => self.builder.build_int_add(lhs_int, rhs_int, "add").unwrap(),
            BinOp::Sub => self.builder.build_int_sub(lhs_int, rhs_int, "sub").unwrap(),
            BinOp::Mul => self.builder.build_int_mul(lhs_int, rhs_int, "mul").unwrap(),
            BinOp::Div => self.builder.build_int_signed_div(lhs_int, rhs_int, "div").unwrap(),
            BinOp::Mod => self.builder.build_int_signed_rem(lhs_int, rhs_int, "mod").unwrap(),
            BinOp::Eq => self.builder.build_int_compare(
                inkwell::IntPredicate::EQ, lhs_int, rhs_int, "eq"
            ).unwrap(),
            BinOp::Ne => self.builder.build_int_compare(
                inkwell::IntPredicate::NE, lhs_int, rhs_int, "ne"
            ).unwrap(),
            BinOp::Lt => self.builder.build_int_compare(
                inkwell::IntPredicate::SLT, lhs_int, rhs_int, "lt"
            ).unwrap(),
            BinOp::Le => self.builder.build_int_compare(
                inkwell::IntPredicate::SLE, lhs_int, rhs_int, "le"
            ).unwrap(),
            BinOp::Gt => self.builder.build_int_compare(
                inkwell::IntPredicate::SGT, lhs_int, rhs_int, "gt"
            ).unwrap(),
            BinOp::Ge => self.builder.build_int_compare(
                inkwell::IntPredicate::SGE, lhs_int, rhs_int, "ge"
            ).unwrap(),
            BinOp::BitAnd => self.builder.build_and(lhs_int, rhs_int, "and").unwrap(),
            BinOp::BitOr => self.builder.build_or(lhs_int, rhs_int, "or").unwrap(),
            BinOp::BitXor => self.builder.build_xor(lhs_int, rhs_int, "xor").unwrap(),
            BinOp::Shl => self.builder.build_left_shift(lhs_int, rhs_int, "shl").unwrap(),
            BinOp::Shr => self.builder.build_right_shift(lhs_int, rhs_int, true, "shr").unwrap(),
            _ => return Err(CodegenError::NotImplemented(format!("binary op {:?}", op))),
        };

        Ok(result.into())
    }

    pub(crate) fn compile_unary(&mut self, op: UnaryOp, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let val = self.compile_expr(operand)?;

        match op {
            UnaryOp::Neg => {
                let int_val = val.into_int_value();
                let result = self.builder.build_int_neg(int_val, "neg").unwrap();
                Ok(result.into())
            }
            UnaryOp::BitNot => {
                let int_val = val.into_int_value();
                let result = self.builder.build_not(int_val, "not").unwrap();
                Ok(result.into())
            }
            UnaryOp::Not => {
                let int_val = val.into_int_value();
                let zero = self.context.bool_type().const_zero();
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ, int_val, zero, "lnot"
                ).unwrap();
                Ok(result.into())
            }
        }
    }
}
