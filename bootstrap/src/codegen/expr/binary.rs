//! Binary and unary operation compilation

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_binary(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Handle logical AND with short-circuit evaluation
        if matches!(op, BinOp::And) {
            return self.compile_logical_and(left, right);
        }

        // Handle logical OR with short-circuit evaluation
        if matches!(op, BinOp::Or) {
            return self.compile_logical_or(left, right);
        }

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

            // Handle dereference assignment (e.g., *ptr = val, *(ptr + offset) = val)
            if let Expr::Deref { operand, .. } = left {
                return self.compile_deref_assign(operand, rhs);
            }

            // Handle array index assignment (e.g., arr[i] = val)
            if let Expr::Index { array, index, .. } = left {
                let (elem_ptr, elem_type) = self.compile_index_ptr(array, index)?;

                // Coerce the value to the element type if needed
                let coerced_rhs = if rhs.is_int_value() && elem_type.is_int_type() {
                    let rhs_int = rhs.into_int_value();
                    let elem_int_type = elem_type.into_int_type();
                    let rhs_width = rhs_int.get_type().get_bit_width();
                    let elem_width = elem_int_type.get_bit_width();

                    if rhs_width > elem_width {
                        // Truncate (e.g., i64 -> i32)
                        self.builder.build_int_truncate(rhs_int, elem_int_type, "trunc").unwrap().into()
                    } else if rhs_width < elem_width {
                        // Sign extend (e.g., i32 -> i64)
                        self.builder.build_int_s_extend(rhs_int, elem_int_type, "sext").unwrap().into()
                    } else {
                        rhs
                    }
                } else {
                    rhs
                };

                self.builder.build_store(elem_ptr, coerced_rhs).unwrap();
                return Ok(coerced_rhs);
            }

            return Err(CodegenError::InvalidAssignment);
        }

        let lhs = self.compile_expr(left)?;
        let rhs = self.compile_expr(right)?;

        // Handle pointer operations (comparison and arithmetic)
        if lhs.is_pointer_value() || rhs.is_pointer_value() {
            // Pointer arithmetic: ptr + int, ptr - int
            if lhs.is_pointer_value() && rhs.is_int_value() {
                let ptr = lhs.into_pointer_value();
                let offset = rhs.into_int_value();

                // Extend offset to i64 if needed
                let offset_i64 = if offset.get_type().get_bit_width() < 64 {
                    self.builder.build_int_s_extend(offset, self.context.i64_type(), "offset_ext").unwrap()
                } else {
                    offset
                };

                match op {
                    BinOp::Add => {
                        // Use GEP for pointer arithmetic (type-aware)
                        // Note: LLVM opaque pointers use i8 as the element type for byte offsets
                        let result = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                ptr,
                                &[offset_i64],
                                "ptr_add"
                            ).unwrap()
                        };
                        return Ok(result.into());
                    }
                    BinOp::Sub => {
                        // Subtract: ptr - offset = ptr + (-offset)
                        let neg_offset = self.builder.build_int_neg(offset_i64, "neg_offset").unwrap();
                        let result = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                ptr,
                                &[neg_offset],
                                "ptr_sub"
                            ).unwrap()
                        };
                        return Ok(result.into());
                    }
                    _ => {}
                }
            }

            // Pointer comparisons
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
                _ => return Err(CodegenError::NotImplemented(format!(
                    "pointer operations only support +, -, ==, and !=; got '{:?}'", op
                ))),
            };

            return Ok(result.into());
        }

        // For == and !=, try Eq trait dispatch first (for ALL types, not just structs)
        // This makes generic functions like fn eq<T: Eq>(a: &T, b: &T) work with primitives
        if matches!(op, BinOp::Eq | BinOp::Ne) {
            if let Some(type_name) = self.get_type_name_for_eq(left)? {
                // Check if this type has an `eq` method
                if let Some(eq_method) = self.type_methods
                    .get(&type_name)
                    .and_then(|methods| methods.get("eq"))
                    .cloned()
                {
                    // Call Type__eq(&lhs, &rhs)
                    let fn_value = self.module
                        .get_function(&eq_method)
                        .ok_or_else(|| CodegenError::UndefinedFunction(eq_method.clone()))?;

                    // Create temporary storage for both operands and pass pointers
                    let lhs_ptr = {
                        let temp = self.builder.build_alloca(lhs.get_type(), "eq_lhs").unwrap();
                        self.builder.build_store(temp, lhs).unwrap();
                        temp
                    };

                    let rhs_ptr = {
                        let temp = self.builder.build_alloca(rhs.get_type(), "eq_rhs").unwrap();
                        self.builder.build_store(temp, rhs).unwrap();
                        temp
                    };

                    let call_site = self.builder
                        .build_call(fn_value, &[lhs_ptr.into(), rhs_ptr.into()], "eq_result")
                        .unwrap();

                    let eq_result = match call_site.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(val) => val,
                        inkwell::values::ValueKind::Instruction(_) => {
                            return Err(CodegenError::NotImplemented(
                                "Eq::eq must return a value".to_string()
                            ));
                        }
                    };

                    // For !=, negate the result
                    if matches!(op, BinOp::Ne) {
                        let bool_val = eq_result.into_int_value();
                        let negated = self.builder.build_int_compare(
                            inkwell::IntPredicate::EQ,
                            bool_val,
                            self.context.bool_type().const_zero(),
                            "ne_result"
                        ).unwrap();
                        return Ok(negated.into());
                    }

                    return Ok(eq_result);
                }
            }

            // No Eq impl found - fall through to raw comparison for primitives,
            // or error for structs
            if lhs.is_struct_value() || rhs.is_struct_value() {
                return Err(CodegenError::NotImplemented(
                    format!("cannot compare struct values with '==' without Eq trait implementation")
                ));
            }
            // Fall through to raw LLVM comparison for primitives without Eq impl
        }

        // For <, <=, >, >=, try Ord trait dispatch (for ALL types, not just structs)
        // This makes generic functions like fn max<T: Ord>(a: &T, b: &T) work with primitives
        if matches!(op, BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge) {
            if let Some(type_name) = self.get_type_name_for_eq(left)? {
                // Check if this type has a `cmp` method (Ord trait)
                if let Some(cmp_method) = self.type_methods
                    .get(&type_name)
                    .and_then(|methods| methods.get("cmp"))
                    .cloned()
                {
                    // Call Type__cmp(&lhs, &rhs) which returns Ordering enum
                    let fn_value = self.module
                        .get_function(&cmp_method)
                        .ok_or_else(|| CodegenError::UndefinedFunction(cmp_method.clone()))?;

                    // Create temporary storage for both operands and pass pointers
                    let lhs_ptr = {
                        let temp = self.builder.build_alloca(lhs.get_type(), "cmp_lhs").unwrap();
                        self.builder.build_store(temp, lhs).unwrap();
                        temp
                    };

                    let rhs_ptr = {
                        let temp = self.builder.build_alloca(rhs.get_type(), "cmp_rhs").unwrap();
                        self.builder.build_store(temp, rhs).unwrap();
                        temp
                    };

                    let call_site = self.builder
                        .build_call(fn_value, &[lhs_ptr.into(), rhs_ptr.into()], "cmp_result")
                        .unwrap();

                    let cmp_result = match call_site.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(val) => val,
                        inkwell::values::ValueKind::Instruction(_) => {
                            return Err(CodegenError::NotImplemented(
                                "Ord::cmp must return a value".to_string()
                            ));
                        }
                    };

                    // Ordering is an enum: Less=0, Equal=1, Greater=2
                    // Extract the tag (first field of the struct)
                    let ordering_struct = cmp_result.into_struct_value();
                    let tag = self.builder
                        .build_extract_value(ordering_struct, 0, "ordering_tag")
                        .unwrap()
                        .into_int_value();

                    // Define tag constants
                    let less_tag = self.context.i32_type().const_int(0, false);
                    let equal_tag = self.context.i32_type().const_int(1, false);
                    let greater_tag = self.context.i32_type().const_int(2, false);

                    // Compare based on operator:
                    // a < b   =>  cmp(a, b) == Less
                    // a <= b  =>  cmp(a, b) != Greater
                    // a > b   =>  cmp(a, b) == Greater
                    // a >= b  =>  cmp(a, b) != Less
                    let result = match op {
                        BinOp::Lt => self.builder.build_int_compare(
                            inkwell::IntPredicate::EQ, tag, less_tag, "lt_result"
                        ).unwrap(),
                        BinOp::Le => self.builder.build_int_compare(
                            inkwell::IntPredicate::NE, tag, greater_tag, "le_result"
                        ).unwrap(),
                        BinOp::Gt => self.builder.build_int_compare(
                            inkwell::IntPredicate::EQ, tag, greater_tag, "gt_result"
                        ).unwrap(),
                        BinOp::Ge => self.builder.build_int_compare(
                            inkwell::IntPredicate::NE, tag, less_tag, "ge_result"
                        ).unwrap(),
                        _ => unreachable!(),
                    };

                    return Ok(result.into());
                }
            }

            // No Ord impl found - fall through to raw comparison for primitives,
            // or error for structs
            if lhs.is_struct_value() || rhs.is_struct_value() {
                return Err(CodegenError::NotImplemented(
                    format!("cannot compare struct values with '<'/'>'/'<='/'>='' without Ord trait implementation")
                ));
            }
            // Fall through to raw LLVM comparison for primitives without Ord impl
        }

        // For arithmetic operators (+, -, *, /, %), try trait dispatch for struct types
        // Primitives use raw LLVM operations below
        if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod) {
            if lhs.is_struct_value() || rhs.is_struct_value() {
                if let Some(type_name) = self.get_type_name_for_eq(left)? {
                    // Map operator to trait method name
                    let method_name = match op {
                        BinOp::Add => "add",
                        BinOp::Sub => "sub",
                        BinOp::Mul => "mul",
                        BinOp::Div => "div",
                        BinOp::Mod => "rem",
                        _ => unreachable!(),
                    };

                    // Check if this type has the trait method
                    if let Some(trait_method) = self.type_methods
                        .get(&type_name)
                        .and_then(|methods| methods.get(method_name))
                        .cloned()
                    {
                        // Call Type__method(lhs, rhs) - arithmetic traits take by value
                        let fn_value = self.module
                            .get_function(&trait_method)
                            .ok_or_else(|| CodegenError::UndefinedFunction(trait_method.clone()))?;

                        let call_site = self.builder
                            .build_call(fn_value, &[lhs.into(), rhs.into()], "arith_result")
                            .unwrap();

                        let result = match call_site.try_as_basic_value() {
                            inkwell::values::ValueKind::Basic(val) => val,
                            inkwell::values::ValueKind::Instruction(_) => {
                                return Err(CodegenError::NotImplemented(
                                    format!("{}::{} must return a value", type_name, method_name)
                                ));
                            }
                        };

                        return Ok(result);
                    }
                }

                // No trait impl found for struct type - error
                return Err(CodegenError::NotImplemented(
                    format!("binary operator '{:?}' not supported for struct types without trait implementation", op)
                ));
            }
            // Fall through to raw LLVM operations for primitives
        }

        // Handle other binary ops on structs (not arithmetic or comparison)
        if lhs.is_struct_value() || rhs.is_struct_value() {
            return Err(CodegenError::NotImplemented(
                format!("binary operator '{:?}' not supported for struct types", op)
            ));
        }

        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();

        // Coerce integer types to the same width for binary operations
        let (lhs_int, rhs_int) = if lhs_int.get_type().get_bit_width() != rhs_int.get_type().get_bit_width() {
            let lhs_width = lhs_int.get_type().get_bit_width();
            let rhs_width = rhs_int.get_type().get_bit_width();
            if lhs_width > rhs_width {
                // Widen rhs to match lhs
                let widened = self.builder.build_int_s_extend(rhs_int, lhs_int.get_type(), "widen").unwrap();
                (lhs_int, widened)
            } else {
                // Widen lhs to match rhs
                let widened = self.builder.build_int_s_extend(lhs_int, rhs_int.get_type(), "widen").unwrap();
                (widened, rhs_int)
            }
        } else {
            (lhs_int, rhs_int)
        };

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
            _ => return Err(CodegenError::NotImplemented(format!(
                "binary operator '{:?}' not supported for integer types", op
            ))),
        };

        Ok(result.into())
    }

    pub(crate) fn compile_unary(&mut self, op: UnaryOp, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_unary_with_type(op, operand, None)
    }

    pub(crate) fn compile_unary_with_type(&mut self, op: UnaryOp, operand: &Expr, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // For negation, pass the expected type to the operand so -1 becomes the right type
        let val = self.compile_expr_with_type(operand, expected_type)?;

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

    fn compile_logical_and(&mut self, left: &Expr, right: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let current_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let bool_type = self.context.bool_type();

        // Create basic blocks
        let rhs_block = self.context.append_basic_block(current_fn, "and.rhs");
        let merge_block = self.context.append_basic_block(current_fn, "and.merge");

        // Compile left side
        let lhs = self.compile_expr(left)?;
        let lhs_int = lhs.into_int_value();
        // Ensure condition is i1 for branching (handles ptr_is_null returning i64)
        let lhs_bool = if lhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, lhs_int, lhs_int.get_type().const_zero(), "lhs_bool"
            ).unwrap()
        } else {
            lhs_int
        };

        // Save the block we're coming from for the phi
        let lhs_block = self.builder.get_insert_block().unwrap();

        // Branch: if lhs is true, evaluate rhs; else short-circuit to false
        self.builder.build_conditional_branch(lhs_bool, rhs_block, merge_block).unwrap();

        // Compile right side
        self.builder.position_at_end(rhs_block);
        let rhs = self.compile_expr(right)?;
        let rhs_int = rhs.into_int_value();
        // Ensure rhs is i1 for phi node
        let rhs_bool = if rhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, rhs_int, rhs_int.get_type().const_zero(), "rhs_bool"
            ).unwrap()
        } else {
            rhs_int
        };
        let rhs_final_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block).unwrap();

        // Merge: phi node to get the result
        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(bool_type, "and.result").unwrap();

        // If we came from lhs_block (short-circuit), result is false
        // If we came from rhs_block, result is the rhs value
        phi.add_incoming(&[
            (&bool_type.const_zero(), lhs_block),
            (&rhs_bool, rhs_final_block),
        ]);

        Ok(phi.as_basic_value())
    }

    /// Compile logical OR with short-circuit evaluation
    /// if left is true, skip right and return true
    fn compile_logical_or(&mut self, left: &Expr, right: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let current_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let bool_type = self.context.bool_type();

        // Create basic blocks
        let rhs_block = self.context.append_basic_block(current_fn, "or.rhs");
        let merge_block = self.context.append_basic_block(current_fn, "or.merge");

        // Compile left side
        let lhs = self.compile_expr(left)?;
        let lhs_int = lhs.into_int_value();
        // Ensure condition is i1 for branching (handles ptr_is_null returning i64)
        let lhs_bool = if lhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, lhs_int, lhs_int.get_type().const_zero(), "lhs_bool"
            ).unwrap()
        } else {
            lhs_int
        };

        // Save the block we're coming from for the phi
        let lhs_block = self.builder.get_insert_block().unwrap();

        // Branch: if lhs is true, short-circuit to true; else evaluate rhs
        self.builder.build_conditional_branch(lhs_bool, merge_block, rhs_block).unwrap();

        // Compile right side
        self.builder.position_at_end(rhs_block);
        let rhs = self.compile_expr(right)?;
        let rhs_int = rhs.into_int_value();
        // Ensure rhs is i1 for phi node
        let rhs_bool = if rhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, rhs_int, rhs_int.get_type().const_zero(), "rhs_bool"
            ).unwrap()
        } else {
            rhs_int
        };
        let rhs_final_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block).unwrap();

        // Merge: phi node to get the result
        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(bool_type, "or.result").unwrap();

        // If we came from lhs_block (short-circuit), result is true
        // If we came from rhs_block, result is the rhs value
        phi.add_incoming(&[
            (&bool_type.const_int(1, false), lhs_block),
            (&rhs_bool, rhs_final_block),
        ]);

        Ok(phi.as_basic_value())
    }
}
