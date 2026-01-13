//! Memory operations - struct init, field access, references, arrays, indexing

use super::{Codegen, CodegenError, EnumTypeInfo, BorrowState};
use crate::ast::*;
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::types::{BasicTypeEnum, BasicType};
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_struct_init(
        &mut self,
        name: &str,
        generics: &[Type],
        fields: &[(String, Expr)],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Generate monomorphized struct if needed
        let mono_name = if generics.is_empty() {
            name.to_string()
        } else {
            // Ensure monomorphized struct exists
            self.ensure_monomorphized_struct(name, generics)?
        };

        let struct_info = self
            .struct_types
            .get(&mono_name)
            .ok_or_else(|| CodegenError::UndefinedType(mono_name.clone()))?
            .clone();

        // Allocate struct on stack
        let alloca = self.builder.build_alloca(struct_info.llvm_type, name).unwrap();

        // Initialize each field
        for (field_name, field_expr) in fields {
            let field_idx = *struct_info
                .field_indices
                .get(field_name)
                .ok_or_else(|| CodegenError::UndefinedField(field_name.clone()))?;

            let field_value = self.compile_expr(field_expr)?;

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, alloca, field_idx, &format!("{}.{}", name, field_name))
                .unwrap();

            self.builder.build_store(field_ptr, field_value).unwrap();
        }

        // Load and return the struct value
        let struct_val = self
            .builder
            .build_load(struct_info.llvm_type, alloca, &format!("{}_val", name))
            .unwrap();

        Ok(struct_val)
    }

    pub(crate) fn compile_enum_variant_constructor(
        &mut self,
        enum_info: &EnumTypeInfo<'ctx>,
        enum_name: &str,
        variant_name: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let tag = *enum_info
            .variant_tags
            .get(variant_name)
            .ok_or_else(|| CodegenError::UndefinedField(format!("{}::{}", enum_name, variant_name)))?;

        // Allocate enum on stack
        let alloca = self.builder.build_alloca(enum_info.llvm_type, enum_name).unwrap();

        // Store tag at index 0
        let tag_ptr = self
            .builder
            .build_struct_gep(enum_info.llvm_type, alloca, 0, &format!("{}.tag", enum_name))
            .unwrap();
        let tag_val = self.context.i32_type().const_int(tag as u64, false);
        self.builder.build_store(tag_ptr, tag_val).unwrap();

        // Store payload values starting at index 1
        for (i, arg) in args.iter().enumerate() {
            let arg_val = self.compile_expr(arg)?;
            let payload_ptr = self
                .builder
                .build_struct_gep(enum_info.llvm_type, alloca, (i + 1) as u32, &format!("{}.payload{}", enum_name, i))
                .unwrap();
            self.builder.build_store(payload_ptr, arg_val).unwrap();
        }

        // Load and return the enum value
        let enum_val = self
            .builder
            .build_load(enum_info.llvm_type, alloca, &format!("{}_val", enum_name))
            .unwrap();

        Ok(enum_val)
    }

    pub(crate) fn compile_field_access(
        &mut self,
        object: &Expr,
        field: &str,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // If object is an identifier, check if it's an enum type or a variable
        if let Expr::Ident(name, _) = object {
            // Check if this is an enum variant access (e.g., Color.Green for unit variant)
            if let Some(enum_info) = self.enum_types.get(name).cloned() {
                // For unit variants, construct the full tagged union
                return self.compile_enum_variant_constructor(&enum_info, name, field, &[]);
            }
        }

        // Check for generic enum unit variant: EnumName<Type>.Variant
        if let Expr::StructInit { name, generics, fields, .. } = object {
            if fields.is_empty() && !generics.is_empty() {
                let mono_name = self.ensure_monomorphized_enum(name, generics)?;
                let enum_info = self.enum_types.get(&mono_name).cloned()
                    .ok_or_else(|| CodegenError::UndefinedType(format!("enum '{}' not found after monomorphization", mono_name)))?;
                return self.compile_enum_variant_constructor(&enum_info, &mono_name, field, &[]);
            }
        }

        // Continue with struct field access for identifiers
        if let Expr::Ident(name, _) = object {

            // Check for use-after-move
            if self.moved_vars.contains(name) {
                return Err(CodegenError::BorrowError(
                    format!("use of moved value: '{}'", name)
                ));
            }

            // Otherwise it's a struct field access
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            // Determine struct name - either direct or through reference
            let struct_name = if var_info.is_ref {
                var_info.ref_struct_name.as_ref()
            } else {
                var_info.struct_name.as_ref()
            };

            let struct_name = struct_name
                .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", name)))?;

            let struct_info = self
                .struct_types
                .get(struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                .clone();

            let field_idx = *struct_info
                .field_indices
                .get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            let field_type = struct_info.field_types[field_idx as usize];

            // If it's a reference, load the pointer first, then GEP
            let struct_ptr = if var_info.is_ref {
                // Load the pointer value (ptr -> ptr to struct -> load -> ptr to struct)
                self.builder
                    .build_load(self.context.ptr_type(AddressSpace::default()), var_info.ptr, "deref_ptr")
                    .unwrap()
                    .into_pointer_value()
            } else {
                var_info.ptr
            };

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, struct_ptr, field_idx, &format!("{}.{}", name, field))
                .unwrap();

            let field_val = self
                .builder
                .build_load(field_type, field_ptr, field)
                .unwrap();

            Ok(field_val)
        } else {
            Err(CodegenError::NotImplemented("field access on non-identifier".to_string()))
        }
    }

    pub(crate) fn compile_field_ptr(
        &mut self,
        object: &Expr,
        field: &str,
    ) -> Result<PointerValue<'ctx>, CodegenError> {
        // Get a pointer to a field (for assignment)
        if let Expr::Ident(name, _) = object {
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            // Check: cannot mutate through read-only reference (&T)
            if var_info.is_ref && !var_info.is_mut_ref {
                return Err(CodegenError::BorrowError(
                    format!("cannot mutate through read-only borrow '&{}': use '~{}' for mutable access",
                        name, name)
                ));
            }

            // Determine struct name - either direct or through reference
            let struct_name = if var_info.is_ref {
                var_info.ref_struct_name.as_ref()
            } else {
                var_info.struct_name.as_ref()
            };

            let struct_name = struct_name
                .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", name)))?;

            let struct_info = self
                .struct_types
                .get(struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                .clone();

            let field_idx = *struct_info
                .field_indices
                .get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            // If it's a reference, load the pointer first, then GEP
            let struct_ptr = if var_info.is_ref {
                self.builder
                    .build_load(self.context.ptr_type(AddressSpace::default()), var_info.ptr, "deref_ptr")
                    .unwrap()
                    .into_pointer_value()
            } else {
                var_info.ptr
            };

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, struct_ptr, field_idx, &format!("{}.{}", name, field))
                .unwrap();

            Ok(field_ptr)
        } else {
            Err(CodegenError::NotImplemented("field pointer on non-identifier".to_string()))
        }
    }

    pub(crate) fn compile_ref(&mut self, operand: &Expr, mutable: bool) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // &expr or ~expr - get the address of the operand
        // The operand must be an lvalue (something we can take address of)
        match operand {
            Expr::Ident(name, _) => {
                // Check for use-after-move
                if self.moved_vars.contains(name) {
                    return Err(CodegenError::BorrowError(
                        format!("cannot borrow '{}' because it has been moved", name)
                    ));
                }

                // Check for borrow conflicts
                if let Some(current_borrow) = self.borrowed_vars.get(name) {
                    match (current_borrow, mutable) {
                        (BorrowState::Mutable, _) => {
                            return Err(CodegenError::BorrowError(
                                format!("cannot borrow '{}': already mutably borrowed", name)
                            ));
                        }
                        (BorrowState::Shared, true) => {
                            return Err(CodegenError::BorrowError(
                                format!("cannot borrow '{}' as mutable: already borrowed as immutable", name)
                            ));
                        }
                        (BorrowState::Shared, false) => {
                            // Multiple shared borrows are OK
                        }
                    }
                }

                // Track this borrow
                let new_state = if mutable { BorrowState::Mutable } else { BorrowState::Shared };
                self.borrowed_vars.insert(name.clone(), new_state);

                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                    .clone();

                // If it's an array, create a slice: { ptr: *T, len: i64 }
                if var_info.ty.is_array_type() {
                    let array_ty = var_info.ty.into_array_type();
                    let len = array_ty.len() as u64;

                    // Get pointer to first element
                    let elem_ptr = unsafe {
                        self.builder.build_gep(
                            array_ty,
                            var_info.ptr,
                            &[self.context.i32_type().const_zero(), self.context.i32_type().const_zero()],
                            "slice_ptr"
                        ).unwrap()
                    };

                    // Build slice struct type: { ptr, len }
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let len_type = self.context.i64_type();
                    let slice_struct_type = self.context.struct_type(&[ptr_type.into(), len_type.into()], false);

                    // Allocate and populate the slice struct
                    let slice_alloca = self.create_entry_block_alloca("slice_tmp", slice_struct_type.into());

                    // Store pointer field
                    let ptr_field = self.builder
                        .build_struct_gep(slice_struct_type, slice_alloca, 0, "slice_ptr_field")
                        .unwrap();
                    self.builder.build_store(ptr_field, elem_ptr).unwrap();

                    // Store length field
                    let len_field = self.builder
                        .build_struct_gep(slice_struct_type, slice_alloca, 1, "slice_len_field")
                        .unwrap();
                    let len_val = self.context.i64_type().const_int(len, false);
                    self.builder.build_store(len_field, len_val).unwrap();

                    // Load and return the slice struct value
                    let slice_val = self.builder
                        .build_load(slice_struct_type, slice_alloca, "slice")
                        .unwrap();
                    return Ok(slice_val);
                }

                // Return the pointer value directly (it's already an address)
                Ok(var_info.ptr.into())
            }
            Expr::Field { object, field, .. } => {
                // &expr.field - get address of a field
                if let Expr::Ident(var_name, _) = object.as_ref() {
                    let var_info = self
                        .variables
                        .get(var_name)
                        .ok_or_else(|| CodegenError::UndefinedVariable(var_name.clone()))?
                        .clone();

                    let struct_name = var_info.struct_name
                        .as_ref()
                        .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", var_name)))?;

                    let struct_info = self
                        .struct_types
                        .get(struct_name)
                        .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                        .clone();

                    let field_idx = *struct_info
                        .field_indices
                        .get(field)
                        .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

                    let field_ptr = self
                        .builder
                        .build_struct_gep(struct_info.llvm_type, var_info.ptr, field_idx, &format!("{}.{}", var_name, field))
                        .unwrap();

                    Ok(field_ptr.into())
                } else {
                    Err(CodegenError::NotImplemented("reference to complex field".to_string()))
                }
            }
            _ => Err(CodegenError::NotImplemented("reference to non-lvalue".to_string())),
        }
    }

    pub(crate) fn compile_deref(&mut self, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // *expr - dereference a pointer
        let ptr_val = self.compile_expr(operand)?;
        let ptr = ptr_val.into_pointer_value();

        // For now, assume we're dereferencing to i32 (need type inference for general case)
        let val = self.builder.build_load(self.context.i32_type(), ptr, "deref").unwrap();
        Ok(val)
    }

    pub(crate) fn compile_array_init_with_type(&mut self, elements: &[Expr], elem_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if elements.is_empty() {
            return Err(CodegenError::InvalidArguments("empty array literal".to_string()));
        }

        // Compile all elements with expected element type for coercion
        let compiled: Vec<BasicValueEnum<'ctx>> = elements
            .iter()
            .map(|e| self.compile_expr_with_type(e, elem_type))
            .collect::<Result<_, _>>()?;

        // Determine element type from first element
        let elem_type = compiled[0].get_type();
        let array_type = elem_type.array_type(elements.len() as u32);

        // Create alloca for the array
        let array_alloca = self.create_entry_block_alloca("array_tmp", array_type.into());

        // Store each element
        for (i, val) in compiled.iter().enumerate() {
            let idx = self.context.i32_type().const_int(i as u64, false);
            let elem_ptr = unsafe {
                self.builder.build_gep(
                    array_type,
                    array_alloca,
                    &[self.context.i32_type().const_zero(), idx],
                    &format!("elem{}", i)
                ).unwrap()
            };
            self.builder.build_store(elem_ptr, *val).unwrap();
        }

        // Load and return the array value
        let array_val = self.builder.build_load(array_type, array_alloca, "array").unwrap();
        Ok(array_val)
    }

    pub(crate) fn compile_index(&mut self, array: &Expr, index: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // For now, handle array variable indexing
        if let Expr::Ident(name, _) = array {
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            let idx = self.compile_expr(index)?;
            let idx_val = idx.into_int_value();

            // Check if this is a slice (struct with {ptr, len} layout)
            if var_info.ty.is_struct_type() {
                let struct_ty = var_info.ty.into_struct_type();
                // Slices are structs with 2 fields: { ptr, i64 }
                if struct_ty.count_fields() == 2 {
                    // Load the slice value
                    let slice_val = self.builder.build_load(struct_ty, var_info.ptr, "slice").unwrap();

                    // Extract pointer from slice (field 0)
                    let data_ptr = self.builder
                        .build_extract_value(slice_val.into_struct_value(), 0, "slice_ptr")
                        .unwrap()
                        .into_pointer_value();

                    // Get element type from tracked slice_elem_type, fallback to i64
                    let elem_type: BasicTypeEnum = if let Some(ref ast_ty) = var_info.slice_elem_type {
                        self.llvm_type(ast_ty)?
                    } else {
                        self.context.i64_type().into()
                    };

                    // GEP to get element at index
                    let elem_ptr = unsafe {
                        self.builder.build_gep(
                            elem_type,
                            data_ptr,
                            &[idx_val],
                            "slice_elem_ptr"
                        ).unwrap()
                    };

                    let val = self.builder.build_load(elem_type, elem_ptr, "slice_elem").unwrap();
                    return Ok(val);
                }
            }

            if var_info.is_ref {
                // Indexing through a reference - load the pointer, then index
                // The pointer points to an array
                let array_ptr = self.builder.build_load(
                    self.context.ptr_type(inkwell::AddressSpace::default()),
                    var_info.ptr,
                    "array_ptr"
                ).unwrap().into_pointer_value();

                // For array references, we need to know the element type
                // For now, assume i64 (need better type tracking for arrays)
                let elem_type = self.context.i64_type();

                // GEP into the array
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        elem_type.array_type(0), // Use unsized array for GEP
                        array_ptr,
                        &[self.context.i32_type().const_zero(), idx_val],
                        "elem_ptr"
                    ).unwrap()
                };

                let val = self.builder.build_load(elem_type.as_basic_type_enum(), elem_ptr, "elem").unwrap();
                Ok(val)
            } else {
                // Direct array variable
                // Get element pointer using GEP
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        var_info.ty,
                        var_info.ptr,
                        &[self.context.i32_type().const_zero(), idx_val],
                        "elem_ptr"
                    ).unwrap()
                };

                // Get element type from array type
                let array_ty = var_info.ty.into_array_type();
                let elem_type: BasicTypeEnum = array_ty.get_element_type();

                let val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
                Ok(val)
            }
        } else {
            Err(CodegenError::NotImplemented("indexing non-variable expressions".to_string()))
        }
    }
}
