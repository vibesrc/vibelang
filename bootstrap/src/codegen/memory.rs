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

            // Get expected field type for coercion
            let expected_type = struct_info.ast_field_types.get(field_idx as usize);
            let field_value = self.compile_expr_with_type(field_expr, expected_type)?;

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

        // Get expected payload types for this variant (for type coercion)
        let payload_types = enum_info.ast_variant_payloads.get(variant_name);

        // Store payload values starting at index 1
        for (i, arg) in args.iter().enumerate() {
            // Get expected type for coercion if available
            let expected_type = payload_types.and_then(|types| types.get(i));
            let arg_val = self.compile_expr_with_type(arg, expected_type)?;
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
        // If object is an identifier, check if it's an enum type, module alias, or a variable
        if let Expr::Ident(name, _) = object {
            // Check if this is an enum variant access (e.g., Color.Green for unit variant)
            if let Some(enum_info) = self.enum_types.get(name).cloned() {
                // For unit variants, construct the full tagged union
                return self.compile_enum_variant_constructor(&enum_info, name, field, &[]);
            }

            // Check if this is a module-qualified type access (e.g., fs.File)
            if self.module_aliases.contains_key(name) {
                // Resolve as module_Type (e.g., fs.File -> fs_File)
                let qualified_name = format!("{}_{}", name, field);
                let resolved_name = self.imports.get(&qualified_name)
                    .cloned()
                    .unwrap_or(qualified_name);

                // Check if it's a struct type - return a placeholder for method chaining
                if self.struct_types.contains_key(&resolved_name) {
                    // This is used for fs.File.create() style calls
                    // Return a dummy value - the actual call handling happens in compile_call
                    return Err(CodegenError::NotImplemented(
                        format!("module type '{}' used in expression context; use '{}.{}' for static methods",
                                resolved_name, name, field)
                    ));
                }

                // Check if it's an enum type
                if let Some(enum_info) = self.enum_types.get(&resolved_name).cloned() {
                    // This allows fs.Result.Ok style syntax
                    return Err(CodegenError::NotImplemented(
                        format!("enum '{}' from module '{}' - use {}.VariantName for variants",
                                field, name, resolved_name)
                    ));
                }

                return Err(CodegenError::UndefinedType(
                    format!("'{}' not found in module '{}'", field, name)
                ));
            }
        }

        // Check for generic enum unit variant: EnumName<Type>.Variant
        if let Expr::StructInit { name, generics, fields, .. } = object {
            if fields.is_empty() && !generics.is_empty() {
                // Only handle as enum if it's actually a generic enum
                if self.generic_enums.contains_key(name) {
                    let mono_name = self.ensure_monomorphized_enum(name, generics)?;
                    let enum_info = self.enum_types.get(&mono_name).cloned()
                        .ok_or_else(|| CodegenError::UndefinedType(format!("enum '{}' not found after monomorphization", mono_name)))?;
                    return self.compile_enum_variant_constructor(&enum_info, &mono_name, field, &[]);
                }
                // For generic structs like Vec<u8>, this will be handled by method call logic
            }
        }

        // Continue with struct field access for identifiers
        if let Expr::Ident(name, _) = object {
            // Check if this is a generic enum - requires explicit type arguments for unit variants
            if self.generic_enums.contains_key(name) {
                return Err(CodegenError::NotImplemented(
                    format!("generic enum unit variant '{}' requires explicit type arguments, e.g., {}<Type>.{}",
                            field, name, field)
                ));
            }

            // Check for use-after-move
            if self.moved_vars.contains(name) {
                return Err(CodegenError::BorrowError(
                    format!("use of moved value: '{}'", name)
                ));
            }

            // Otherwise it's a struct or tuple field access
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            // Check for tuple field access: tuple.0, tuple.1, etc.
            if let Ok(field_idx) = field.parse::<u32>() {
                if let Some(Type::Tuple(types)) = &var_info.ast_type {
                    if (field_idx as usize) < types.len() {
                        // Get the tuple type
                        let field_types: Vec<_> = types.iter()
                            .map(|t| self.llvm_type(t))
                            .collect::<Result<_, _>>()?;
                        let tuple_type = self.context.struct_type(&field_types, false);
                        let field_type = field_types[field_idx as usize];

                        // GEP to get the field
                        let field_ptr = self.builder
                            .build_struct_gep(tuple_type, var_info.ptr, field_idx, &format!("{}.{}", name, field))
                            .unwrap();

                        let field_val = self.builder
                            .build_load(field_type, field_ptr, field)
                            .unwrap();

                        return Ok(field_val);
                    } else {
                        return Err(CodegenError::UndefinedField(
                            format!("tuple index {} out of bounds (tuple has {} elements)", field_idx, types.len())
                        ));
                    }
                }
            }

            // Determine struct name - either direct or through reference
            let struct_name = if var_info.is_ref {
                var_info.ref_struct_name.as_ref()
            } else {
                var_info.struct_name.as_ref()
            };

            let struct_name = struct_name
                .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", name)))?;

            // Ensure the struct is monomorphized if it's a generic type
            // This handles cases like Slice_u8 where we know the name but haven't materialized it yet
            if !self.struct_types.contains_key(struct_name) {
                // Try to extract base name and type args from mangled name (e.g., "Slice_u8" -> "Slice", [u8])
                if let Some((base_name, type_args)) = self.parse_mangled_name(struct_name) {
                    if self.generic_structs.contains_key(&base_name) {
                        self.ensure_monomorphized_struct(&base_name, &type_args)?;
                    }
                }
            }

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
        } else if let Expr::Field { object: inner_obj, field: inner_field, .. } = object {
            // Nested field access: a.b.c
            // Get a pointer to the inner field (a.b), then access .c on that
            let (inner_ptr, inner_struct_name) = self.compile_nested_field_ptr(inner_obj, inner_field)?;

            // Now access the outer field on the inner struct
            let struct_info = self
                .struct_types
                .get(&inner_struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(inner_struct_name.clone()))?
                .clone();

            let field_idx = *struct_info
                .field_indices
                .get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            let field_type = struct_info.field_types[field_idx as usize];

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, inner_ptr, field_idx, &format!("nested.{}", field))
                .unwrap();

            let field_val = self
                .builder
                .build_load(field_type, field_ptr, field)
                .unwrap();

            Ok(field_val)
        } else {
            Err(CodegenError::NotImplemented(
                "field access on complex expressions not yet supported".to_string()
            ))
        }
    }

    /// Get a pointer to a nested field and return both the pointer and the struct type name
    fn compile_nested_field_ptr(
        &mut self,
        object: &Expr,
        field: &str,
    ) -> Result<(PointerValue<'ctx>, String), CodegenError> {
        if let Expr::Ident(name, _) = object {
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            let struct_name = if var_info.is_ref {
                var_info.ref_struct_name.as_ref()
            } else {
                var_info.struct_name.as_ref()
            };

            let struct_name = struct_name
                .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", name)))?
                .clone();

            // Ensure struct is monomorphized
            if !self.struct_types.contains_key(&struct_name) {
                if let Some((base_name, type_args)) = self.parse_mangled_name(&struct_name) {
                    if self.generic_structs.contains_key(&base_name) {
                        self.ensure_monomorphized_struct(&base_name, &type_args)?;
                    }
                }
            }

            let struct_info = self
                .struct_types
                .get(&struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                .clone();

            let field_idx = *struct_info
                .field_indices
                .get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            // Get the AST type of this field to determine its struct name
            let field_ast_type = struct_info.ast_field_types.get(field_idx as usize)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?
                .clone();

            let field_struct_name = self.get_struct_name_for_type(&field_ast_type)
                .ok_or_else(|| CodegenError::NotImplemented(
                    format!("field '{}' is not a struct type", field)
                ))?;

            // Get pointer to the struct
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

            Ok((field_ptr, field_struct_name))
        } else if let Expr::Field { object: inner_obj, field: inner_field, .. } = object {
            // Recursive case: a.b.c.d
            let (inner_ptr, inner_struct_name) = self.compile_nested_field_ptr(inner_obj, inner_field)?;

            let struct_info = self
                .struct_types
                .get(&inner_struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(inner_struct_name.clone()))?
                .clone();

            let field_idx = *struct_info
                .field_indices
                .get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            let field_ast_type = struct_info.ast_field_types.get(field_idx as usize)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?
                .clone();

            let field_struct_name = self.get_struct_name_for_type(&field_ast_type)
                .ok_or_else(|| CodegenError::NotImplemented(
                    format!("field '{}' is not a struct type", field)
                ))?;

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, inner_ptr, field_idx, &format!("nested.{}", field))
                .unwrap();

            Ok((field_ptr, field_struct_name))
        } else {
            Err(CodegenError::NotImplemented(
                "complex nested field access not yet supported".to_string()
            ))
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
            Err(CodegenError::NotImplemented(
                "nested field assignment (e.g., 'a.b.c = x') is not yet supported. \
                 Use an intermediate variable or reference".to_string()
            ))
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
                    Err(CodegenError::NotImplemented(
                        "nested field reference (e.g., '&a.b.c') is not yet supported. \
                         Use an intermediate variable".to_string()
                    ))
                }
            }
            _ => Err(CodegenError::NotImplemented(
                "can only take reference (&/~) of variables and struct fields, not expressions".to_string()
            )),
        }
    }

    pub(crate) fn compile_deref(&mut self, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // *expr - dereference a pointer
        let ptr_val = self.compile_expr(operand)?;
        let ptr = ptr_val.into_pointer_value();

        // Try to infer the pointed-to type from context
        // For now, support common cases: pointer to i8/i32/i64 and generic type T
        // Default to i64 as it's the common case for Array<i64> etc.
        // TODO: Add proper type inference based on pointer type annotations
        let val = self.builder.build_load(self.context.i64_type(), ptr, "deref").unwrap();
        Ok(val)
    }

    /// Compile assignment through pointer: *ptr = value
    pub(crate) fn compile_deref_assign(&mut self, target: &Expr, value: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ptr_val = self.compile_expr(target)?;
        let ptr = ptr_val.into_pointer_value();
        self.builder.build_store(ptr, value).unwrap();
        Ok(value)
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

    /// Compile array repeat syntax: [val; count]
    pub(crate) fn compile_array_repeat(&mut self, value: &Expr, count: usize, elem_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if count == 0 {
            return Err(CodegenError::InvalidArguments("array repeat count cannot be 0".to_string()));
        }

        // Compile the value with expected element type for coercion
        let val = self.compile_expr_with_type(value, elem_type)?;

        // Determine element type
        let llvm_elem_type = val.get_type();
        let array_type = llvm_elem_type.array_type(count as u32);

        // Create alloca for the array
        let array_alloca = self.create_entry_block_alloca("array_repeat", array_type.into());

        // Store the value into each element
        for i in 0..count {
            let idx = self.context.i32_type().const_int(i as u64, false);
            let elem_ptr = unsafe {
                self.builder.build_gep(
                    array_type,
                    array_alloca,
                    &[self.context.i32_type().const_zero(), idx],
                    &format!("elem{}", i)
                ).unwrap()
            };
            self.builder.build_store(elem_ptr, val).unwrap();
        }

        // Load and return the array value
        let array_val = self.builder.build_load(array_type, array_alloca, "array_repeat").unwrap();
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

                // Extract element type and array size from AST type
                // For &T[N], we need to unwrap the Ref to get Array(T, N)
                let (elem_llvm_type, array_size): (BasicTypeEnum, u32) = if let Some(ref ast_type) = var_info.ast_type {
                    match ast_type {
                        Type::Ref(inner) | Type::RefMut(inner) => {
                            if let Type::Array(elem_type, size) = inner.as_ref() {
                                (self.llvm_type(elem_type)?, *size as u32)
                            } else {
                                // Fallback for non-array references
                                (self.context.i64_type().into(), 0)
                            }
                        }
                        Type::Array(elem_type, size) => {
                            // Direct array type (shouldn't happen with is_ref, but handle it)
                            (self.llvm_type(elem_type)?, *size as u32)
                        }
                        _ => (self.context.i64_type().into(), 0)
                    }
                } else {
                    // Fallback if no AST type available
                    (self.context.i64_type().into(), 0)
                };

                // Build the array type for proper GEP
                let array_type = elem_llvm_type.array_type(array_size);

                // GEP into the array with [0, idx] to go through pointer then index
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        array_type,
                        array_ptr,
                        &[self.context.i32_type().const_zero(), idx_val],
                        "elem_ptr"
                    ).unwrap()
                };

                let val = self.builder.build_load(elem_llvm_type, elem_ptr, "elem").unwrap();
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
            Err(CodegenError::NotImplemented(
                "can only index array/slice variables directly (e.g., 'arr[i]'). \
                 Indexing expressions like 'get_array()[i]' requires an intermediate variable".to_string()
            ))
        }
    }

    /// Get a pointer to an array element for assignment (arr[i] = val)
    /// Returns (pointer, element_type) so we can properly coerce the value before storing
    pub(crate) fn compile_index_ptr(&mut self, array: &Expr, index: &Expr) -> Result<(PointerValue<'ctx>, BasicTypeEnum<'ctx>), CodegenError> {
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

                    // GEP to get element pointer at index
                    let elem_ptr = unsafe {
                        self.builder.build_gep(
                            elem_type,
                            data_ptr,
                            &[idx_val],
                            "slice_elem_ptr"
                        ).unwrap()
                    };

                    return Ok((elem_ptr, elem_type));
                }
            }

            if var_info.is_ref {
                // Indexing through a reference - load the pointer, then index
                let array_ptr = self.builder.build_load(
                    self.context.ptr_type(inkwell::AddressSpace::default()),
                    var_info.ptr,
                    "array_ptr"
                ).unwrap().into_pointer_value();

                // Extract element type and array size from AST type
                let (elem_llvm_type, array_size): (BasicTypeEnum, u32) = if let Some(ref ast_type) = var_info.ast_type {
                    match ast_type {
                        Type::Ref(inner) | Type::RefMut(inner) => {
                            if let Type::Array(elem_type, size) = inner.as_ref() {
                                (self.llvm_type(elem_type)?, *size as u32)
                            } else {
                                (self.context.i64_type().into(), 0)
                            }
                        }
                        Type::Array(elem_type, size) => {
                            (self.llvm_type(elem_type)?, *size as u32)
                        }
                        _ => (self.context.i64_type().into(), 0)
                    }
                } else {
                    (self.context.i64_type().into(), 0)
                };

                let array_type = elem_llvm_type.array_type(array_size);

                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        array_type,
                        array_ptr,
                        &[self.context.i32_type().const_zero(), idx_val],
                        "elem_ptr"
                    ).unwrap()
                };

                Ok((elem_ptr, elem_llvm_type))
            } else {
                // Direct array variable - GEP to get element pointer
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

                Ok((elem_ptr, elem_type))
            }
        } else {
            Err(CodegenError::NotImplemented(
                "can only assign to array/slice variables directly (e.g., 'arr[i] = val')".to_string()
            ))
        }
    }
}
