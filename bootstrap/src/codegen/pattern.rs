//! Pattern matching compilation

use super::{Codegen, CodegenError, VarInfo};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_match(
        &mut self,
        value: &Expr,
        arms: &[MatchArm],
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let match_val = self.compile_expr(value)?;

        // Get enum info - first try from the pattern, then try to find monomorphized version
        let enum_info = if let Some(arm) = arms.first() {
            if let Pattern::Enum { path, .. } = &arm.pattern {
                if path.len() >= 1 {
                    let base_name = &path[0];
                    // First try direct lookup
                    if let Some(info) = self.enum_types.get(base_name).cloned() {
                        Some(info)
                    } else {
                        // Try to find a monomorphized version by checking the LLVM type name
                        // The match value's struct type name will be the monomorphized enum name
                        let _llvm_type_name = match_val.get_type().print_to_string().to_string();
                        // Look for any enum type whose name starts with the pattern name
                        self.enum_types.iter()
                            .find(|(name, _)| name.starts_with(base_name))
                            .map(|(_, info)| info.clone())
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        // Store value and extract tag from the matched value
        let (match_int, match_ptr) = if let Some(ref info) = enum_info {
            // Store value to get a pointer for GEP (used for both tag and payload extraction)
            let alloca = self.builder.build_alloca(info.llvm_type, "match_val").unwrap();
            self.builder.build_store(alloca, match_val).unwrap();

            // Extract tag (field 0)
            let tag_ptr = self.builder
                .build_struct_gep(info.llvm_type, alloca, 0, "tag_ptr")
                .unwrap();
            let tag = self.builder.build_load(self.context.i32_type(), tag_ptr, "tag").unwrap().into_int_value();
            (tag, Some(alloca))
        } else {
            // Fallback to direct int for simple enums
            (match_val.into_int_value(), None)
        };

        let fn_value = self.current_function.unwrap();
        let merge_bb = self.context.append_basic_block(fn_value, "match.merge");

        // Collect arm results for phi node
        let mut incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> = Vec::new();

        // Create default block (unreachable for exhaustive match)
        let default_bb = self.context.append_basic_block(fn_value, "match.default");

        // Build switch with cases - use the resolved enum_info
        let mut cases: Vec<(inkwell::values::IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> = Vec::new();
        let mut arm_info: Vec<(inkwell::basic_block::BasicBlock<'ctx>, Option<(String, Vec<Pattern>)>)> = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let arm_bb = self.context.append_basic_block(fn_value, &format!("match.arm{}", i));

            // Get the tag value for this pattern
            if let Pattern::Enum { path, fields } = &arm.pattern {
                if path.len() == 2 {
                    let variant_name = &path[1];

                    // Use the already-resolved enum_info instead of looking up by pattern name
                    if let Some(ref info) = enum_info {
                        if let Some(&tag) = info.variant_tags.get(variant_name) {
                            let tag_val = self.context.i32_type().const_int(tag as u64, false);
                            cases.push((tag_val, arm_bb));
                            arm_info.push((arm_bb, Some((variant_name.clone(), fields.clone()))));
                        }
                    }
                }
            }
        }

        // Build switch instruction
        self.builder.build_switch(match_int, default_bb, &cases).unwrap();

        // Compile each arm body
        for (i, arm) in arms.iter().enumerate() {
            if let Some((bb, binding_info)) = arm_info.get(i) {
                self.builder.position_at_end(*bb);

                // Extract payload bindings if any - use the resolved enum_info
                if let (Some(ptr), Some((variant_name, fields))) = (match_ptr, binding_info) {
                    if let Some(ref info) = enum_info {
                        if let Some(payload_types) = info.variant_payloads.get(variant_name) {
                            for (j, pattern) in fields.iter().enumerate() {
                                if let Pattern::Ident(var_name) = pattern {
                                    if j < payload_types.len() {
                                        let payload_type = payload_types[j];
                                        // Extract payload at index j+1 (0 is tag)
                                        let payload_ptr = self.builder
                                            .build_struct_gep(info.llvm_type, ptr, (j + 1) as u32, &format!("payload{}", j))
                                            .unwrap();
                                        let payload_val = self.builder
                                            .build_load(payload_type, payload_ptr, var_name)
                                            .unwrap();

                                        // Bind to variable
                                        let var_alloca = self.create_entry_block_alloca(var_name, payload_type);
                                        self.builder.build_store(var_alloca, payload_val).unwrap();
                                        self.variables.insert(var_name.clone(), VarInfo {
                                            ptr: var_alloca,
                                            ty: payload_type,
                                            struct_name: None,
                                            is_ref: false,
                                            is_mut_ref: false,
                                            ref_struct_name: None,
                                            slice_elem_type: None,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                let arm_val = self.compile_expr(&arm.body)?;

                // Get the current block (may have changed during compile_expr)
                let current_bb = self.builder.get_insert_block().unwrap();
                if current_bb.get_terminator().is_none() {
                    incoming.push((arm_val, current_bb));
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
            }
        }

        // Default block - unreachable
        self.builder.position_at_end(default_bb);
        self.builder.build_unreachable().unwrap();

        // Merge block with phi
        self.builder.position_at_end(merge_bb);

        if !incoming.is_empty() {
            let phi_type = incoming[0].0.get_type();
            let phi = self.builder.build_phi(phi_type, "match.result").unwrap();
            for (val, bb) in &incoming {
                phi.add_incoming(&[(val, *bb)]);
            }
            return Ok(Some(phi.as_basic_value()));
        }

        Ok(None)
    }
}
