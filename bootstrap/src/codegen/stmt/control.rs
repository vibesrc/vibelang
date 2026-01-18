//! Control flow statements: if and while

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_if(
        &mut self,
        condition: &Expr,
        then_block: &Block,
        else_block: Option<&Block>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let cond_value = self.compile_expr(condition)?;
        let cond_int = cond_value.into_int_value();

        // Ensure condition is i1 for branching
        // If it's not i1 (e.g., i64 from ptr_is_null), compare with 0
        let cond_bool = if cond_int.get_type().get_bit_width() != 1 {
            self.builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond_int,
                    cond_int.get_type().const_zero(),
                    "cond_bool",
                )
                .unwrap()
        } else {
            cond_int
        };

        let fn_value = self.current_function.unwrap();
        let then_bb = self.context.append_basic_block(fn_value, "then");
        let else_bb = self.context.append_basic_block(fn_value, "else");
        let merge_bb = self.context.append_basic_block(fn_value, "merge");

        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .unwrap();

        // Then block
        self.builder.position_at_end(then_bb);
        self.compile_block(then_block)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Else block
        self.builder.position_at_end(else_bb);
        if let Some(else_blk) = else_block {
            self.compile_block(else_blk)?;
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Merge block
        self.builder.position_at_end(merge_bb);

        Ok(None)
    }

    pub(crate) fn compile_while(
        &mut self,
        condition: &Expr,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let cond_bb = self.context.append_basic_block(fn_value, "while.cond");
        let body_bb = self.context.append_basic_block(fn_value, "while.body");
        let end_bb = self.context.append_basic_block(fn_value, "while.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(cond_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition block
        self.builder.position_at_end(cond_bb);
        let cond_value = self.compile_expr(condition)?;
        let cond_int = cond_value.into_int_value();

        // Ensure condition is i1 for branching
        let cond_bool = if cond_int.get_type().get_bit_width() != 1 {
            self.builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond_int,
                    cond_int.get_type().const_zero(),
                    "cond_bool",
                )
                .unwrap()
        } else {
            cond_int
        };

        self.builder
            .build_conditional_branch(cond_bool, body_bb, end_bb)
            .unwrap();

        // Body block
        self.builder.position_at_end(body_bb);
        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(cond_bb).unwrap();
        }

        // End block
        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }
}
