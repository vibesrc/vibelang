//! Type-related code generation utilities

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::types::{BasicTypeEnum, BasicType};
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn llvm_type(&self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
        match ty {
            Type::I8 => Ok(self.context.i8_type().into()),
            Type::I16 => Ok(self.context.i16_type().into()),
            Type::I32 => Ok(self.context.i32_type().into()),
            Type::I64 => Ok(self.context.i64_type().into()),
            Type::U8 => Ok(self.context.i8_type().into()),
            Type::U16 => Ok(self.context.i16_type().into()),
            Type::U32 => Ok(self.context.i32_type().into()),
            Type::U64 => Ok(self.context.i64_type().into()),
            Type::F32 => Ok(self.context.f32_type().into()),
            Type::F64 => Ok(self.context.f64_type().into()),
            Type::Bool => Ok(self.context.bool_type().into()),
            Type::Pointer(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Ref(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::RefMut(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Array(inner, size) => {
                let inner_ty = self.llvm_type(inner)?;
                Ok(inner_ty.array_type(*size as u32).into())
            }
            Type::Named { name, generics } => {
                // Generate mangled name if generics present
                let lookup_name = if generics.is_empty() {
                    name.clone()
                } else {
                    self.mangle_name(name, generics)
                };

                // Check if it's an enum type
                if let Some(enum_info) = self.enum_types.get(&lookup_name) {
                    return Ok(enum_info.llvm_type.as_basic_type_enum());
                }
                // Look up the struct type
                let struct_info = self
                    .struct_types
                    .get(&lookup_name)
                    .ok_or_else(|| CodegenError::UndefinedType(lookup_name.clone()))?;
                Ok(struct_info.llvm_type.as_basic_type_enum())
            }
            Type::Slice(_inner) => {
                // Slice is a struct: { ptr: *T, len: i64 }
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let len_type = self.context.i64_type();
                Ok(self
                    .context
                    .struct_type(&[ptr_type.into(), len_type.into()], false)
                    .into())
            }
            _ => Err(CodegenError::NotImplemented(format!("type {:?}", ty))),
        }
    }

    /// Generate a mangled name for monomorphized types/functions
    /// e.g., "Pair" + [i32] => "Pair_i32"
    pub(crate) fn mangle_name(&self, name: &str, type_args: &[Type]) -> String {
        if type_args.is_empty() {
            return name.to_string();
        }

        let type_suffixes: Vec<String> = type_args
            .iter()
            .map(|ty| self.type_name(ty))
            .collect();

        format!("{}_{}", name, type_suffixes.join("_"))
    }

    /// Get a string representation of a type for mangling
    pub(crate) fn type_name(&self, ty: &Type) -> String {
        match ty {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Pointer(inner) => format!("ptr_{}", self.type_name(inner)),
            Type::Ref(inner) => format!("ref_{}", self.type_name(inner)),
            Type::RefMut(inner) => format!("mut_{}", self.type_name(inner)),
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    name.clone()
                } else {
                    self.mangle_name(name, generics)
                }
            }
            Type::Slice(inner) => format!("slice_{}", self.type_name(inner)),
            _ => "unknown".to_string(),
        }
    }

    /// Convert an LLVM type back to an AST type (for primitive types only)
    pub(crate) fn llvm_type_to_ast_type(&self, ty: BasicTypeEnum<'ctx>) -> Option<Type> {
        if ty.is_int_type() {
            let int_ty = ty.into_int_type();
            let bit_width = int_ty.get_bit_width();
            match bit_width {
                1 => Some(Type::Bool),
                8 => Some(Type::I8),   // Could be U8, but we default to signed
                16 => Some(Type::I16),
                32 => Some(Type::I32),
                64 => Some(Type::I64),
                _ => None,
            }
        } else if ty.is_float_type() {
            let float_ty = ty.into_float_type();
            // Check if it's f32 or f64 based on type
            if float_ty == self.context.f32_type() {
                Some(Type::F32)
            } else if float_ty == self.context.f64_type() {
                Some(Type::F64)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(crate) fn get_struct_name_for_type(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Named { name, generics } => {
                // Check for monomorphized name first (e.g., Pair_i32)
                if !generics.is_empty() {
                    let mangled = self.mangle_name(name, generics);
                    if self.struct_types.contains_key(&mangled) {
                        return Some(mangled);
                    }
                }
                // Fall back to base name
                if self.struct_types.contains_key(name) {
                    Some(name.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(crate) fn get_ref_info(&self, ty: &Type) -> (bool, bool, Option<String>) {
        // Returns (is_ref, is_mut_ref, ref_struct_name)
        match ty {
            Type::Ref(inner) => {
                let struct_name = self.get_struct_name_for_type(inner);
                (true, false, struct_name)
            }
            Type::RefMut(inner) => {
                let struct_name = self.get_struct_name_for_type(inner);
                (true, true, struct_name)
            }
            _ => (false, false, None),
        }
    }

    /// Get the type name from a Type for method lookup
    pub(crate) fn get_type_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    Some(name.clone())
                } else {
                    Some(self.mangle_name(name, generics))
                }
            }
            _ => None,
        }
    }
}
