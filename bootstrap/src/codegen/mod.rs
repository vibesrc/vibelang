//! Vibelang code generation - LLVM IR emission via inkwell

mod call;
mod define;
mod expr;
mod intrinsics;
mod memory;
mod module;
mod monomorph;
mod pattern;
mod project;
mod stmt;
mod types;

pub use project::{ProjectConfig, ProjectContext};

use crate::ast::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::types::BasicTypeEnum;
use inkwell::AddressSpace;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// Tracks the borrow state of a variable
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum BorrowState {
    Shared,    // Has one or more & borrows
    Mutable,   // Has a single ~ borrow
}

pub struct Codegen<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) variables: HashMap<String, VarInfo<'ctx>>,
    pub(crate) current_function: Option<FunctionValue<'ctx>>,
    pub(crate) current_function_return_type: Option<Type>,
    pub(crate) struct_types: HashMap<String, StructTypeInfo<'ctx>>,
    pub(crate) enum_types: HashMap<String, EnumTypeInfo<'ctx>>,
    // Ownership tracking
    pub(crate) moved_vars: HashSet<String>,              // Variables that have been moved
    pub(crate) borrowed_vars: HashMap<String, BorrowState>, // Active borrows on variables
    // Generic definitions (stored for monomorphization)
    pub(crate) generic_structs: HashMap<String, Struct>,
    pub(crate) generic_enums: HashMap<String, Enum>,
    pub(crate) generic_functions: HashMap<String, Function>,
    pub(crate) generic_impls: HashMap<String, Impl>, // Generic impl blocks (type_name -> impl)
    // Loop control flow targets (for break/continue)
    pub(crate) loop_break_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) loop_continue_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    // Deferred expressions (LIFO order)
    pub(crate) deferred_exprs: Vec<Expr>,
    // Methods for types (type_name -> (method_name -> mangled_function_name))
    pub(crate) type_methods: HashMap<String, HashMap<String, String>>,
    // Module system
    pub(crate) current_module_path: Vec<String>,            // Current module path (e.g., ["compiler", "lexer"])
    pub(crate) source_dir: Option<PathBuf>,                 // Directory of the current source file
    pub(crate) current_file_path: Option<PathBuf>,          // Path to the current source file being compiled
    pub(crate) loaded_modules: HashSet<String>,             // Modules already loaded (prevent duplicates)
    pub(crate) imports: HashMap<String, String>,            // Name aliases from 'use' (short_name -> qualified_name)
    pub(crate) module_aliases: HashMap<String, String>,    // Module namespace aliases (e.g., "fs" -> "fs" for use std.fs)
    pub(crate) module_public_items: HashMap<String, HashSet<String>>, // Public items per module for visibility checking
    // Project context
    pub(crate) project: ProjectContext,                     // Project configuration and paths
    // Function return types (for inferring struct types from function calls)
    pub(crate) function_return_types: HashMap<String, Option<Type>>,
    // Function parameter types (for argument coercion)
    pub(crate) function_param_types: HashMap<String, Vec<Type>>,
    // Unsafe context tracking
    pub(crate) in_unsafe: bool,                             // Currently inside an unsafe block
    // Static variables (global constants)
    pub(crate) static_vars: HashMap<String, PointerValue<'ctx>>,
}

#[derive(Clone)]
pub(crate) struct VarInfo<'ctx> {
    pub(crate) ptr: PointerValue<'ctx>,       // Pointer to the variable storage
    pub(crate) ty: BasicTypeEnum<'ctx>,       // LLVM type
    pub(crate) struct_name: Option<String>,   // If it's a struct, the struct name (may be mangled)
    pub(crate) ast_type: Option<Type>,        // Full AST type with generics (for type inference)
    pub(crate) is_ref: bool,                  // Is this a reference (&T or ~T)?
    pub(crate) is_mut_ref: bool,              // Is this a mutable reference (~T)?
    pub(crate) ref_struct_name: Option<String>, // If it's a ref to a struct, the struct name
    pub(crate) slice_elem_type: Option<Type>, // If it's a slice, the element type (e.g., I32 for &[i32])
}

#[derive(Clone)]
pub(crate) struct StructTypeInfo<'ctx> {
    pub(crate) llvm_type: inkwell::types::StructType<'ctx>,
    pub(crate) field_indices: HashMap<String, u32>,
    pub(crate) field_types: Vec<BasicTypeEnum<'ctx>>,
    pub(crate) field_names: Vec<String>, // Field names in order (for ToString)
    pub(crate) ast_field_types: Vec<Type>, // AST types for recursive ToString
    pub(crate) name: String, // Struct name (for ToString)
}

#[derive(Clone)]
pub(crate) struct EnumTypeInfo<'ctx> {
    pub(crate) llvm_type: inkwell::types::StructType<'ctx>, // { i32 tag, payload }
    pub(crate) variant_tags: HashMap<String, u32>,           // variant name -> tag value
    pub(crate) variant_payloads: HashMap<String, Vec<BasicTypeEnum<'ctx>>>, // variant name -> payload types
    pub(crate) ast_variant_payloads: HashMap<String, Vec<Type>>, // variant name -> AST payload types
    pub(crate) payload_size: u32, // size of largest payload in bytes
    pub(crate) variant_names: Vec<String>, // Variant names in tag order (for ToString)
    pub(crate) name: String, // Enum name (for ToString)
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let mut codegen = Codegen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            current_function: None,
            current_function_return_type: None,
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            moved_vars: HashSet::new(),
            borrowed_vars: HashMap::new(),
            generic_structs: HashMap::new(),
            generic_enums: HashMap::new(),
            generic_functions: HashMap::new(),
            generic_impls: HashMap::new(),
            loop_break_block: None,
            loop_continue_block: None,
            deferred_exprs: Vec::new(),
            type_methods: HashMap::new(),
            current_module_path: Vec::new(),
            source_dir: None,
            current_file_path: None,
            loaded_modules: HashSet::new(),
            imports: HashMap::new(),
            module_aliases: HashMap::new(),
            module_public_items: HashMap::new(),
            project: ProjectContext::discover(std::path::Path::new(".")),
            function_return_types: HashMap::new(),
            function_param_types: HashMap::new(),
            in_unsafe: false,
            static_vars: HashMap::new(),
        };

        // Declare intrinsics
        codegen.declare_intrinsics();

        codegen
    }

    /// Create a new Codegen with a specific source file path for project detection
    pub fn new_with_source(context: &'ctx Context, module_name: &str, source_path: &std::path::Path) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        // Discover project from source file location
        let project = ProjectContext::discover(source_path);
        let source_dir = source_path.parent().map(|p| p.to_path_buf());

        let mut codegen = Codegen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            current_function: None,
            current_function_return_type: None,
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            moved_vars: HashSet::new(),
            borrowed_vars: HashMap::new(),
            generic_structs: HashMap::new(),
            generic_enums: HashMap::new(),
            generic_functions: HashMap::new(),
            generic_impls: HashMap::new(),
            loop_break_block: None,
            loop_continue_block: None,
            deferred_exprs: Vec::new(),
            type_methods: HashMap::new(),
            current_module_path: Vec::new(),
            source_dir,
            current_file_path: Some(source_path.to_path_buf()),
            loaded_modules: HashSet::new(),
            imports: HashMap::new(),
            module_aliases: HashMap::new(),
            module_public_items: HashMap::new(),
            project,
            function_return_types: HashMap::new(),
            function_param_types: HashMap::new(),
            in_unsafe: false,
            static_vars: HashMap::new(),
        };

        // Declare intrinsics
        codegen.declare_intrinsics();

        codegen
    }

    fn declare_intrinsics(&mut self) {
        // Declare puts from libc for print functionality
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let puts_type = i32_type.fn_type(&[ptr_type.into()], false);
        self.module.add_function("puts", puts_type, None);

        // Declare printf for formatted output
        let printf_type = i32_type.fn_type(&[ptr_type.into()], true); // variadic
        self.module.add_function("printf", printf_type, None);

        // Declare malloc for heap allocation
        let malloc_type = ptr_type.fn_type(&[i64_type.into()], false);
        self.module.add_function("malloc", malloc_type, None);

        // Declare realloc for resizing allocations
        let realloc_type = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        self.module.add_function("realloc", realloc_type, None);

        // Declare free for releasing memory
        let free_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
        self.module.add_function("free", free_type, None);

        // Declare memcpy for copying memory
        let memcpy_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into(), i64_type.into()], false);
        self.module.add_function("memcpy", memcpy_type, None);

        // Declare exit for panic
        let exit_type = self.context.void_type().fn_type(&[i32_type.into()], false);
        self.module.add_function("exit", exit_type, None);

        // Declare snprintf for number-to-string conversion
        // int snprintf(char *str, size_t size, const char *format, ...);
        let snprintf_type = i32_type.fn_type(&[ptr_type.into(), i64_type.into(), ptr_type.into()], true);
        self.module.add_function("snprintf", snprintf_type, None);

        // Declare strlen for string length
        let strlen_type = i64_type.fn_type(&[ptr_type.into()], false);
        self.module.add_function("strlen", strlen_type, None);

        // File I/O syscalls
        // int open(const char *pathname, int flags, mode_t mode)
        let open_type = i32_type.fn_type(&[ptr_type.into(), i32_type.into(), i32_type.into()], false);
        self.module.add_function("open", open_type, None);

        // int close(int fd)
        let close_type = i32_type.fn_type(&[i32_type.into()], false);
        self.module.add_function("close", close_type, None);

        // ssize_t read(int fd, void *buf, size_t count)
        let read_type = i64_type.fn_type(&[i32_type.into(), ptr_type.into(), i64_type.into()], false);
        self.module.add_function("read", read_type, None);

        // ssize_t write(int fd, const void *buf, size_t count)
        let write_type = i64_type.fn_type(&[i32_type.into(), ptr_type.into(), i64_type.into()], false);
        self.module.add_function("write", write_type, None);

        // off_t lseek(int fd, off_t offset, int whence)
        let lseek_type = i64_type.fn_type(&[i32_type.into(), i64_type.into(), i32_type.into()], false);
        self.module.add_function("lseek", lseek_type, None);

        // Time syscalls
        // int clock_gettime(clockid_t clockid, struct timespec *tp)
        let clock_gettime_type = i32_type.fn_type(&[i32_type.into(), ptr_type.into()], false);
        self.module.add_function("clock_gettime", clock_gettime_type, None);

        // int nanosleep(const struct timespec *req, struct timespec *rem)
        let nanosleep_type = i32_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        self.module.add_function("nanosleep", nanosleep_type, None);
    }

    /// Set the source directory for module resolution
    pub fn set_source_dir(&mut self, dir: PathBuf) {
        self.source_dir = Some(dir);
    }

    /// Load prelude definitions (Option, Result, etc.) if available
    /// These are auto-imported into all modules without explicit 'use'
    fn load_prelude(&mut self) -> Result<(), CodegenError> {
        // Only load prelude once
        if self.loaded_modules.contains("__prelude__") {
            return Ok(());
        }
        self.loaded_modules.insert("__prelude__".to_string());

        // Clone stdlib path to avoid borrow issues
        let stdlib_path = match &self.project.stdlib_path {
            Some(p) => p.clone(),
            None => return Ok(()),
        };

        // Load types from std/src/types/mod.vibe (Option, Result, Error, Slice)
        let types_mod = stdlib_path.join("src").join("types").join("mod.vibe");
        if types_mod.exists() {
            self.load_prelude_file(&types_mod)?;
        }

        // Note: Vec and String require explicit import due to dependencies on std.mem
        // A proper prelude system would need to resolve module dependencies

        Ok(())
    }

    /// Load a prelude file and extract definitions
    fn load_prelude_file(&mut self, path: &PathBuf) -> Result<(), CodegenError> {
        let path_str = path.to_string_lossy().to_string();

        // Check if already loaded
        if self.loaded_modules.contains(&path_str) {
            return Ok(());
        }
        self.loaded_modules.insert(path_str.clone());

        // Read and parse the prelude file
        let source = std::fs::read_to_string(path)
            .map_err(|e| CodegenError::NotImplemented(format!("failed to read prelude '{}': {}", path.display(), e)))?;

        let prelude_program = crate::parser::Parser::parse(&source)
            .map_err(|e| CodegenError::NotImplemented(format!("failed to parse prelude '{}': {}", path.display(), e)))?;

        // Track public items from this module (needed for later 'use' statements)
        let mut public_items = std::collections::HashSet::new();
        for item in &prelude_program.items {
            match item {
                Item::Function(f) if f.is_pub => { public_items.insert(f.name.clone()); }
                Item::Struct(s) if s.is_pub => { public_items.insert(s.name.clone()); }
                Item::Enum(e) if e.is_pub => { public_items.insert(e.name.clone()); }
                Item::Static(s) if s.is_pub => { public_items.insert(s.name.clone()); }
                _ => {}
            }
        }
        self.module_public_items.insert(path_str, public_items);

        // Process items from prelude - first pass: define types
        for item in &prelude_program.items {
            match item {
                Item::Struct(s) if s.is_pub => {
                    if s.generics.is_empty() {
                        self.define_struct(s)?;
                    } else {
                        self.generic_structs.insert(s.name.clone(), s.clone());
                    }
                }
                Item::Enum(e) if e.is_pub => {
                    if e.generics.is_empty() {
                        self.define_enum(e)?;
                    } else {
                        self.generic_enums.insert(e.name.clone(), e.clone());
                    }
                }
                _ => {}
            }
        }

        // Second pass: declare functions and impl methods
        for item in &prelude_program.items {
            match item {
                Item::Function(func) if func.is_pub => {
                    if func.generics.is_empty() {
                        self.declare_function(func)?;
                    } else {
                        self.generic_functions.insert(func.name.clone(), func.clone());
                    }
                }
                Item::Impl(impl_block) => {
                    self.declare_impl_methods(impl_block)?;
                }
                _ => {}
            }
        }

        // Third pass: compile function bodies
        for item in &prelude_program.items {
            match item {
                Item::Function(func) if func.is_pub && func.generics.is_empty() => {
                    self.compile_function(func)?;
                }
                Item::Impl(impl_block) => {
                    self.compile_impl_methods(impl_block)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), CodegenError> {
        // Load prelude first (Option, Result, etc.)
        self.load_prelude()?;

        // Pass 0: Process use declarations first
        for item in &program.items {
            if let Item::Use(u) = item {
                self.process_use(u)?;
            }
        }

        // First pass: store generic definitions, define concrete struct/enum types
        for item in &program.items {
            match item {
                Item::Struct(s) => {
                    if s.generics.is_empty() {
                        self.define_struct(s)?;
                    } else {
                        // Store generic struct for later monomorphization
                        self.generic_structs.insert(s.name.clone(), s.clone());
                    }
                }
                Item::Enum(e) => {
                    if e.generics.is_empty() {
                        self.define_enum(e)?;
                    } else {
                        // Store generic enum for later monomorphization
                        self.generic_enums.insert(e.name.clone(), e.clone());
                    }
                }
                Item::Function(func) => {
                    if !func.generics.is_empty() {
                        // Store generic function for later monomorphization
                        self.generic_functions.insert(func.name.clone(), func.clone());
                    }
                }
                Item::Static(s) => {
                    // Compile static variable as global constant
                    self.compile_static(s)?;
                }
                _ => {}
            }
        }

        // Second pass: declare all concrete functions and impl methods
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    if func.generics.is_empty() {
                        self.declare_function(func)?;
                    }
                }
                Item::Impl(impl_block) => {
                    self.declare_impl_methods(impl_block)?;
                }
                _ => {}
            }
        }

        // Third pass: define all concrete functions and impl methods
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    if func.generics.is_empty() {
                        self.compile_function(func)?;
                    }
                }
                Item::Impl(impl_block) => {
                    self.compile_impl_methods(impl_block)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    pub(crate) fn create_entry_block_alloca(&self, name: &str, ty: BasicTypeEnum<'ctx>) -> PointerValue<'ctx> {
        let fn_value = self.current_function.unwrap();
        let entry = fn_value.get_first_basic_block().unwrap();

        let builder = self.context.create_builder();
        match entry.get_first_instruction() {
            Some(inst) => builder.position_before(&inst),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name).unwrap()
    }

    /// Compile a static variable declaration as a global constant
    fn compile_static(&mut self, s: &Static) -> Result<(), CodegenError> {
        use crate::ast::Literal;

        // Determine the type
        let ty = if let Some(ref declared_ty) = s.ty {
            self.llvm_type(declared_ty)?
        } else {
            // Infer from value (only supports literals for now)
            match &s.value {
                Expr::Literal(Literal::Int(_, _), _) => self.context.i64_type().into(),
                Expr::Literal(Literal::Float(_), _) => self.context.f64_type().into(),
                Expr::Literal(Literal::Bool(_), _) => self.context.bool_type().into(),
                Expr::Literal(Literal::String(_), _) => {
                    // String literals are Slice<u8>
                    let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                    let len_type = self.context.i64_type();
                    self.context.struct_type(&[ptr_type.into(), len_type.into()], false).into()
                }
                _ => return Err(CodegenError::NotImplemented(
                    format!("static variable '{}' requires explicit type annotation for non-literal values", s.name)
                )),
            }
        };

        // Create the global variable
        let global = self.module.add_global(ty, None, &s.name);

        // For simple integer literals, we can set the initializer directly
        match &s.value {
            Expr::Literal(Literal::Int(val, _suffix), _) => {
                if ty.is_int_type() {
                    let int_ty = ty.into_int_type();
                    let const_val = int_ty.const_int(*val as u64, true);
                    global.set_initializer(&const_val);
                }
            }
            Expr::Literal(Literal::Float(val), _) => {
                if ty.is_float_type() {
                    let float_ty = ty.into_float_type();
                    let const_val = float_ty.const_float(*val);
                    global.set_initializer(&const_val);
                }
            }
            Expr::Literal(Literal::Bool(val), _) => {
                if ty.is_int_type() {
                    let bool_ty = ty.into_int_type();
                    let const_val = bool_ty.const_int(if *val { 1 } else { 0 }, false);
                    global.set_initializer(&const_val);
                }
            }
            _ => {
                // For complex initializers, we'd need to evaluate at runtime
                // For now, just set to zero/null and hope it's assigned later
                global.set_initializer(&ty.const_zero());
            }
        }

        // Mark as constant (immutable)
        global.set_constant(true);

        // Store the global in our static vars map for lookup
        self.static_vars.insert(s.name.clone(), global.as_pointer_value());

        Ok(())
    }

    pub fn print_ir(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }

    pub fn write_ir_to_file(&self, path: &str) -> Result<(), CodegenError> {
        self.module
            .print_to_file(path)
            .map_err(|e| CodegenError::IoError(e.to_string()))
    }

    pub fn write_object_file(&self, path: &str) -> Result<(), CodegenError> {
        use inkwell::targets::{
            CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
        };
        use inkwell::OptimizationLevel;

        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| CodegenError::TargetError(e.to_string()))?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|e| CodegenError::TargetError(e.to_string()))?;

        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();

        let target_machine = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                OptimizationLevel::Default,
                RelocMode::PIC,  // Use PIC for PIE compatibility
                CodeModel::Default,
            )
            .ok_or_else(|| CodegenError::TargetError("failed to create target machine".to_string()))?;

        target_machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
            .map_err(|e| CodegenError::IoError(e.to_string()))
    }
}

#[derive(Debug)]
pub enum CodegenError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    UndefinedType(String),
    UndefinedField(String),
    InvalidAssignment,
    InvalidArguments(String),
    BorrowError(String),
    NotImplemented(String),
    IoError(String),
    TargetError(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::UndefinedVariable(name) => write!(f, "undefined variable: {}", name),
            CodegenError::UndefinedFunction(name) => write!(f, "undefined function: {}", name),
            CodegenError::UndefinedType(name) => write!(f, "undefined type: {}", name),
            CodegenError::UndefinedField(name) => write!(f, "undefined field: {}", name),
            CodegenError::InvalidAssignment => write!(f, "invalid assignment target"),
            CodegenError::InvalidArguments(msg) => write!(f, "invalid arguments: {}", msg),
            CodegenError::BorrowError(msg) => write!(f, "borrow error: {}", msg),
            CodegenError::NotImplemented(what) => write!(f, "not implemented: {}", what),
            CodegenError::IoError(msg) => write!(f, "IO error: {}", msg),
            CodegenError::TargetError(msg) => write!(f, "target error: {}", msg),
        }
    }
}
