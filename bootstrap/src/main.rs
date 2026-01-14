use vibec::codegen::Codegen;
use vibec::parser::Parser;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: vibec <file.vibe> [options]");
        eprintln!("Options:");
        eprintln!("  --emit=llvm    Output LLVM IR (.ll file)");
        eprintln!("  --emit=obj     Output object file (.o file)");
        eprintln!("  -o <file>      Output file name");
        std::process::exit(1);
    }

    let filename = &args[1];

    // Parse options
    let mut emit_llvm = false;
    let mut emit_obj = false;
    let mut output_name: Option<String> = None;
    let mut quiet = false;

    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "--emit=llvm" => emit_llvm = true,
            "--emit=obj" => emit_obj = true,
            "-q" | "--quiet" => quiet = true,
            "-o" => {
                i += 1;
                if i < args.len() {
                    output_name = Some(args[i].clone());
                }
            }
            _ => {
                eprintln!("Unknown option: {}", args[i]);
                std::process::exit(1);
            }
        }
        i += 1;
    }

    // Read source file
    let source = std::fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading {}: {}", filename, e);
        std::process::exit(1);
    });

    if !quiet {
        println!("Compiling: {}", filename);
    }

    // Parse
    let program = match Parser::parse(&source) {
        Ok(prog) => prog,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    if !quiet {
        println!("Parsed {} items", program.items.len());
    }

    // Generate code
    let context = inkwell::context::Context::create();
    let module_name = Path::new(filename)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");

    // Create codegen with source file path for project detection
    let source_path = Path::new(filename).canonicalize().unwrap_or_else(|_| PathBuf::from(filename));
    let mut codegen = Codegen::new_with_source(&context, module_name, &source_path);

    if let Err(e) = codegen.compile(&program) {
        eprintln!("Codegen error: {}", e);
        std::process::exit(1);
    }

    // Determine output paths
    let base_name = output_name.unwrap_or_else(|| {
        Path::new(filename)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("out")
            .to_string()
    });

    // Ensure bin directory exists
    std::fs::create_dir_all("bin").ok();

    if emit_llvm {
        let ll_path = format!("bin/{}.ll", base_name);
        if let Err(e) = codegen.write_ir_to_file(&ll_path) {
            eprintln!("Error writing LLVM IR: {}", e);
            std::process::exit(1);
        }
        if !quiet {
            println!("Wrote LLVM IR to {}", ll_path);
        }
    }

    if emit_obj {
        let obj_path = format!("bin/{}.o", base_name);
        if let Err(e) = codegen.write_object_file(&obj_path) {
            eprintln!("Error writing object file: {}", e);
            std::process::exit(1);
        }
        if !quiet {
            println!("Wrote object file to {}", obj_path);
        }
    }

    // Default: compile to native binary
    if !emit_llvm && !emit_obj {
        let obj_path = format!("bin/{}.o", base_name);
        let bin_path = format!("bin/{}", base_name);

        // Generate object file
        if let Err(e) = codegen.write_object_file(&obj_path) {
            eprintln!("Error writing object file: {}", e);
            std::process::exit(1);
        }

        // Link with system linker
        let status = Command::new("cc")
            .args([&obj_path, "-o", &bin_path])
            .status();

        match status {
            Ok(s) if s.success() => {
                if !quiet {
                    println!("Compiled to {}", bin_path);
                }
                // Clean up object file
                std::fs::remove_file(&obj_path).ok();
            }
            Ok(s) => {
                eprintln!("Linker failed with exit code: {:?}", s.code());
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("Failed to run linker: {}", e);
                std::process::exit(1);
            }
        }
    }
}
