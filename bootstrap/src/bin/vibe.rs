//! Vibe CLI - the Vibelang build tool
//!
//! Commands:
//!   vibe run [file.vibe]     Compile and run a file (or project)
//!   vibe build [file.vibe]   Compile to executable
//!   vibe new <name>          Create a new project (TBD)

use std::env;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

fn print_usage() {
    eprintln!("Usage: vibe <command> [options]");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  run [file.vibe]       Compile and run a file");
    eprintln!("  build [file.vibe]     Compile to executable");
    eprintln!("  new <name>            Create a new project (coming soon)");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -o <name>             Output file name (for build)");
    eprintln!("  -v, --verbose         Show compiler output (run is quiet by default)");
    eprintln!("  -q, --quiet           Suppress compiler output (for build)");
    eprintln!("  --emit=<type>         Output type: exe, llvm, obj");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  vibe run main.vibe");
    eprintln!("  vibe build src/main.vibe -o myapp");
    eprintln!("  vibe run              # runs main.vibe or src/main.vibe");
}

fn find_vibec() -> Option<PathBuf> {
    // First, check if vibec is in the same directory as vibe
    if let Ok(exe_path) = env::current_exe() {
        let dir = exe_path.parent()?;
        let vibec = dir.join("vibec");
        if vibec.exists() {
            return Some(vibec);
        }
    }

    // Fall back to PATH
    which("vibec")
}

fn which(cmd: &str) -> Option<PathBuf> {
    env::var_os("PATH").and_then(|paths| {
        env::split_paths(&paths).find_map(|dir| {
            let full_path = dir.join(cmd);
            if full_path.exists() {
                Some(full_path)
            } else {
                None
            }
        })
    })
}

fn find_main_file() -> Option<PathBuf> {
    // Look for main.vibe in current directory
    let main_vibe = Path::new("main.vibe");
    if main_vibe.exists() {
        return Some(main_vibe.to_path_buf());
    }

    // Look for src/main.vibe
    let src_main = Path::new("src/main.vibe");
    if src_main.exists() {
        return Some(src_main.to_path_buf());
    }

    None
}

fn get_output_name(input: &Path) -> String {
    input.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output")
        .to_string()
}

fn cmd_run(args: &[String]) -> ExitCode {
    let vibec = match find_vibec() {
        Some(path) => path,
        None => {
            eprintln!("Error: vibec compiler not found");
            return ExitCode::from(1);
        }
    };

    let mut file: Option<PathBuf> = None;
    let mut verbose = false;
    let mut i = 0;

    while i < args.len() {
        let arg = &args[i];
        if arg == "-v" || arg == "--verbose" {
            verbose = true;
        } else if !arg.starts_with('-') && file.is_none() {
            file = Some(PathBuf::from(arg));
        }
        i += 1;
    }

    let input_file = match file {
        Some(f) => f,
        None => match find_main_file() {
            Some(f) => f,
            None => {
                eprintln!("Error: No .vibe file specified and no main.vibe found");
                eprintln!("Usage: vibe run [file.vibe]");
                return ExitCode::from(1);
            }
        }
    };

    if !input_file.exists() {
        eprintln!("Error: File not found: {}", input_file.display());
        return ExitCode::from(1);
    }

    let output_name = get_output_name(&input_file);
    let output_path = PathBuf::from("bin").join(&output_name);

    // Compile (quiet by default)
    let mut compile_cmd = Command::new(&vibec);
    compile_cmd.arg(&input_file);
    compile_cmd.arg("-o").arg(&output_name);
    if !verbose {
        compile_cmd.arg("-q");
    }

    let compile_status = compile_cmd.status();
    match compile_status {
        Ok(status) if status.success() => {}
        Ok(status) => {
            return ExitCode::from(status.code().unwrap_or(1) as u8);
        }
        Err(e) => {
            eprintln!("Error running compiler: {}", e);
            return ExitCode::from(1);
        }
    }

    // Run
    if verbose {
        println!("Running {}", output_path.display());
    }

    let run_status = Command::new(&output_path).status();
    match run_status {
        Ok(status) => ExitCode::from(status.code().unwrap_or(0) as u8),
        Err(e) => {
            eprintln!("Error running executable: {}", e);
            ExitCode::from(1)
        }
    }
}

fn cmd_build(args: &[String]) -> ExitCode {
    let vibec = match find_vibec() {
        Some(path) => path,
        None => {
            eprintln!("Error: vibec compiler not found");
            return ExitCode::from(1);
        }
    };

    let mut file: Option<PathBuf> = None;
    let mut output: Option<String> = None;
    let mut quiet = false;
    let mut emit: Option<String> = None;
    let mut i = 0;

    while i < args.len() {
        let arg = &args[i];
        if arg == "-q" || arg == "--quiet" {
            quiet = true;
        } else if arg == "-o" {
            i += 1;
            if i < args.len() {
                output = Some(args[i].clone());
            }
        } else if arg.starts_with("--emit=") {
            emit = Some(arg.strip_prefix("--emit=").unwrap().to_string());
        } else if !arg.starts_with('-') && file.is_none() {
            file = Some(PathBuf::from(arg));
        }
        i += 1;
    }

    let input_file = match file {
        Some(f) => f,
        None => match find_main_file() {
            Some(f) => f,
            None => {
                eprintln!("Error: No .vibe file specified and no main.vibe found");
                eprintln!("Usage: vibe build [file.vibe]");
                return ExitCode::from(1);
            }
        }
    };

    if !input_file.exists() {
        eprintln!("Error: File not found: {}", input_file.display());
        return ExitCode::from(1);
    }

    let output_name = output.unwrap_or_else(|| get_output_name(&input_file));

    // Build command
    let mut compile_cmd = Command::new(&vibec);
    compile_cmd.arg(&input_file);
    compile_cmd.arg("-o").arg(&output_name);
    if quiet {
        compile_cmd.arg("-q");
    }
    if let Some(emit_type) = emit {
        compile_cmd.arg(format!("--emit={}", emit_type));
    }

    let compile_status = compile_cmd.status();
    match compile_status {
        Ok(status) if status.success() => {
            if !quiet {
                println!("Built bin/{}", output_name);
            }
            ExitCode::SUCCESS
        }
        Ok(status) => ExitCode::from(status.code().unwrap_or(1) as u8),
        Err(e) => {
            eprintln!("Error running compiler: {}", e);
            ExitCode::from(1)
        }
    }
}

fn cmd_new(args: &[String]) -> ExitCode {
    if args.is_empty() {
        eprintln!("Error: Project name required");
        eprintln!("Usage: vibe new <name>");
        return ExitCode::from(1);
    }

    let _name = &args[0];

    eprintln!("Note: 'vibe new' is coming soon!");
    eprintln!("For now, create a .vibe file manually and run with 'vibe run <file>'");

    ExitCode::SUCCESS
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        return ExitCode::from(1);
    }

    let command = &args[1];
    let cmd_args: Vec<String> = args[2..].to_vec();

    match command.as_str() {
        "run" => cmd_run(&cmd_args),
        "build" => cmd_build(&cmd_args),
        "new" => cmd_new(&cmd_args),
        "-h" | "--help" | "help" => {
            print_usage();
            ExitCode::SUCCESS
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            eprintln!();
            print_usage();
            ExitCode::from(1)
        }
    }
}
