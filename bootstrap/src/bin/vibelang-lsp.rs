//! Vibelang Language Server Protocol binary
//!
//! This is the entry point for the Vibelang LSP server.
//! All implementation details are in the vibec::lsp module.

use tower_lsp_server::{LspService, Server};
use vibec::lsp::Backend;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
