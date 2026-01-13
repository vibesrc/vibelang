import * as path from 'path';
import * as fs from 'fs';
import { workspace, ExtensionContext, window } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

function findLspBinary(configPath: string): string | undefined {
    // 1. Check environment variable (for development)
    const envPath = process.env.VIBELANG_LSP_PATH;
    if (envPath && fs.existsSync(envPath)) {
        return envPath;
    }

    // 2. Check user-configured path
    if (configPath && fs.existsSync(configPath)) {
        return configPath;
    }

    // 3. Common locations to check
    const candidates = [
        // Workspace-relative paths
        ...(workspace.workspaceFolders?.map(ws =>
            path.join(ws.uri.fsPath, 'bootstrap', 'target', 'release', 'vibelang-lsp')
        ) ?? []),
        ...(workspace.workspaceFolders?.map(ws =>
            path.join(ws.uri.fsPath, 'bootstrap', 'target', 'debug', 'vibelang-lsp')
        ) ?? []),
        // Home directory
        path.join(process.env.HOME ?? '', '.cargo', 'bin', 'vibelang-lsp'),
        // System paths will be handled by just using the binary name
    ];

    for (const candidate of candidates) {
        if (fs.existsSync(candidate)) {
            return candidate;
        }
    }

    // 3. Fall back to PATH lookup (let the system find it)
    return 'vibelang-lsp';
}

export function activate(context: ExtensionContext) {
    const outputChannel = window.createOutputChannel('Vibelang');
    outputChannel.appendLine('Vibelang extension activating...');

    const config = workspace.getConfiguration('vibelang');
    const configuredPath = config.get<string>('lsp.path') ?? '';

    outputChannel.appendLine(`Env VIBELANG_LSP_PATH: ${process.env.VIBELANG_LSP_PATH}`);
    outputChannel.appendLine(`Configured path: ${configuredPath}`);

    const serverPath = findLspBinary(configuredPath);
    outputChannel.appendLine(`Resolved server path: ${serverPath}`);

    if (!serverPath) {
        window.showWarningMessage(
            'Vibelang LSP server not found. Some features may not work. ' +
            'Build the LSP server with: cd bootstrap && cargo build --release'
        );
        return;
    }

    outputChannel.appendLine(`Starting LSP from: ${serverPath}`);
    outputChannel.appendLine(`File exists: ${fs.existsSync(serverPath)}`);
    outputChannel.show();

    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'vibelang' }],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/*.vibe')
        }
    };

    client = new LanguageClient(
        'vibelangLsp',
        'Vibelang Language Server',
        serverOptions,
        clientOptions
    );

    client.start();

    context.subscriptions.push({
        dispose: () => {
            if (client) {
                client.stop();
            }
        }
    });
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
