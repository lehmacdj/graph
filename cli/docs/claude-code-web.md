# Haskell Development Environment for Claude Code Web

This guide helps you set up the Haskell CLI development environment in Claude Code's web/container environment.

## Quick Start

For experienced users, here's the complete setup in one script:

```bash
# Install ghcup and Stack
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
source ~/.ghcup/env
ghcup install stack

# Configure proxy (REQUIRED for Claude Code Web)
mkdir -p ~/.stack
cat > ~/.stack/config.yaml << 'EOF'
http-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
https-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
EOF

# Run tests
cd cli
stack test
```

## Step-by-Step Setup

### 1. Install the Haskell Toolchain

Install ghcup (the Haskell toolchain installer):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
```

Activate the ghcup environment:

```bash
source ~/.ghcup/env
```

Install Stack:

```bash
ghcup install stack
```

**Optional:** Add ghcup to your shell configuration to persist across sessions:

```bash
echo 'source ~/.ghcup/env' >> ~/.bashrc  # or ~/.zshrc
```

### 2. Configure Proxy (REQUIRED for Claude Code Web)

**This step is critical.** Claude Code's web environment requires all HTTPS connections to go through a proxy. Stack doesn't automatically use the `https_proxy` environment variable, so you must configure it manually.

Create the Stack configuration file:

```bash
mkdir -p ~/.stack
cat > ~/.stack/config.yaml << 'EOF'
http-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
https-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
EOF
```

**You only need to do this once.** The configuration persists across Stack commands.

### 3. Verify Your Setup

Run the test suite:

```bash
cd cli
stack test
```

On first run, Stack will:
- Download and install GHC (Glasgow Haskell Compiler) - ~200MB
- Download the Hackage package index
- Build all project dependencies
- Build and run the tests

This initial build takes several minutes. Subsequent builds are much faster.

## Development Workflow

### Building the Project

Build the CLI executable:

```bash
cd cli
stack build
```

For faster builds during development (skips optimizations):

```bash
stack build --fast
```

### Running Tests

Run the full test suite:

```bash
cd cli
stack test
```

Run tests with fast compilation:

```bash
stack test --fast
```

### Running the CLI

Execute the CLI directly:

```bash
cd cli
stack exec graph -- <arguments>
```

Examples:

```bash
stack exec graph -- --help
stack exec graph -- ls
```

## Code Quality Tools

### Ormolu (Code Formatting)

This project requires [ormolu](https://github.com/tweag/ormolu) for consistent formatting. All Haskell files must be formatted before committing.

**Install ormolu:**

```bash
ghcup install ormolu
```

**Format files:**

```bash
# Format a single file
ormolu --mode inplace path/to/File.hs

# Format all Haskell files in the project
find cli -name '*.hs' -type f -print0 | xargs -0 ormolu --mode inplace

# Check formatting without modifying files
find cli -name '*.hs' -type f -print0 | xargs -0 ormolu --mode check
```

**Note:** GitHub CI automatically checks formatting on all PRs.

### HLint (Linting)

HLint suggests code improvements. The project includes a `.hlint.yaml` configuration, and GitHub Actions shows HLint warnings on PRs.

**Install HLint:**

```bash
stack install hlint apply-refact
```

**Run HLint:**

```bash
# Check all files in the cli directory
hlint cli/

# Check a specific file
hlint cli/src/Graph/Command.hs

# Auto-apply suggestions (use with caution)
hlint cli/src/Graph/Command.hs --refactor --refactor-options="--inplace"
```

**Recommended workflow:**
1. Run `hlint cli/` before committing
2. Review suggestions and apply manually or with `--refactor`

## Troubleshooting

### TLS/SSL Handshake Failures

If you see errors like:

```
TLS HandshakeFailed (Error_Protocol ... HandshakeFailure)
```

or

```
Network.Socket.connect: ... connection refused
```

**Solution:** Verify your proxy configuration exists:

```bash
cat ~/.stack/config.yaml
```

It should contain:

```yaml
http-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
https-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
```

If the file is missing or incorrect, recreate it using the command in Step 2 above.

### Unrecognized Fields Warning

You may see a warning:

```
Warning: /root/.stack/config.yaml: Unrecognized fields in ConfigMonoid: http-proxy, https-proxy
```

**This is normal and can be ignored.** Stack still uses the proxy settings correctly despite the warning.

### Build Failures After Environment Reset

If your environment resets (e.g., container restart), you may need to:

1. Re-source the ghcup environment: `source ~/.ghcup/env`
2. Verify the proxy config still exists: `cat ~/.stack/config.yaml`

The ghcup installation and Stack proxy config should persist in your home directory (`~/.ghcup` and `~/.stack`), but you need to source the environment in each new shell session.
