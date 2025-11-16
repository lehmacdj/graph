# Haskell Setup for Claude Code Web

**For Claude:** This is your reference for setting up Haskell in web environments. Copy-paste the commands and move on.

## First Time Setup

Run this once per environment:

```bash
# Step 1: Install system dependencies (REQUIRED - builds will fail without these)
apt-get update && apt-get install -y build-essential libgmp-dev libpcre3-dev libffi-dev libncurses-dev pkg-config

# Step 2: Install ghcup toolchain
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh

# Step 3: Activate ghcup (do this in EVERY new shell)
source ~/.ghcup/env

# Step 4: Install Stack
ghcup install stack

# Step 5: Configure proxy (Stack won't work without this)
mkdir -p ~/.stack
cat > ~/.stack/config.yaml << 'EOF'
http-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
https-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
EOF
```

That's it! You can now build the project.

## Every New Shell Session

```bash
source ~/.ghcup/env
```

That's it. The proxy config persists, ghcup/stack binaries persist, but the PATH doesn't.

## Common Commands

```bash
# Build (use --fast for development)
cd cli && stack build --fast

# Run tests
stack test --fast

# Format code (REQUIRED before commits)
ghcup install ormolu  # once
find cli -name '*.hs' -exec ormolu --mode inplace {} \;

# Run the graph CLI
stack exec graph -- ls
stack exec graph -- --help
```

## What to Expect

**Normal warnings (ignore these):**
- `Warning: /root/.stack/config.yaml: Unrecognized fields in ConfigMonoid: http-proxy, https-proxy` - Stack uses these fields even though they're "unrecognized"

**First build:**
- Downloads ~200MB of GHC (takes 2-5 minutes)
- Compiles all dependencies (takes 5-10 minutes)
- Subsequent builds are much faster (seconds to minutes)
- **Pro tip:** Start `stack build --fast` in the background immediately after setup, then begin making edits while dependencies compile. Most of the build time is compiling dependencies (not your code), so you can work in parallel.

## Troubleshooting

**Build fails with "cannot find -lgmp" or similar:**
- You didn't install system dependencies. Run Step 1 from "First Time Setup"

**TLS/SSL handshake failures:**
- Proxy not configured. Run Step 5 from "First Time Setup"

**"stack: command not found":**
- Run `source ~/.ghcup/env`

## Before Committing

1. Format code: `find cli -name '*.hs' -exec ormolu --mode inplace {} \;`
2. Run tests: `stack test --fast`
3. Verify build: `stack build --fast`

GitHub CI will reject commits with incorrect formatting.
