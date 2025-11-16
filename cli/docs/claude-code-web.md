# Haskell Setup for Claude Code Web

**For Claude:** This is your reference for setting up Haskell in web environments. Copy-paste the commands and move on.

## First Time Setup

Run this once per environment:

```bash
# Install toolchain
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh

# Activate (do this in EVERY new shell)
source ~/.ghcup/env

# Install Stack
ghcup install stack

# CRITICAL: Configure proxy (Stack won't work without this)
mkdir -p ~/.stack
cat > ~/.stack/config.yaml << 'EOF'
http-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
https-proxy: http://egress.public-claude-proxy.svc.cluster.local:80
EOF
```

**Verify it worked:**
```bash
stack --version  # Should show: Version 3.3.1 or similar
cat ~/.stack/config.yaml  # Should show the proxy config
```

## Every New Shell Session

```bash
source ~/.ghcup/env
```

That's it. The proxy config persists, ghcup/stack binaries persist, but the PATH doesn't.

## Common Commands

```bash
# Build the project
cd cli && stack build

# Fast build (no optimizations, use during development)
stack build --fast

# Run tests
stack test

# Run tests fast
stack test --fast

# Run the CLI
stack exec graph -- <args>
stack exec graph -- --help
stack exec graph -- ls

# Format code (REQUIRED before commits)
ghcup install ormolu  # once
find cli -name '*.hs' -exec ormolu --mode inplace {} \;

# Lint code
stack install hlint  # once
hlint cli/
```

## Errors You'll See (and Can Ignore)

**This is NORMAL:**
```
Warning: /root/.stack/config.yaml: Unrecognized fields in ConfigMonoid: http-proxy, https-proxy
```
Stack doesn't officially support these fields but uses them anyway. Ignore.

**This is NORMAL on first build:**
```
Preparing to download ghc-tinfo6-9.6.7 ...
ghc-tinfo6-9.6.7: download has begun
ghc-tinfo6-9.6.7: ... (lots of progress lines)
```
First build downloads ~200MB of GHC. Takes a few minutes. Subsequent builds are fast.

## Errors That Mean Something Is Wrong

**TLS/SSL handshake failures:**
```
TLS HandshakeFailed (Error_Protocol ... HandshakeFailure)
```
**Fix:** You forgot the proxy config. Run the proxy config commands from "First Time Setup" above.

**stack: command not found:**
```
/bin/bash: stack: command not found
```
**Fix:** Run `source ~/.ghcup/env`

**Permission denied on ~/.stack:**
```
Permission denied ... ~/.stack
```
**Fix:** This shouldn't happen with the config above, but if it does: `chmod -R u+w ~/.stack`

## How It Works (for debugging)

1. **ghcup** installs to `~/.ghcup/` and manages GHC/Stack/etc versions
2. **Stack** installs to `~/.ghcup/bin/stack` and stores its data in `~/.stack/`
3. The `~/.stack/config.yaml` file forces Stack to use the egress proxy
4. Without the proxy, Stack can't reach downloads.haskell.org or hackage.haskell.org
5. `source ~/.ghcup/env` adds `~/.ghcup/bin` to PATH

## Quick Checks

```bash
# Is ghcup installed?
ls ~/.ghcup/bin/

# Is Stack installed?
source ~/.ghcup/env && stack --version

# Is proxy configured?
cat ~/.stack/config.yaml

# Can Stack reach the internet?
stack update  # Should download Hackage index
```

## Build Times Reference

- **First build:** 5-15 minutes (downloads GHC + all dependencies)
- **Subsequent clean builds:** 3-5 minutes
- **Incremental builds:** 10-30 seconds
- **Fast incremental builds:** 5-15 seconds

## Code Quality Requirements

Before committing:
1. Format with ormolu: `find cli -name '*.hs' -exec ormolu --mode inplace {} \;`
2. Check lint: `hlint cli/` (fix what's reasonable)
3. Tests pass: `stack test`

GitHub CI will fail if formatting is wrong.

## Pro Tips

- Use `--fast` during development, full builds only for final testing
- Install ormolu once: `ghcup install ormolu`
- The first Stack command in a new environment is slow (downloads GHC), be patient
- If builds seem stuck, they're probably downloading packages - check with `ps aux | grep stack`
- Stack caches aggressively - if something seems wrong, `stack clean` and rebuild
