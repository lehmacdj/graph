# Setting Up Haskell Development Environment

The CLI project uses Stack as its build tool. To set up your development environment:

## Installing Stack

We recommend using ghcup to install Stack:

1. Install ghcup (the Haskell toolchain installer):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
   ```

2. Source the ghcup environment:
   ```bash
   source ~/.ghcup/env
   ```

3. Install Stack using ghcup:
   ```bash
   ghcup install stack
   ```

4. Add the ghcup environment to your shell configuration (e.g., `~/.bashrc` or `~/.zshrc`):
   ```bash
   echo 'source ~/.ghcup/env' >> ~/.bashrc  # or ~/.zshrc
   ```

## Running Tests

From the `cli/` directory, run:

```bash
STACK_ROOT=~/.stack stack test
```

**Note:** The `STACK_ROOT=~/.stack` prefix is required in web/container environments to avoid permission issues with the default Stack root directory.

On first run, Stack will:
- Download and install GHC (Glasgow Haskell Compiler)
- Download the Hackage package index
- Build all project dependencies
- Build and run the tests

This initial setup can take several minutes. Subsequent runs will be much faster.

## Building the CLI

To build the CLI executable:

```bash
cd cli
STACK_ROOT=~/.stack stack build
```

To run the CLI:

```bash
STACK_ROOT=~/.stack stack exec graph -- <arguments>
```

## Formatting Haskell Code

This project uses [ormolu](https://github.com/tweag/ormolu) for consistent Haskell code formatting. All Haskell files must be formatted with ormolu before committing.

### Installing Ormolu

Install ormolu using ghcup:

```bash
ghcup install ormolu
```

Or download it directly:

```bash
curl -L https://github.com/tweag/ormolu/releases/download/0.7.4.0/ormolu-x86_64-linux.zip -o ormolu.zip
unzip ormolu.zip
chmod +x ormolu
sudo mv ormolu /usr/local/bin/
```

### Formatting Files

To format a single Haskell file:

```bash
ormolu --mode inplace path/to/File.hs
```

To format all Haskell files in the project:

```bash
find . -name '*.hs' -type f -print0 | xargs -0 ormolu --mode inplace
```

To check if files are formatted correctly without modifying them:

```bash
find . -name '*.hs' -type f -print0 | xargs -0 ormolu --mode check
```

**Note:** The GitHub CI will automatically check that all Haskell files are properly formatted. Make sure to format your code before pushing.
