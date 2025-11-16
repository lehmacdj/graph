# Polysemy to Effectful Migration

This document tracks the migration from polysemy to effectful effects library.

## Completed Changes

### 1. Dependencies (package.yaml)
- ✅ Removed: `polysemy`, `polysemy-plugin`, `polysemy-readline`, `polysemy-zoo`
- ✅ Added: `effectful-core`, `effectful-th`
- ✅ Removed GHC plugin: `-fplugin=Polysemy.Plugin`

### 2. Core Infrastructure
- ✅ `MyPrelude/Effect.hs` - Converted to use Effectful
- ✅ `Utils/Polysemy.hs` - Converted (Input → Reader, updated all utilities)

### 3. Custom Effect Definitions (21 effects converted)

#### I/O Wrapper Effects (7):
- ✅ `Effect/IOWrapper/Web.hs`
- ✅ `Effect/IOWrapper/FileSystem.hs`
- ✅ `Effect/IOWrapper/FileTypeOracle.hs`
- ✅ `Effect/IOWrapper/GetTime.hs`
- ✅ `Effect/IOWrapper/Echo.hs`
- ✅ `Effect/IOWrapper/Editor.hs`
- ✅ `Effect/IOWrapper/DisplayImage.hs`

#### Graph Effects (3):
- ✅ `Graph/Effect.hs` - ReadGraph, ReadGraphDataless, WriteGraph
- ✅ `Graph/FreshNID.hs`
- ✅ `Graph/NodeLocated.hs` - GetLocation, SetLocation (created Output effect)

#### DAL Effects (4):
- ✅ `DAL/RawGraph.hs`
- ✅ `DAL/FileSystemOperations/Metadata.hs`
- ✅ `DAL/FileSystemOperations/Data.hs`
- ✅ `DAL/FileSystemOperations/MetadataWriteDiff.hs`

#### Error Effects (2):
- ✅ `Error/Warn.hs`
- ✅ `Error/Missing.hs`
- ✅ `Error/UserError.hs`

#### Graph Editing Effects (2):
- ✅ `Graph/GraphDataEditing.hs`
- ✅ `Graph/GraphMetadataEditing.hs`

#### Library Effects (3):
- ✅ `MyPrelude/EarlyReturn.hs`
- ✅ `Graph/Check.hs` - ReportMissing
- ✅ `Effect/Readline.hs` - NEW: Created effectful wrapper for haskeline

### 4. Effect Usage Sites (30+ files updated)
- ✅ All imports updated from `Polysemy.*` to `Effectful.*`
- ✅ Type signatures: `Sem` → `Eff`, `effs` → `es`
- ✅ Constraints: `Member` → `:>`, `Members` → `:>>` or multiple constraints
- ✅ Error functions: `throw` → `throwError`
- ✅ IO embedding: `Embed IO` → `IOE`
- ✅ Input effect: Replaced with `Reader` throughout
- ✅ Template Haskell: `makeSem` → `makeEffect`

### 5. Interpreters Updated
- ✅ All `interpret`, `reinterpret` calls updated for effectful API
- ✅ Pattern: `interpret $ \case` → `interpret $ \_ -> \case`
- ✅ Runner functions: `runM` → `runEff`, `runFinal` → `runEff`

## Known Remaining Issues (Requires Compilation Testing)

### Critical Files Needing Manual Review:

#### 1. `Graph/AppInterpreters.hs`
Complex type-level programming needs conversion:
- `Concat` type family (polysemy-specific) → Direct effect list composition
- `>>>` operator usage → Sequential function composition or `.`
- `subsume` calls → Remove (not needed in effectful)
- `Scoped_` → `Labeled`
- `Final` effects → `IOE`
- `Members` constraints → `:>` or `:>>` syntax

#### 2. `Executable/GraphEditor.hs`
Similar issues to AppInterpreters:
- Effect stack composition
- Scoped/Labeled conversion
- Members/Final usage

#### 3. `Graph/MaterializePath.hs`, `Graph/LegacyPathMaterialization.hs`, `Graph/Command.hs`
- May have remaining constraint syntax issues
- Effect stack composition needs verification

### Minor Issues:

#### 1. Readline Effect (`Effect/Readline.hs`)
- ✅ Created but needs testing
- Current implementation wraps haskeline
- May need adjustment based on actual usage patterns

#### 2. Scoped Effects
- Polysemy's `Scoped_` → Effectful's `Labeled`
- Conversion started but needs verification
- See `Graph/GraphMetadataEditing.hs` for example

#### 3. Type-level List Operations
- Polysemy uses `Concat` type family
- Effectful uses direct `:` cons operator
- Some complex effect stacks may need restructuring

## Testing Checklist

Once Haskell tools are available:

- [ ] Run `stack build` or `cabal build`
- [ ] Fix compilation errors iteratively
- [ ] Pay special attention to:
  - [ ] Graph/AppInterpreters.hs
  - [ ] Executable/GraphEditor.hs
  - [ ] Effect/Readline.hs integration
- [ ] Run test suite: `stack test`
- [ ] Verify all effects work correctly:
  - [ ] Graph operations (read/write)
  - [ ] File system operations
  - [ ] REPL/Readline functionality
  - [ ] Error handling
  - [ ] State management

## Migration Strategy Used

1. **Dependencies first**: Updated package.yaml to use effectful packages
2. **Core infrastructure**: Converted MyPrelude.Effect and Utils.Polysemy
3. **Effect definitions**: Converted all custom effect GADTs and makeEffect calls
4. **Interpreters**: Updated all interpret/reinterpret functions
5. **Usage sites**: Bulk replacement of common patterns (Sem→Eff, Member→:>, etc.)
6. **Special cases**: Handled Input→Reader, Scoped→Labeled, Readline wrapper

## Key Differences: Polysemy vs Effectful

| Polysemy | Effectful | Notes |
|----------|-----------|-------|
| `Sem r a` | `Eff es a` | Monad type |
| `Member e r` | `e :> es` | Single effect constraint |
| `Members [e1, e2] r` | `(e1 :> es, e2 :> es)` or `[e1, e2] :>> es` | Multiple constraints |
| `makeSem` | `makeEffect` | TH generator |
| `Embed IO` | `IOE` | IO effect |
| `Input` | `Reader` | Read-only environment |
| `throw` | `throwError` | Error throwing |
| `runM` | `runEff` | Final runner |
| `runFinal` | `runEff` | Final runner |
| `interpret $ \case` | `interpret $ \_ -> \case` | Interpreter pattern |
| `Scoped` | `Labeled` | Scoped effects |
| `Concat` | Direct `:` | Type-level lists |
| `subsume` | (not needed) | Effect already in stack |

## Notes

- Effectful has better performance than Polysemy (one design goal)
- Effectful uses static vs dynamic dispatch (we chose dynamic for compatibility)
- Some type inference may be worse/different than with polysemy-plugin
- Effect order in the type list matters (as with polysemy)

## Contact/Questions

This migration was performed by Claude AI assistant. For questions about specific
design decisions, refer to:
- Effectful documentation: https://hackage.haskell.org/package/effectful
- Polysemy documentation: https://hackage.haskell.org/package/polysemy
