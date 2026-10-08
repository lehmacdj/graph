# Testing Best Practices

## Creating Readable Tests
Use helper functions to make tests more readable:

```haskell
test_formatMaybeRelativeTimestamp :: TestTree
test_formatMaybeRelativeTimestamp =
  testGroup "formatMaybeRelativeTimestamp"
    [ 60 `formatsTo` "1 min ago",
      3600 `formatsTo` "1 hour ago"
    ]
  where
    formatsTo :: NominalDiffTime -> String -> TestTree
    formatsTo offset expected =
      testCase (show offset ++ " -> " ++ show expected) $
        formatMaybeRelativeTimestamp utc ref (addUTCTime offset ref)
          @?= expected
```

Benefits:
- Concise test cases that look like data
- Descriptive test names generated automatically
- Easy to add new test cases

## Tasty
- We use the tasty-discover as our test runner. To run specific tests use `stack test --test-arguments "--pattern <string pattern>"`.
- Prefer writing Hspec `spec_` tests, over using `testGroup`

## Test Organization

### In-File Tests (Preferred)
- **Prefer** placing tests directly in the same file as the implementation
- Place test functions alongside regular functions (no preprocessor directives needed)
- Example: `Models/Path/Parse.hs`, `MyPrelude/Time.hs`

### Separate Test Files
- Use `*Spec.hs` files for tests that require additional dependencies
- Place in same directory structure under `cli/src/`
- Use `tasty-discover` naming: `test_*` for `TestTree`, `spec_*` for Hspec `Spec`

## Test Utilities

### Utils.Testing vs Utils.Testing.External
- **Utils.Testing.External**: Use in MyPrelude modules to avoid cyclic imports
  - Only depends on ClassyPrelude and testing libraries
  - Re-exports: Tasty, HUnit, Hspec, QuickCheck
- **Utils.Testing**: Use in application code
  - Includes all of Utils.Testing.External
  - Additional utilities: golden tests, graph helpers, parser testing

## Golden Tests

Use golden tests for snapshot testing of output:

```haskell
goldenTest :: String -> Text -> TestTree
goldenTestBinary :: String -> ByteString -> TestTree
goldenTestShow :: (Show a) => String -> a -> TestTree
```

- Test files placed in `test/` mirroring module structure
- Uses `HasCallStack` to automatically determine file location
- Example: test in `Models.Example` creates golden file at `test/Models/Example/<testName>`

### CLI golden tests

`test/golden/ge/**/*.golden` run `ge` on some input and compare its
stdout, stderr and resulting graph (see
`Executable.GraphEditor.GoldenSpec`). A test can start from an inline base
graph instead of an `examples/` template:

```
==========graph==========
0 -a-> 1
1 data "existing text"
==========input==========
rn a b
==========stdout==========
```

Nodes are `smallNID`s (node 0 is the origin). Such tests also show every
node's data in a trailing `data` section. Regenerate expectations with
`--ta '-p integration --accept'` and review the diff.

The `graph` section can also describe a graph that `ge` cannot produce,
to test what happens on a damaged one:

```
0 -b-> 2 !out     only node 0's outgoing record of the edge
0 -b-> 2 !in      only node 2's incoming record of the edge
missing 2         no file for node 2
raw 2 "{not json" the file of node 2 holds exactly this text
id 2 3            the file of node 2 says its id is 3
```

`missing`, `raw` and `id` are applied in order after every node's file is
written. An `exit-code` section follows `stderr` when `ge` did not exit
with 0, and output that does not end with a newline is followed by a
`<no newline at the end>` line. Each run of `ge` gets its own
`XDG_DATA_HOME`, so the tests do not touch the history file of the REPL.

The input is fed to the REPL. To run one command line as `ge -e` does
(which is where the exit code tells whether the command failed), head the
section `expression` instead of `input`.

## Avoiding Cyclic Imports

When writing tests in MyPrelude modules:
1. Import `Utils.Testing.External` instead of `Utils.Testing`
2. Avoid importing application-level modules
3. Keep tests focused on the module's own functions

## Test Naming Conventions

- `test_functionName` for TestTree-based tests (Tasty)
- `spec_functionName` for Spec-based tests (Hspec)
- Test groups should match the function/feature being tested
