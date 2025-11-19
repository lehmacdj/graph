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

## Avoiding Cyclic Imports

When writing tests in MyPrelude modules:
1. Import `Utils.Testing.External` instead of `Utils.Testing`
2. Avoid importing application-level modules
3. Keep tests focused on the module's own functions

## Test Naming Conventions

- `test_functionName` for TestTree-based tests (Tasty)
- `spec_functionName` for Spec-based tests (Hspec)
- Test groups should match the function/feature being tested
