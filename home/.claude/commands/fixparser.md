---
description: Debug parser mismatches and formatting issues
---

# Debug Parser Mismatch

When there's a parser mismatch between nixfmt-rs and the reference nixfmt implementation, follow this workflow:

1. **Find a minimal reproducer** for the mismatch
   - Start with the failing input
   - Remove unnecessary code while preserving the issue
   - Identify the smallest test case that shows the problem

2. **Add regression test** to `tests/regression_tests.rs`
   - Add the minimal reproducer as a test case
   - The test should fail initially, demonstrating the bug
   - Prevents the issue from reoccurring once fixed

3. **Add debug output** to understand the issue
   - Add `dbg!()` statements in the relevant parser functions
   - Track token positions, spans, and parsing decisions
   - Compare behavior at key decision points

4. **Look at nixfmt reference implementation** (at ../nixfmt)
   - Study how the reference implementation handles this case
   - Understand the correct parsing behavior
   - Identify what our implementation is doing differently

5. **Fix the issue** in the parser
   - Make the minimal change needed to match reference behavior
   - Ensure the fix doesn't break other cases

6. **Run the tests**
   - Execute `cargo test` to verify no regressions
   - Verify the new regression test now passes
   - Test with the original failing case

7. **Format and commit**
   - Run `cargo fmt` to format the code
   - Commit only the changed files with a descriptive message

<!-- UTF-8 marker: ✓ -->
