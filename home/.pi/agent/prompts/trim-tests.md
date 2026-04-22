---
description: Trim tests — drop redundant/low-value/brittle tests
---

Trim the test suite. Goal: shrink it. Losing some coverage is acceptable when
the test cost outweighs its value.

Scope: $ARGUMENTS (If no scope given: test files touched on this branch, else
whole suite.)

Identify and remove tests that are:

- redundant (same behaviour covered elsewhere → consolidate into one
  table-driven test)
- trivial assertions that exercise no real logic
- mock-heavy and never hit the code under test
- brittle (exact formatting, hashes, timing)

Also extract copy-paste setup into shared helpers where it pays off.

Then run the remaining tests and make them pass. Summarise what was
dropped/consolidated and any coverage intentionally given up.
