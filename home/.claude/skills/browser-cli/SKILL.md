---
name: browser-cli
description: Control Firefox browser from the command line. Use for web automation, scraping, testing, or any browser interaction tasks.
---

# Usage

```bash
# List managed tabs
browser-cli --list

# Open a page and get snapshot
browser-cli <<'EOF'
await tab("https://example.com")
snap()
EOF

# Execute in a specific tab
browser-cli abc123 <<'EOF'
snap()
EOF
```

# JavaScript API

All functions are available in the execution context. Actions return simple
confirmations; use `snap()` to get page state.

## Element Interaction (use refs from snap())
```bash
browser-cli <<'EOF'
await click(1)                    // Click element [1]
await click(1, {double: true})    // Double click
await type(2, "user@example.com") // Type into element [2]
await type(2, "new", {clear: true}) // Clear first, then type
await hover(3)                    // Hover over element [3]
await drag(4, 5)                  // Drag from [4] to [5]
await select(6, "option-value")   // Select dropdown option
key("Enter")                      // Press key
key("Tab")
EOF

# Can still use CSS selectors when needed
browser-cli <<'EOF'
await click("#submit-button")
await click("Sign In", "text")
EOF
```

## Page Inspection
```bash
browser-cli <<'EOF'
snap()                    // Get page snapshot with refs
snap({forms: true})       // Only form elements
snap({links: true})       // Only links
snap({buttons: true})     // Only buttons
snap({text: "login"})     // Elements containing "login"
diff()                    // Show changes since last snap
logs()                    // Get console logs
EOF
```

## Waiting
```bash
browser-cli <<'EOF'
await wait(1000)              // Wait 1 second
await wait("idle")            // Wait for DOM to stabilize
await wait("text", "Success") // Wait for text to appear
await wait("gone", "Loading") // Wait for text to disappear
EOF
```

## Screenshots & Tabs
```bash
browser-cli <<'EOF'
await shot()                      // Screenshot, returns data URL
await shot("/tmp/page.png")       // Screenshot to file
await tab()                       // New tab
await tab("https://example.com")  // New tab with URL
await tabs()                      // List all tabs
EOF
```

# Snapshot Output Format

```
Page: Example Site
URL: https://example.com

[1] heading "Welcome"
[2] input[email] "Email" [required]
[3] input[password] "Password" [required]
[4] checkbox "Remember me"
[5] button "Sign In"
[6] link "Forgot password?"
```

- `[N]` - Reference number for use with click(), type(), etc.
- Role and accessible name shown
- Attributes in brackets: `[disabled]`, `[checked]`, `[required]`, etc.

# Examples

```bash
# Search on Google
browser-cli <<'EOF'
await tab("https://google.com")
snap()
EOF
# Output shows [12] combobox "Suche"

browser-cli <<'EOF'
await type(12, "hello world")
diff()
EOF
# Shows: Added (autocomplete options), Changed (input value)

# Form filling
browser-cli <<'EOF'
await tab("https://example.com/login")
snap()
EOF
# Output: [1] input "Email", [2] input "Password", [3] button "Sign In"

browser-cli <<'EOF'
await type(1, "user@test.com")
await type(2, "secret123")
await click(3)
diff()
EOF

# Wait for dynamic content
browser-cli <<'EOF'
await click(5)
await wait("text", "Success")
snap()
EOF
```
