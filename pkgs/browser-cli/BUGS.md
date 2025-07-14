# Browser CLI Bugs

## 1. Console logs from page load may not be captured

- **Issue**: Console logs that happen during initial page load might not be
  captured
- **Details**:
  - The console override in content.js needs to be active before any console
    calls
  - Logs generated after content script injection work fine
  - Console logs triggered via `browser-cli eval` are captured correctly
- **Workaround**: Reload the page after extension is active or use eval to
  trigger console logs

## 2. Drag command may not trigger drop events properly

- **Issue**: The `drag` command doesn't seem to trigger the drop event or
  complete the drag action
- **Details**:
  - Drag command executes without error but items don't actually move
  - Status messages don't update (e.g., "List reordered!" never appears)
  - The drag might only be dispatching dragstart/dragend without proper drop
    handling
- **Impact**: Cannot use drag command for actual drag-and-drop functionality
