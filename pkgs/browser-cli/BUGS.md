# Browser CLI Bugs

## Bugs found during Amazon testing

### 1. Hidden autofill fields can interfere with form filling

- **Issue**: When typing a password on Amazon's email entry page, the text went
  into a hidden autofill field instead of waiting for the actual password page
- **Details**:
  - Amazon's login has a two-step process: first email, then password on
    separate pages
  - The email page contains a hidden password field
    (id="ap-credential-autofill-hint") for browser autofill
  - Using `input[type="password"]` selector matched this hidden field instead of
    the visible one
  - The password was typed into the hidden field before clicking Continue
  - After navigating to the actual password page, the field was empty
- **Impact**: Multi-step authentication flows may fail if hidden autofill fields
  are present
- **Workaround**: Be more specific with selectors or check element visibility
  before typing

### 2. File downloads are not captured or tracked

- **Issue**: Clicking on invoice/bill links that trigger downloads doesn't
  provide feedback
- **Details**:
  - Clicking "Rechnung" (invoice) links on Amazon likely triggers PDF downloads
  - browser-cli doesn't indicate whether a download was initiated
  - No way to verify if download succeeded or access downloaded files
  - The page doesn't change after clicking download links
- **Impact**: Cannot verify or test file download functionality
- **Feature gap**: browser-cli needs download handling capabilities
