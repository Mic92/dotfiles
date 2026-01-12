/// <reference types="firefox-webext-browser" />

/**
 * Content script for Browser CLI Controller
 * Optimized API for LLM agents with limited context windows
 *
 * @typedef {'css' | 'text' | 'aria-label' | 'placeholder'} SelectorType
 *
 * @typedef {object} ConsoleLog
 * @property {string} type - Log type (log, error, warn, info, debug)
 * @property {string} message - Log message
 * @property {string} timestamp - ISO timestamp
 *
 * @typedef {object} ElementData
 * @property {number} ref - Element reference number
 * @property {string} role - Element role (button, link, input, etc.)
 * @property {string} name - Accessible name
 * @property {string} [value] - Current value (for inputs)
 * @property {string[]} attrs - Attributes like [clickable], [disabled]
 * @property {Element} domElement - Reference to DOM element
 */

/** @type {ConsoleLog[]} Store console logs */
const consoleLogs = [];
/** @type {number} */
const MAX_CONSOLE_LOGS = 1000;

/** @type {HTMLDivElement|undefined} Virtual cursor element */
let virtualCursor;

/** @type {HTMLInputElement|HTMLTextAreaElement|undefined} Last focused input element */
let lastFocusedInput;

/** @type {Map<number, Element>} Current ref to element mapping */
let currentRefMap = new Map();

/** @type {Snapshot|undefined} Last snapshot for diffing */
let lastSnapshot;

// ============================================================================
// Snapshot and Element Classes
// ============================================================================

/**
 * Element wrapper with LLM-friendly toString
 */
class SnapshotElement {
  /**
   * @param {ElementData} data
   */
  constructor(data) {
    /** @type {number} */
    this.ref = data.ref;
    /** @type {string} */
    this.role = data.role;
    /** @type {string} */
    this.name = data.name;
    /** @type {string|undefined} */
    this.value = data.value;
    /** @type {string[]} */
    this.attrs = data.attrs;
    /** @type {Element} */
    this.domElement = data.domElement;
  }

  /**
   * LLM-friendly string representation
   * @returns {string}
   */
  toString() {
    let s = `[${this.ref}] ${this.role}`;
    if (this.name) s += ` "${this.name}"`;
    if (this.attrs.length > 0) s += ` [${this.attrs.join(", ")}]`;
    if (this.value !== undefined) s += ` value="${this.value}"`;
    return s;
  }

  /**
   * JSON representation (without DOM element)
   * @returns {object}
   */
  toJSON() {
    return {
      ref: this.ref,
      role: this.role,
      name: this.name,
      value: this.value,
      attrs: this.attrs,
    };
  }
}

/**
 * Snapshot class with filtering and LLM-friendly output
 */
class Snapshot {
  /**
   * @param {SnapshotElement[]} elements
   * @param {string} url
   * @param {string} title
   */
  constructor(elements, url, title) {
    /** @type {SnapshotElement[]} */
    this.elements = elements;
    /** @type {string} */
    this.url = url;
    /** @type {string} */
    this.title = title;
    /** @type {Map<number, SnapshotElement>} */
    this._refMap = new Map();
    for (const el of elements) {
      this._refMap.set(el.ref, el);
    }
  }

  /**
   * Find element by ref
   * @param {number} ref
   * @returns {SnapshotElement|undefined}
   */
  find(ref) {
    return this._refMap.get(ref);
  }

  /**
   * Get all buttons
   * @returns {SnapshotElement[]}
   */
  get buttons() {
    return this.elements.filter((e) => e.role === "button");
  }

  /**
   * Get all links
   * @returns {SnapshotElement[]}
   */
  get links() {
    return this.elements.filter((e) => e.role === "link");
  }

  /**
   * Get all inputs
   * @returns {SnapshotElement[]}
   */
  get inputs() {
    return this.elements.filter((e) =>
      ["input", "textarea", "select"].includes(e.role) ||
      e.role.startsWith("input[")
    );
  }

  /**
   * Get all form elements
   * @returns {SnapshotElement[]}
   */
  get forms() {
    return this.elements.filter((e) =>
      ["input", "textarea", "select", "checkbox", "radio"].includes(e.role) ||
      e.role.startsWith("input[")
    );
  }

  /**
   * Search by text
   * @param {string} text
   * @returns {SnapshotElement[]}
   */
  search(text) {
    const lower = text.toLowerCase();
    return this.elements.filter((e) =>
      e.name?.toLowerCase().includes(lower) ||
      e.value?.toLowerCase().includes(lower)
    );
  }

  /**
   * Compare with another snapshot and return differences
   * @param {Snapshot} other - Previous snapshot to compare against
   * @returns {SnapshotDiff}
   */
  diff(other) {
    return new SnapshotDiff(other, this);
  }

  /**
   * LLM-friendly output
   * @returns {string}
   */
  toString() {
    let out = `Page: ${this.title}\nURL: ${this.url}\n\n`;
    for (const el of this.elements) {
      out += el.toString() + "\n";
    }
    return out;
  }

  /**
   * JSON representation
   * @returns {object}
   */
  toJSON() {
    return {
      url: this.url,
      title: this.title,
      elements: this.elements.map((e) => e.toJSON()),
    };
  }
}

/**
 * Diff between two snapshots
 */
class SnapshotDiff {
  /**
   * @param {Snapshot} before
   * @param {Snapshot} after
   */
  constructor(before, after) {
    /** @type {boolean} */
    this.urlChanged = before.url !== after.url;
    /** @type {string|undefined} */
    this.oldUrl = this.urlChanged ? before.url : undefined;
    /** @type {string|undefined} */
    this.newUrl = this.urlChanged ? after.url : undefined;

    /** @type {boolean} */
    this.titleChanged = before.title !== after.title;
    /** @type {string|undefined} */
    this.oldTitle = this.titleChanged ? before.title : undefined;
    /** @type {string|undefined} */
    this.newTitle = this.titleChanged ? after.title : undefined;

    // Build maps by element signature (role + name) for comparison
    /** @type {Map<string, SnapshotElement>} */
    const beforeMap = new Map();
    for (const el of before.elements) {
      const key = `${el.role}|${el.name}`;
      beforeMap.set(key, el);
    }

    /** @type {Map<string, SnapshotElement>} */
    const afterMap = new Map();
    for (const el of after.elements) {
      const key = `${el.role}|${el.name}`;
      afterMap.set(key, el);
    }

    /** @type {SnapshotElement[]} */
    this.added = [];
    /** @type {SnapshotElement[]} */
    this.removed = [];
    /** @type {Array<{element: SnapshotElement, changes: string[]}>} */
    this.changed = [];

    // Find added and changed elements
    for (const [key, el] of afterMap) {
      const beforeEl = beforeMap.get(key);
      if (beforeEl) {
        // Check for changes
        const changes = [];
        if (beforeEl.value !== el.value) {
          changes.push(
            `value: "${beforeEl.value ?? ""}" → "${el.value ?? ""}"`,
          );
        }
        const beforeAttrs = beforeEl.attrs.join(",");
        const afterAttrs = el.attrs.join(",");
        if (beforeAttrs !== afterAttrs) {
          changes.push(`attrs: [${beforeAttrs}] → [${afterAttrs}]`);
        }
        if (changes.length > 0) {
          this.changed.push({ element: el, changes });
        }
      } else {
        this.added.push(el);
      }
    }

    // Find removed elements
    for (const [key, el] of beforeMap) {
      if (!afterMap.has(key)) {
        this.removed.push(el);
      }
    }
  }

  /**
   * Check if anything changed
   * @returns {boolean}
   */
  get hasChanges() {
    return this.urlChanged || this.titleChanged ||
      this.added.length > 0 || this.removed.length > 0 ||
      this.changed.length > 0;
  }

  /**
   * LLM-friendly output
   * @returns {string}
   */
  toString() {
    if (!this.hasChanges) {
      return "No changes";
    }

    const lines = [];

    if (this.urlChanged) {
      lines.push(`URL: ${this.oldUrl} → ${this.newUrl}`);
    }
    if (this.titleChanged) {
      lines.push(`Title: "${this.oldTitle}" → "${this.newTitle}"`);
    }

    if (this.added.length > 0) {
      lines.push("", `Added (${this.added.length}):`);
      for (const el of this.added) {
        lines.push(`  + ${el.toString()}`);
      }
    }

    if (this.removed.length > 0) {
      lines.push("", `Removed (${this.removed.length}):`);
      for (const el of this.removed) {
        lines.push(`  - ${el.toString()}`);
      }
    }

    if (this.changed.length > 0) {
      lines.push("", `Changed (${this.changed.length}):`);
      for (const { element, changes } of this.changed) {
        lines.push(
          `  ~ ${element.toString()}`,
          ...changes.map((c) => `      ${c}`),
        );
      }
    }

    return lines.join("\n");
  }

  /**
   * JSON representation
   * @returns {object}
   */
  toJSON() {
    return {
      urlChanged: this.urlChanged,
      oldUrl: this.oldUrl,
      newUrl: this.newUrl,
      titleChanged: this.titleChanged,
      oldTitle: this.oldTitle,
      newTitle: this.newTitle,
      added: this.added.map((e) => e.toJSON()),
      removed: this.removed.map((e) => e.toJSON()),
      changed: this.changed.map(({ element, changes }) => ({
        element: element.toJSON(),
        changes,
      })),
    };
  }
}

// ============================================================================
// Virtual Cursor
// ============================================================================

/**
 * Create and initialize the virtual cursor
 */
function initializeVirtualCursor() {
  if (virtualCursor) return;

  virtualCursor = document.createElement("div");
  virtualCursor.id = "browser-cli-cursor";
  virtualCursor.style.cssText = `
    position: fixed;
    width: 20px;
    height: 20px;
    pointer-events: none;
    z-index: 999999;
    transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
    opacity: 1;
  `;

  const inner = document.createElement("div");
  inner.style.cssText = `
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    width: 8px;
    height: 8px;
    background: #ff6b6b;
    border-radius: 50%;
    box-shadow: 0 0 10px rgba(255, 107, 107, 0.5);
  `;

  const outer = document.createElement("div");
  outer.style.cssText = `
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    width: 20px;
    height: 20px;
    border: 2px solid #ff6b6b;
    border-radius: 50%;
    animation: browser-cli-pulse 1.5s ease-out infinite;
  `;

  const style = document.createElement("style");
  style.textContent = `
    @keyframes browser-cli-pulse {
      0% { transform: translate(-50%, -50%) scale(1); opacity: 1; }
      100% { transform: translate(-50%, -50%) scale(2); opacity: 0; }
    }
    @keyframes browser-cli-click {
      0% { transform: translate(-50%, -50%) scale(1); }
      50% { transform: translate(-50%, -50%) scale(0.8); }
      100% { transform: translate(-50%, -50%) scale(1); }
    }
  `;

  virtualCursor.append(inner);
  virtualCursor.append(outer);
  document.head.append(style);
  document.body.append(virtualCursor);
}

/**
 * Move virtual cursor to element with animation
 * @param {Element} element - Target element
 * @param {boolean} [showClick=false] - Whether to show click animation
 */
function moveCursorToElement(element, showClick = false) {
  if (!virtualCursor) {
    initializeVirtualCursor();
  }

  const rect = element.getBoundingClientRect();
  const x = rect.left + rect.width / 2;
  const y = rect.top + rect.height / 2;

  if (virtualCursor) {
    virtualCursor.style.left = `${x}px`;
    virtualCursor.style.top = `${y}px`;
    virtualCursor.style.opacity = "1";

    if (showClick) {
      const inner = virtualCursor.querySelector("div:first-child");
      if (inner && inner instanceof HTMLElement) {
        inner.style.animation = "browser-cli-click 0.3s ease-out";
        setTimeout(() => {
          inner.style.animation = "";
        }, 300);
      }
    }
  }
}

// ============================================================================
// Console Override
// ============================================================================

/**
 * Inject console override into page context to capture all logs
 */
function injectConsoleOverride() {
  const script = document.createElement("script");
  script.textContent = `
    (function() {
      const consoleMethods = ['log', 'error', 'warn', 'info', 'debug'];
      consoleMethods.forEach(method => {
        const original = console[method];
        console[method] = function(...args) {
          window.postMessage({
            type: 'browser-cli-console',
            method: method,
            args: args.map(arg => {
              try {
                return typeof arg === 'object' ? JSON.stringify(arg) : String(arg);
              } catch {
                return '[Circular reference]';
              }
            }),
            timestamp: new Date().toISOString()
          }, '*');
          original.apply(console, args);
        };
      });
    })();
  `;
  (document.head || document.documentElement).append(script);
  script.remove();
}

window.addEventListener("message", (event) => {
  if (event.source !== window) return;

  if (event.data && event.data.type === "browser-cli-console") {
    consoleLogs.push({
      type: event.data.method,
      message: event.data.args.join(" "),
      timestamp: event.data.timestamp,
    });

    if (consoleLogs.length > MAX_CONSOLE_LOGS) {
      consoleLogs.shift();
    }
  }
});

injectConsoleOverride();

/** @type {Array<keyof Console>} */
const consoleMethods =
  /** @type {any} */ (["log", "error", "warn", "info", "debug"]);
for (const method of consoleMethods) {
  // @ts-ignore
  const original = console[method];
  // @ts-ignore
  console[method] = function (.../** @type {any[]} */ args) {
    consoleLogs.push({
      type: method,
      message: args.map((arg) => {
        try {
          return typeof arg === "object" ? JSON.stringify(arg) : String(arg);
        } catch {
          return String(arg);
        }
      }).join(" "),
      timestamp: new Date().toISOString(),
    });

    if (consoleLogs.length > MAX_CONSOLE_LOGS) {
      consoleLogs.shift();
    }

    original.apply(console, args);
  };
}

// ============================================================================
// Element Finding Functions
// ============================================================================

/**
 * Find element by ref number
 * @param {number} ref - Reference number from snapshot
 * @returns {Element} Found element
 */
function findByRef(ref) {
  const element = currentRefMap.get(ref);
  if (element) {
    // Verify element is still in DOM
    if (document.contains(element)) {
      return element;
    }
    // Element was removed, clear stale ref
    currentRefMap.delete(ref);
  }

  // Build helpful error message with available refs
  const available = [];
  for (const [r, el] of currentRefMap) {
    if (!document.contains(el)) {
      continue;
    }
    const role = getElementRole(el);
    const name = getAccessibleName(el);
    available.push(`  [${r}] ${role}${name ? ` "${name}"` : ""}`);
    if (available.length >= 5) {
      available.push("  ...");
      break;
    }
  }

  let msg = `Element [${ref}] not found.`;
  msg += available.length > 0
    ? ` Available:\n${available.join("\n")}`
    : " No refs available. Call snap() first.";
  throw new Error(msg);
}

/**
 * Find element by CSS selector
 * @param {string} selector - CSS selector
 * @returns {Element} Found element
 */
function findBySelector(selector) {
  try {
    const element = document.querySelector(selector);
    if (element) return element;
    throw new Error(`No element matches CSS selector: ${selector}`);
  } catch (error) {
    if (error instanceof Error && error.name === "SyntaxError") {
      throw new Error(`Invalid CSS selector: ${selector}`);
    }
    throw error;
  }
}

/**
 * Find element by text content
 * @param {string} text - Text to search for
 * @returns {Element} Found element
 */
function findByText(text) {
  const xpath = text.includes('"')
    ? `//*[contains(text(), '${text}')]`
    : `//*[contains(text(), "${text}")]`;

  const result = document.evaluate(
    xpath,
    document,
    undefined,
    XPathResult.FIRST_ORDERED_NODE_TYPE,
  );
  if (result.singleNodeValue && result.singleNodeValue instanceof Element) {
    return result.singleNodeValue;
  }
  throw new Error(`No element found with text: ${text}`);
}

/**
 * Find element by aria-label
 * @param {string} label - ARIA label to search for
 * @returns {Element} Found element
 */
function findByAriaLabel(label) {
  const element = document.querySelector(`[aria-label="${CSS.escape(label)}"]`);
  if (element) return element;
  throw new Error(`No element found with aria-label: ${label}`);
}

/**
 * Find element by placeholder
 * @param {string} placeholder - Placeholder text to search for
 * @returns {Element} Found element
 */
function findByPlaceholder(placeholder) {
  const element = document.querySelector(
    `[placeholder="${CSS.escape(placeholder)}"]`,
  );
  if (element) return element;
  throw new Error(`No element found with placeholder: ${placeholder}`);
}

/**
 * Find element based on selector type or ref
 * @param {string|number} selector - The selector value or ref number
 * @param {SelectorType} [selectorType='css'] - Type of selector
 * @returns {Element} Found element
 */
function find(selector, selectorType = "css") {
  // If selector is a number, treat as ref
  if (typeof selector === "number") {
    return findByRef(selector);
  }

  switch (selectorType) {
    case "css": {
      return findBySelector(selector);
    }
    case "text": {
      return findByText(selector);
    }
    case "aria-label": {
      return findByAriaLabel(selector);
    }
    case "placeholder": {
      return findByPlaceholder(selector);
    }
    default: {
      throw new Error(`Unknown selector type: ${selectorType}`);
    }
  }
}

// ============================================================================
// Snapshot Generation
// ============================================================================

/**
 * Get the role of an element
 * @param {Element} element
 * @returns {string}
 */
function getElementRole(element) {
  const explicitRole = element.getAttribute("role");
  if (explicitRole) return explicitRole;

  const tag = element.tagName.toLowerCase();

  // Map common tags to roles
  const tagRoleMap = {
    "a": "link",
    "button": "button",
    "input": "input",
    "textarea": "textarea",
    "select": "select",
    "img": "img",
    "h1": "heading",
    "h2": "heading",
    "h3": "heading",
    "h4": "heading",
    "h5": "heading",
    "h6": "heading",
    "nav": "navigation",
    "main": "main",
    "header": "banner",
    "footer": "contentinfo",
    "aside": "complementary",
    "form": "form",
    "table": "table",
    "ul": "list",
    "ol": "list",
    "li": "listitem",
  };

  if (tag === "input") {
    const type = element.getAttribute("type") || "text";
    if (type === "checkbox") return "checkbox";
    if (type === "radio") return "radio";
    if (type === "submit" || type === "button") return "button";
    return `input[${type}]`;
  }

  return /** @type {Record<string, string>} */ (tagRoleMap)[tag] || tag;
}

/**
 * Get accessible name of an element
 * @param {Element} element
 * @returns {string}
 */
function getAccessibleName(element) {
  // aria-label takes precedence
  const ariaLabel = element.getAttribute("aria-label");
  if (ariaLabel) return ariaLabel;

  // aria-labelledby
  const labelledBy = element.getAttribute("aria-labelledby");
  if (labelledBy) {
    const labelEl = document.querySelector(`#${CSS.escape(labelledBy)}`);
    if (labelEl) return labelEl.textContent?.trim() || "";
  }

  // For inputs, check associated label
  if (
    element instanceof HTMLInputElement ||
    element instanceof HTMLTextAreaElement ||
    element instanceof HTMLSelectElement
  ) {
    // Check for label wrapping the input
    const parentLabel = element.closest("label");
    if (parentLabel) {
      const text = parentLabel.textContent?.trim() || "";
      return text;
    }

    // Check for label with for attribute
    if (element.id) {
      const label = document.querySelector(
        `label[for="${CSS.escape(element.id)}"]`,
      );
      if (label) return label.textContent?.trim() || "";
    }

    // Fall back to placeholder
    const placeholder = element.getAttribute("placeholder");
    if (placeholder) return placeholder;
  }

  // alt text for images
  if (element instanceof HTMLImageElement) {
    return element.alt || "";
  }

  // title attribute
  const title = element.getAttribute("title");
  if (title) return title;

  // Text content for buttons, links, etc.
  const tag = element.tagName.toLowerCase();
  if (["button", "a", "h1", "h2", "h3", "h4", "h5", "h6"].includes(tag)) {
    // Get text content excluding script/style elements
    let text = "";
    const walker = document.createTreeWalker(
      element,
      NodeFilter.SHOW_TEXT,
      {
        acceptNode: (node) => {
          const parent = node.parentElement;
          if (
            parent && ["SCRIPT", "STYLE", "NOSCRIPT"].includes(parent.tagName)
          ) {
            return NodeFilter.FILTER_REJECT;
          }
          return NodeFilter.FILTER_ACCEPT;
        },
      },
    );
    while (walker.nextNode()) {
      text += walker.currentNode.textContent;
    }
    text = text.trim();
    // Filter out CSS-looking content (starts with . or # followed by { or contains {)
    if (text && /^\s*[.#]?[\w-]+\s*\{/.test(text)) {
      return "";
    }
    return text;
  }

  return "";
}

/**
 * Get element attributes for display
 * @param {Element} element
 * @returns {string[]}
 */
function getElementAttrs(element) {
  const attrs = [];

  // Disabled state
  if (
    element.hasAttribute("disabled") ||
    element.getAttribute("aria-disabled") === "true"
  ) {
    attrs.push("disabled");
  }

  // Checked state for checkboxes/radios
  if (
    element instanceof HTMLInputElement &&
    (element.type === "checkbox" || element.type === "radio") &&
    element.checked
  ) {
    attrs.push("checked");
  }

  // Required
  if (
    element.hasAttribute("required") ||
    element.getAttribute("aria-required") === "true"
  ) {
    attrs.push("required");
  }

  // Expanded/collapsed
  const expanded = element.getAttribute("aria-expanded");
  if (expanded === "true") attrs.push("expanded");
  if (expanded === "false") attrs.push("collapsed");

  // Readonly
  if (element.hasAttribute("readonly")) {
    attrs.push("readonly");
  }

  // Clickable (for non-obvious elements)
  const tag = element.tagName.toLowerCase();
  if (
    !["a", "button", "input", "select", "textarea"].includes(tag) &&
    (element.getAttribute("onclick") ||
      element.getAttribute("role") === "button" ||
      window.getComputedStyle(element).cursor === "pointer")
  ) {
    attrs.push("clickable");
  }

  return attrs;
}

/**
 * Get element value
 * @param {Element} element
 * @returns {string|undefined}
 */
function getElementValue(element) {
  if (element instanceof HTMLInputElement) {
    if (element.type === "checkbox" || element.type === "radio") {
      return; // Use checked attr instead
    }
    if (element.type === "password") {
      return element.value ? "••••••" : "";
    }
    return element.value;
  }

  if (element instanceof HTMLTextAreaElement) {
    return element.value;
  }

  if (element instanceof HTMLSelectElement) {
    const selected = element.options[element.selectedIndex];
    return selected ? selected.text : "";
  }
}

/**
 * Check if element should be included in snapshot
 * @param {Element} element
 * @returns {boolean}
 */
function shouldIncludeElement(element) {
  // Skip hidden elements
  const style = window.getComputedStyle(element);
  if (style.display === "none" || style.visibility === "hidden") {
    return false;
  }

  // Skip zero-size elements
  const rect = element.getBoundingClientRect();
  if (rect.width === 0 && rect.height === 0) {
    return false;
  }

  // Skip script, style, noscript, template
  const tag = element.tagName.toLowerCase();
  if (
    ["script", "style", "noscript", "template", "svg", "path"].includes(tag)
  ) {
    return false;
  }

  // Include interactive elements
  if (["a", "button", "input", "textarea", "select"].includes(tag)) {
    return true;
  }

  // Include elements with explicit roles
  if (element.hasAttribute("role")) {
    const role = element.getAttribute("role");
    if (role && !["presentation", "none"].includes(role)) return true;
  }

  // Include headings
  if (["h1", "h2", "h3", "h4", "h5", "h6"].includes(tag)) {
    return true;
  }

  // Include landmarks
  if (["nav", "main", "header", "footer", "aside", "form"].includes(tag)) {
    return true;
  }

  // Include images with alt text
  if (tag === "img" && element.getAttribute("alt")) {
    return true;
  }

  // Include elements with onclick attribute
  if (element.getAttribute("onclick")) {
    return true;
  }

  // Include elements with aria-label
  if (element.hasAttribute("aria-label")) {
    return true;
  }

  // For cursor:pointer elements, only include if they:
  // 1. Have a meaningful accessible name (not empty, not just whitespace)
  // 2. Are not generic wrapper tags (div, span, g, rect, circle, etc.)
  // 3. Or have tabindex making them keyboard-focusable
  if (window.getComputedStyle(element).cursor === "pointer") {
    const genericTags = [
      "div",
      "span",
      "g",
      "rect",
      "circle",
      "path",
      "svg",
      "text",
      "br",
      "hr",
      "b",
      "i",
      "em",
      "strong",
    ];
    if (genericTags.includes(tag)) {
      // Only include generic clickable if it has tabindex or meaningful name
      if (
        element.hasAttribute("tabindex") &&
        element.getAttribute("tabindex") !== "-1"
      ) {
        return true;
      }
      // Check for meaningful accessible name
      const name = getAccessibleName(element);
      if (name && name.length > 0 && name.length < 200 && !/^\s*$/.test(name)) {
        return true;
      }
      return false;
    }
    return true;
  }

  return false;
}

/**
 * @typedef {object} SnapOptions
 * @property {boolean} [forms] - Only form elements
 * @property {boolean} [links] - Only links
 * @property {boolean} [buttons] - Only buttons
 * @property {string} [text] - Filter by text content
 * @property {number} [near] - Elements near ref
 */

/**
 * Generate snapshot with optional filtering
 * @param {SnapOptions} [options]
 * @returns {Snapshot}
 */
function generateSnapshot(options = {}) {
  const elements = [];
  let ref = 1;

  // Clear old ref map
  currentRefMap = new Map();

  // Collect all relevant elements
  const allElements = document.body.querySelectorAll("*");

  for (const element of allElements) {
    if (!shouldIncludeElement(element)) continue;

    const role = getElementRole(element);
    const name = getAccessibleName(element);
    const attrs = getElementAttrs(element);
    const value = getElementValue(element);

    // Apply filters
    if (
      options.forms &&
      !["input", "textarea", "select", "checkbox", "radio"].includes(role) &&
      !role.startsWith("input[")
    ) {
      continue;
    }

    if (options.links && role !== "link") continue;
    if (options.buttons && role !== "button") continue;

    if (options.text) {
      const searchText = options.text.toLowerCase();
      if (
        !name.toLowerCase().includes(searchText) &&
        !(value && value.toLowerCase().includes(searchText))
      ) {
        continue;
      }
    }

    const snapshotElement = new SnapshotElement({
      ref,
      role,
      name,
      value,
      attrs,
      domElement: element,
    });

    elements.push(snapshotElement);
    currentRefMap.set(ref, element);
    ref++;
  }

  // Handle "near" filter - find elements near a reference element
  if (options.near === undefined) {
    return new Snapshot(elements, window.location.href, document.title);
  }

  const nearElement = currentRefMap.get(options.near);
  if (!nearElement) {
    return new Snapshot(elements, window.location.href, document.title);
  }

  const nearRect = nearElement.getBoundingClientRect();
  const threshold = 200; // pixels

  const filtered = elements.filter((el) => {
    const rect = el.domElement.getBoundingClientRect();
    const distance = Math.hypot(
      rect.left - nearRect.left,
      rect.top - nearRect.top,
    );
    return distance < threshold;
  });

  return new Snapshot(filtered, window.location.href, document.title);
}

// ============================================================================
// JS API - Main Functions
// ============================================================================

/**
 * Click an element
 * @param {string|number} selector - Element selector or ref number
 * @param {SelectorType|{double?: boolean}} [selectorTypeOrOptions='css']
 * @returns {Promise<{clicked: string|number}>}
 */
async function click(selector, selectorTypeOrOptions = "css") {
  let selectorType = "css";
  let double = false;

  if (typeof selectorTypeOrOptions === "object") {
    double = selectorTypeOrOptions.double || false;
  } else {
    selectorType = selectorTypeOrOptions;
  }

  const element = /** @type {HTMLElement} */ (find(
    selector,
    /** @type {SelectorType} */ (selectorType),
  ));
  moveCursorToElement(element, true);
  await new Promise((resolve) => setTimeout(resolve, 300));

  if (double) {
    element.dispatchEvent(
      new MouseEvent("dblclick", {
        view: window,
        bubbles: true,
        cancelable: true,
      }),
    );
  } else {
    element.click();
  }

  if (element.tagName === "INPUT" || element.tagName === "TEXTAREA") {
    lastFocusedInput =
      /** @type {HTMLInputElement|HTMLTextAreaElement} */ (element);
  }

  return { clicked: selector };
}

/**
 * Type text into an element
 * @param {string|number} selector - Element selector or ref number
 * @param {string} text - Text to type
 * @param {SelectorType|{clear?: boolean}} [selectorTypeOrOptions='css']
 * @returns {Promise<{typed: string, into: string|number}>}
 */
async function type(selector, text, selectorTypeOrOptions = "css") {
  let selectorType = "css";
  let clear = false;

  if (typeof selectorTypeOrOptions === "object") {
    clear = selectorTypeOrOptions.clear || false;
  } else {
    selectorType = selectorTypeOrOptions;
  }

  const element = /** @type {HTMLInputElement|HTMLTextAreaElement} */ (find(
    selector,
    /** @type {SelectorType} */ (selectorType),
  ));

  if (element.tagName !== "INPUT" && element.tagName !== "TEXTAREA") {
    throw new Error(
      `Element is not an input field. Found: <${element.tagName.toLowerCase()}>`,
    );
  }

  moveCursorToElement(element);
  await new Promise((resolve) => setTimeout(resolve, 300));
  element.focus();
  lastFocusedInput = element;

  if (clear) {
    element.value = "";
  }

  element.value = clear ? text : element.value + text;
  element.dispatchEvent(new Event("input", { bubbles: true }));
  element.dispatchEvent(new Event("change", { bubbles: true }));

  return { typed: text, into: selector };
}

/**
 * Hover over an element
 * @param {string|number} selector - Element selector or ref number
 * @param {SelectorType} [selectorType='css'] - Type of selector
 * @returns {Promise<{hovered: string|number}>}
 */
async function hover(selector, selectorType = "css") {
  const element = find(selector, selectorType);
  moveCursorToElement(element);
  await new Promise((resolve) => setTimeout(resolve, 300));

  element.dispatchEvent(
    new MouseEvent("mouseenter", {
      view: window,
      bubbles: true,
      cancelable: true,
    }),
  );
  element.dispatchEvent(
    new MouseEvent("mouseover", {
      view: window,
      bubbles: true,
      cancelable: true,
    }),
  );

  return { hovered: selector };
}

/**
 * Drag from one element to another
 * @param {string|number} startSelector - Start element selector or ref
 * @param {string|number} endSelector - End element selector or ref
 * @param {SelectorType} [selectorType='css'] - Type of selector
 * @returns {Promise<{dragged: string|number, to: string|number}>}
 */
async function drag(startSelector, endSelector, selectorType = "css") {
  const startElement = find(startSelector, selectorType);
  const endElement = find(endSelector, selectorType);

  const startRect = startElement.getBoundingClientRect();
  const endRect = endElement.getBoundingClientRect();
  const startX = startRect.left + startRect.width / 2;
  const startY = startRect.top + startRect.height / 2;
  const endX = endRect.left + endRect.width / 2;
  const endY = endRect.top + endRect.height / 2;

  moveCursorToElement(startElement, true);
  await new Promise((resolve) => setTimeout(resolve, 300));

  const dataTransfer = new DataTransfer();
  dataTransfer.effectAllowed = "all";
  dataTransfer.dropEffect = "move";

  startElement.dispatchEvent(
    new MouseEvent("mousedown", {
      view: window,
      bubbles: true,
      cancelable: true,
      clientX: startX,
      clientY: startY,
    }),
  );

  startElement.dispatchEvent(
    new DragEvent("dragstart", {
      view: window,
      bubbles: true,
      cancelable: true,
      clientX: startX,
      clientY: startY,
      dataTransfer,
    }),
  );

  if (virtualCursor) {
    virtualCursor.style.transition = "all 0.8s cubic-bezier(0.4, 0, 0.2, 1)";
    moveCursorToElement(endElement);
    await new Promise((resolve) => setTimeout(resolve, 800));
  }

  endElement.dispatchEvent(
    new DragEvent("dragenter", {
      view: window,
      bubbles: true,
      cancelable: true,
      clientX: endX,
      clientY: endY,
      dataTransfer,
    }),
  );

  endElement.dispatchEvent(
    new DragEvent("dragover", {
      view: window,
      bubbles: true,
      cancelable: true,
      clientX: endX,
      clientY: endY,
      dataTransfer,
    }),
  );

  await new Promise((resolve) => setTimeout(resolve, 50));

  endElement.dispatchEvent(
    new DragEvent("drop", {
      view: window,
      bubbles: true,
      cancelable: true,
      clientX: endX,
      clientY: endY,
      dataTransfer,
    }),
  );

  startElement.dispatchEvent(
    new DragEvent("dragend", {
      view: window,
      bubbles: true,
      cancelable: true,
      clientX: endX,
      clientY: endY,
      dataTransfer,
    }),
  );

  if (virtualCursor) {
    virtualCursor.style.transition = "all 0.3s cubic-bezier(0.4, 0, 0.2, 1)";
  }

  return { dragged: startSelector, to: endSelector };
}

/**
 * Select an option in a dropdown
 * @param {string|number} selector - Select element selector or ref
 * @param {string} option - Option value to select
 * @param {SelectorType} [selectorType='css'] - Type of selector
 * @returns {Promise<{selected: string, in: string|number}>}
 */
async function select(selector, option, selectorType = "css") {
  const element = /** @type {HTMLSelectElement} */ (find(
    selector,
    selectorType,
  ));
  if (element.tagName !== "SELECT") {
    throw new Error(
      `Element is not a select field. Found: <${element.tagName.toLowerCase()}>`,
    );
  }
  moveCursorToElement(element, true);
  await new Promise((resolve) => setTimeout(resolve, 300));
  element.value = option;
  element.dispatchEvent(new Event("change", { bubbles: true }));

  return { selected: option, in: selector };
}

/**
 * Press a keyboard key
 * @param {string} keyName - Key to press
 * @returns {Snapshot}
 */
/** @type {Record<string, {code: string, keyCode: number}>} */
const SPECIAL_KEYS = {
  "Enter": { code: "Enter", keyCode: 13 },
  "Tab": { code: "Tab", keyCode: 9 },
  "Escape": { code: "Escape", keyCode: 27 },
  "Backspace": { code: "Backspace", keyCode: 8 },
  "Delete": { code: "Delete", keyCode: 46 },
  "ArrowUp": { code: "ArrowUp", keyCode: 38 },
  "ArrowDown": { code: "ArrowDown", keyCode: 40 },
  "ArrowLeft": { code: "ArrowLeft", keyCode: 37 },
  "ArrowRight": { code: "ArrowRight", keyCode: 39 },
  "Home": { code: "Home", keyCode: 36 },
  "End": { code: "End", keyCode: 35 },
  "PageUp": { code: "PageUp", keyCode: 33 },
  "PageDown": { code: "PageDown", keyCode: 34 },
  " ": { code: "Space", keyCode: 32 },
  "Space": { code: "Space", keyCode: 32 },
};

const TEXT_INPUT_TYPES = new Set([
  "text",
  "search",
  "email",
  "password",
  "tel",
  "url",
  "number",
]);

/**
 * Build keyboard event options for a key
 * @param {string} keyName
 * @returns {{key: string, code: string, keyCode: number, which: number, bubbles: boolean, cancelable: boolean, composed: boolean}}
 */
function buildKeyEventOptions(keyName) {
  let code = keyName;
  let keyCode = 0;
  let key = keyName;

  if (SPECIAL_KEYS[keyName]) {
    code = SPECIAL_KEYS[keyName].code;
    keyCode = SPECIAL_KEYS[keyName].keyCode;
    key = keyName === "Space" ? " " : keyName;
  } else if (keyName.length === 1) {
    keyCode = keyName.codePointAt(0) || 0;
    if (keyName >= "a" && keyName <= "z") {
      code = `Key${keyName.toUpperCase()}`;
      keyCode = keyName.toUpperCase().codePointAt(0) || 0;
    } else if (keyName >= "A" && keyName <= "Z") {
      code = `Key${keyName}`;
    } else if (keyName >= "0" && keyName <= "9") {
      code = `Digit${keyName}`;
    }
  } else {
    code = keyName.charAt(0).toUpperCase() + keyName.slice(1);
  }

  return {
    key,
    code,
    keyCode,
    which: keyCode,
    bubbles: true,
    cancelable: true,
    composed: true,
  };
}

/**
 * Check if element is a text-editable input
 * @param {Element} element
 * @returns {boolean}
 */
function isTextInput(element) {
  if (element.tagName === "TEXTAREA") {
    return true;
  }
  if (element.tagName === "INPUT") {
    const type =
      /** @type {HTMLInputElement} */ (element).type?.toLowerCase() || "text";
    return TEXT_INPUT_TYPES.has(type);
  }
  return false;
}

/**
 * Insert text at cursor position in input/textarea
 * @param {HTMLInputElement|HTMLTextAreaElement} element
 * @param {string} text
 */
function insertTextAtCursor(element, text) {
  const start = element.selectionStart || 0;
  const end = element.selectionEnd || 0;
  element.value = element.value.slice(0, start) + text +
    element.value.slice(end);
  element.selectionStart = element.selectionEnd = start + text.length;
  element.dispatchEvent(
    new Event("input", { bubbles: true, cancelable: true }),
  );
}

/**
 * Try to submit the form containing an input
 * @param {HTMLInputElement} input
 * @returns {boolean} True if form was submitted
 */
function trySubmitForm(input) {
  const form = input.form;
  if (!form) {
    return false;
  }

  // Try requestSubmit first (triggers submit event handlers)
  if (typeof form.requestSubmit === "function") {
    try {
      form.requestSubmit();
      return true;
    } catch {
      // requestSubmit can throw if form is invalid, fall through
    }
  }

  // Fallback: find submit button and click it
  const submitButton = form.querySelector(
    'button[type="submit"], input[type="submit"], button:not([type])',
  );
  if (submitButton) {
    /** @type {HTMLButtonElement|HTMLInputElement} */ (submitButton).click();
    return true;
  }

  // Last resort: dispatch submit event
  form.dispatchEvent(
    new SubmitEvent("submit", { bubbles: true, cancelable: true }),
  );
  return true;
}

/**
 * Handle Tab key navigation
 * @param {Element} activeElement
 */
function handleTabKey(activeElement) {
  const focusableElements =
    /** @type {HTMLElement[]} */ ([...document.querySelectorAll(
      'a[href], button, input, textarea, select, [tabindex]:not([tabindex="-1"])',
    )]).filter((el) =>
      !(/** @type {HTMLButtonElement|HTMLInputElement} */ (el)).disabled &&
      el.offsetParent !== null
    );

  const currentIndex = focusableElements.indexOf(
    /** @type {HTMLElement} */ (activeElement),
  );
  if (currentIndex !== -1 && currentIndex < focusableElements.length - 1) {
    focusableElements[currentIndex + 1].focus();
  }

  // Track newly focused input
  setTimeout(() => {
    const newActive = document.activeElement;
    if (
      newActive &&
      (newActive.tagName === "INPUT" || newActive.tagName === "TEXTAREA")
    ) {
      lastFocusedInput =
        /** @type {HTMLInputElement|HTMLTextAreaElement} */ (newActive);
    }
  }, 50);
}

/**
 * Handle character input in contentEditable
 * @param {Element} element
 * @param {string} char
 */
function insertCharInContentEditable(element, char) {
  const selection = window.getSelection();
  if (!selection || selection.rangeCount === 0) {
    return;
  }

  const range = selection.getRangeAt(0);
  range.deleteContents();
  const textNode = document.createTextNode(char);
  range.insertNode(textNode);
  range.setStartAfter(textNode);
  range.setEndAfter(textNode);
  selection.removeAllRanges();
  selection.addRange(range);
  element.dispatchEvent(
    new Event("input", { bubbles: true, cancelable: true }),
  );
}

/**
 * Simulate a key press
 * @param {string} keyName - Key to press (e.g., "Enter", "Tab", "a", "A")
 * @returns {{key: string}}
 */
function key(keyName) {
  // Get active element, restoring last focused input if needed
  let activeElement = document.activeElement || document.body;
  if (activeElement === document.body && lastFocusedInput) {
    activeElement = lastFocusedInput;
    lastFocusedInput.focus();
  }

  const eventOptions = buildKeyEventOptions(keyName);

  // Dispatch keydown
  const keydownPrevented = !activeElement.dispatchEvent(
    new KeyboardEvent("keydown", eventOptions),
  );

  // Handle special keys
  if (!keydownPrevented) {
    if (keyName === "Enter") {
      if (activeElement.tagName === "INPUT" && isTextInput(activeElement)) {
        trySubmitForm(/** @type {HTMLInputElement} */ (activeElement));
      } else if (activeElement.tagName === "TEXTAREA") {
        insertTextAtCursor(
          /** @type {HTMLTextAreaElement} */ (activeElement),
          "\n",
        );
      }
    } else if (keyName === "Tab") {
      handleTabKey(activeElement);
    } else if (keyName.length === 1) {
      // Printable character
      if (isTextInput(activeElement)) {
        insertTextAtCursor(
          /** @type {HTMLInputElement|HTMLTextAreaElement} */ (activeElement),
          keyName,
        );
        activeElement.dispatchEvent(
          new Event("change", { bubbles: true, cancelable: true }),
        );
      } else if (
        /** @type {HTMLElement} */ (activeElement).contentEditable === "true"
      ) {
        insertCharInContentEditable(activeElement, keyName);
      }
    }
  }

  // Dispatch keypress for printable characters
  if (keyName.length === 1) {
    activeElement.dispatchEvent(new KeyboardEvent("keypress", eventOptions));
  }

  // Dispatch keyup
  activeElement.dispatchEvent(new KeyboardEvent("keyup", eventOptions));

  return { key: keyName };
}

/**
 * Get snapshot of the page
 * Returns full snapshot on first call, diff on subsequent calls (to save tokens)
 * @param {SnapOptions & {full?: boolean}} [options] - Filter options, use {full: true} for full snapshot
 * @returns {Snapshot|SnapshotDiff}
 */
function snap(options) {
  const snapshot = generateSnapshot(options);

  // If full snapshot explicitly requested or options are set (filtering), return full
  if (
    options?.full || (options && Object.keys(options).some((k) => k !== "full"))
  ) {
    if (!options || !Object.keys(options).some((k) => k !== "full")) {
      lastSnapshot = snapshot;
    }
    return snapshot;
  }

  // First call or no previous snapshot - return full snapshot
  if (!lastSnapshot) {
    lastSnapshot = snapshot;
    return snapshot;
  }

  // Return diff to save tokens
  const result = snapshot.diff(lastSnapshot);
  lastSnapshot = snapshot;
  return result;
}

/**
 * Get console logs
 * @returns {{logs: ConsoleLog[]}}
 */
function logs() {
  return { logs: [...consoleLogs] };
}

/**
 * Wait for DOM to stabilize (no mutations for a period)
 * @param {number} [stableMs=300] - How long DOM must be stable
 * @param {number} [timeout=10000] - Maximum wait time
 * @returns {Promise<{waited: string}>}
 */
function waitForIdle(stableMs = 300, timeout = 10_000) {
  return new Promise((resolve, reject) => {
    let lastMutationTime = Date.now();
    let resolved = false;
    const startTime = Date.now();

    const observer = new MutationObserver(() => {
      lastMutationTime = Date.now();
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true,
      attributes: true,
      characterData: true,
    });

    const checkStable = () => {
      if (resolved) {
        return;
      }

      const now = Date.now();
      if (now - startTime > timeout) {
        resolved = true;
        observer.disconnect();
        reject(new Error("Timeout waiting for page to stabilize"));
        return;
      }

      if (now - lastMutationTime >= stableMs) {
        resolved = true;
        observer.disconnect();
        resolve({ waited: "idle" });
        return;
      }

      setTimeout(checkStable, 50);
    };

    // Start checking after initial delay
    setTimeout(checkStable, 50);
  });
}

/**
 * Wait for condition or timeout
 * @param {number|string} condition - ms to wait, "idle", "text", or "gone"
 * @param {string|number} [value] - Text to wait for, or stableMs for idle
 * @param {number} [timeout=10000] - Maximum wait time in ms
 * @returns {Promise<{waited: number|string}>}
 */
async function wait(condition, value, timeout = 10_000) {
  if (typeof condition === "number") {
    // Simple delay
    await new Promise((resolve) => setTimeout(resolve, condition));
    return { waited: condition };
  }

  if (condition === "idle") {
    // Wait for DOM to stabilize
    const stableMs = typeof value === "number" ? value : 300;
    return waitForIdle(stableMs, timeout);
  }

  if (condition === "text" && typeof value === "string") {
    // Wait for text to appear
    const startTime = Date.now();
    while (Date.now() - startTime < timeout) {
      if (document.body.textContent?.includes(value)) {
        return { waited: `text:${value}` };
      }
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
    throw new Error(`Timeout waiting for text: "${value}"`);
  }

  if (condition === "gone" && typeof value === "string") {
    // Wait for text to disappear
    const startTime = Date.now();
    while (Date.now() - startTime < timeout) {
      if (!document.body.textContent?.includes(value)) {
        return { waited: `gone:${value}` };
      }
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
    throw new Error(`Timeout waiting for text to disappear: "${value}"`);
  }

  throw new Error(`Invalid wait condition: ${condition}`);
}

// ============================================================================
// Browser-level functions (delegate to background script)
// ============================================================================

/** @type {number} */
let bgMessageCounter = 0;

/**
 * Send a command to the background script and wait for response
 * @param {string} command
 * @param {object} [params={}]
 * @returns {Promise<any>}
 */
function sendToBackground(command, params = {}) {
  return new Promise((resolve, reject) => {
    const messageId = `content_${++bgMessageCounter}`;

    /**
     * @param {any} response
     */
    function handleResponse(response) {
      if (response && response.bgMessageId === messageId) {
        browser.runtime.onMessage.removeListener(handleResponse);
        if (response.error) {
          reject(new Error(response.error));
        } else {
          resolve(response.result);
        }
      }
    }

    browser.runtime.onMessage.addListener(handleResponse);
    browser.runtime.sendMessage({ command, params, bgMessageId: messageId });

    // Timeout after 30 seconds
    setTimeout(() => {
      browser.runtime.onMessage.removeListener(handleResponse);
      reject(new Error("Background script timeout"));
    }, 30_000);
  });
}

/**
 * Take a screenshot
 * @param {string} [path] - Output file path
 * @returns {Promise<string>} Path to saved screenshot
 */
async function shot(path) {
  const result = await sendToBackground("screenshot", { output_path: path });
  return result.screenshot_path || result.screenshot;
}

/**
 * Create a new tab
 * @param {string} [url] - URL to open
 * @returns {Promise<{tabId: string, url: string}>} Tab info
 */
async function tab(url) {
  const result = await sendToBackground("new-tab", { url });
  return { tabId: result.tabId, url: result.url };
}

/**
 * List all managed tabs
 * @returns {Promise<Array<{id: string, url: string, title: string, active: boolean}>>}
 */
async function tabs() {
  const result = await sendToBackground("list-tabs");
  return result.tabs;
}

/**
 * Download a file
 * @param {string} url - URL to download
 * @param {string} [filename] - Filename (relative to downloads folder)
 * @returns {Promise<{downloaded: string, path: string}>}
 */
async function download(url, filename) {
  return sendToBackground("download", { url, filename });
}

// ============================================================================
// Message handling for background script commands
// ============================================================================

/**
 * Serialize result for transport
 * @param {any} value
 * @returns {any}
 */
function serializeResult(value) {
  if (value === null || value === undefined) {
    return value;
  }

  // Handle Snapshot and SnapshotElement
  if (value instanceof Snapshot || value instanceof SnapshotElement) {
    return value.toJSON();
  }

  // Handle arrays
  if (Array.isArray(value)) {
    return value.map((v) => serializeResult(v));
  }

  // Handle objects with toJSON
  if (typeof value === "object" && typeof value.toJSON === "function") {
    return value.toJSON();
  }

  // Try structuredClone for other objects
  try {
    return structuredClone(value);
  } catch {
    return String(value);
  }
}

/**
 * Handle exec command - execute JavaScript with API available
 * @param {string} code - JavaScript code to execute
 * @returns {Promise<{result: any}>}
 */
async function handleExec(code) {
  // Create async function to allow await in code
  const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;

  // Wrap code to auto-return the last expression if no explicit return
  let wrappedCode = code.trim();
  if (!wrappedCode.includes("return ") && !wrappedCode.includes("return;")) {
    // Find the last statement and return it
    const lines = wrappedCode.split("\n");
    const lastLine = lines.pop()?.trim() || "";
    if (lastLine && !lastLine.endsWith(";")) {
      // Last line is an expression - return it
      wrappedCode = [...lines, `return (${lastLine})`].join("\n");
    } else if (lastLine) {
      // Last line ends with semicolon - try to return it anyway
      const expr = lastLine.slice(0, -1).trim();
      if (
        expr && !expr.startsWith("const ") && !expr.startsWith("let ") &&
        !expr.startsWith("var ")
      ) {
        wrappedCode = [...lines, `return (${expr})`].join("\n");
      }
    }
  }

  // Make API functions available in the execution context
  const fn = new AsyncFunction(
    // Content script functions
    "click",
    "type",
    "hover",
    "drag",
    "select",
    "key",
    "snap",
    "logs",
    "find",
    "wait",
    // Background script functions
    "shot",
    "tab",
    "tabs",
    "download",
    wrappedCode,
  );

  const result = await fn(
    // Content script functions
    click,
    type,
    hover,
    drag,
    select,
    key,
    snap,
    logs,
    find,
    wait,
    // Background script functions
    shot,
    tab,
    tabs,
    download,
  );

  return { result: serializeResult(result) };
}

browser.runtime.onMessage.addListener((message, _sender, _sendResponse) => {
  const { command, params, messageId } = message;

  // Skip if this is a response to a background script request
  if (message.bgMessageId) {
    return false;
  }

  if (!virtualCursor && command !== "exec") {
    initializeVirtualCursor();
  }

  /** */
  async function executeCommand() {
    try {
      let result;

      switch (command) {
        case "exec": {
          result = await handleExec(params.code);
          break;
        }
        default: {
          throw new Error(`Unknown command: ${command}`);
        }
      }

      browser.runtime.sendMessage({ messageId, result, success: true });
    } catch (error) {
      browser.runtime.sendMessage({
        messageId,
        error: error instanceof Error ? error.message : String(error),
        success: false,
      });
    }
  }

  executeCommand();
  return true;
});
