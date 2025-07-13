/// <reference types="firefox-webext-browser" />

/**
 * Content script for Browser CLI Controller
 * Executes commands in the page context
 *
 * @typedef {object} ConsoleLog
 * @property {string} type - Log type (log, error, warn, info, debug)
 * @property {string} message - Log message
 * @property {string} timestamp - ISO timestamp
 *
 * @typedef {object} AriaNode
 * @property {string} type - Node type (text or element)
 * @property {string} [content] - Text content
 * @property {string} [role] - ARIA role or tag name
 * @property {string} [label] - ARIA label or alternative text
 * @property {number} level - Nesting level
 * @property {Record<string, any>} [attributes] - Additional attributes
 */

/** @type {ConsoleLog[]} Store console logs */
const consoleLogs = [];
/** @type {number} */
const MAX_CONSOLE_LOGS = 1000;

/**
 * Override console methods to capture logs
 */
/** @type {Array<keyof Console>} */
const consoleMethods =
  /** @type {any} */ (["log", "error", "warn", "info", "debug"]);
for (const method of consoleMethods) {
  // @ts-ignore - accessing console methods dynamically
  const original = console[method];
  // @ts-ignore - we're intentionally overriding console methods
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

    // Keep only last MAX_CONSOLE_LOGS entries
    if (consoleLogs.length > MAX_CONSOLE_LOGS) {
      consoleLogs.shift();
    }

    // Call original method
    original.apply(console, args);
  };
}

/**
 * Find element by selector or text
 * @param {string} selector - CSS selector, text, aria-label, or placeholder
 * @returns {Element} Found element
 * @throws {Error} If element not found
 */
function findElement(selector) {
  // Try as CSS selector first
  try {
    const element = document.querySelector(selector);
    if (element) return element;
  } catch {
    // Not a valid CSS selector, continue
  }

  // Try to find by text content
  const xpath = `//*[contains(text(), '${selector}')]`;
  const result = document.evaluate(
    xpath,
    document,
    undefined,
    XPathResult.FIRST_ORDERED_NODE_TYPE,
  );
  if (result.singleNodeValue && result.singleNodeValue instanceof Element) {
    return result.singleNodeValue;
  }

  // Try to find by aria-label
  const ariaElement = document.querySelector(`[aria-label="${selector}"]`);
  if (ariaElement) return ariaElement;

  // Try to find by placeholder
  const placeholderElement = document.querySelector(
    `[placeholder="${selector}"]`,
  );
  if (placeholderElement) return placeholderElement;

  throw new Error(`Element not found: ${selector}`);
}

/**
 * Get ARIA snapshot of the page
 * @returns {AriaNode[]} Array of ARIA nodes
 */
function getAriaSnapshot() {
  /** @type {AriaNode[]} */
  const snapshot = [];

  /**
   * Process a DOM node recursively
   * @param {Node} node - DOM node to process
   * @param {number} [level=0] - Nesting level
   */
  function processNode(node, level = 0) {
    if (node.nodeType === Node.TEXT_NODE) {
      const text = (node.textContent || "").trim();
      if (text) {
        snapshot.push({
          type: "text",
          content: text,
          level,
        });
      }
    } else if (node.nodeType === Node.ELEMENT_NODE && node instanceof Element) {
      /** @type {HTMLElement} */
      const element = /** @type {HTMLElement} */ (node);
      const role = element.getAttribute("role") ||
        element.tagName.toLowerCase();
      const label = element.getAttribute("aria-label") ||
        element.getAttribute("alt") ||
        element.getAttribute("title") ||
        (element instanceof HTMLInputElement
          ? element.getAttribute("placeholder")
          : "") ||
        "";

      /** @type {AriaNode} */
      const item = {
        type: "element",
        role,
        label,
        level,
        attributes: /** @type {Record<string, any>} */ ({}),
      };

      // Add relevant attributes
      if (element.tagName === "A" && item.attributes) {
        item.attributes["href"] = element.getAttribute("href");
      }
      if (
        (element instanceof HTMLInputElement ||
          element instanceof HTMLTextAreaElement) && item.attributes
      ) {
        item.attributes["value"] = element.value;
        item.attributes["type"] = element.getAttribute("type");
      }
      if (
        (element.tagName === "BUTTON" ||
          ("onclick" in element && element.onclick)) && item.attributes
      ) {
        item.attributes["clickable"] = true;
      }

      snapshot.push(item);

      // Process children
      for (const child of element.childNodes) {
        processNode(child, level + 1);
      }
    }
  }

  processNode(document.body);
  return snapshot;
}

/**
 * Message handler
 * @param {any} message - Message from background script
 * @param {any} sender - Message sender
 * @param {(response?: any) => void} sendResponse - Response callback
 * @returns {boolean} True to keep channel open
 */
browser.runtime.onMessage.addListener((message, _sender, _sendResponse) => {
  const { command, params, messageId } = message;

  /**
   * Execute the requested command
   * @returns {Promise<void>}
   */
  async function executeCommand() {
    try {
      let result;

      switch (command) {
        case "click": {
          /** @type {HTMLElement} */
          const element =
            /** @type {HTMLElement} */ (findElement(params.selector));
          element.click();
          result = { message: `Clicked element: ${params.selector}` };
          break;
        }

        case "type": {
          /** @type {HTMLInputElement|HTMLTextAreaElement} */
          const element =
            /** @type {HTMLInputElement|HTMLTextAreaElement} */ (findElement(
              params.selector,
            ));
          if (element.tagName === "INPUT" || element.tagName === "TEXTAREA") {
            element.focus();
            element.value = params.text;
            element.dispatchEvent(new Event("input", { bubbles: true }));
            element.dispatchEvent(new Event("change", { bubbles: true }));
          } else {
            throw new Error("Element is not an input field");
          }
          result = { message: `Typed text into: ${params.selector}` };
          break;
        }

        case "hover": {
          /** @type {Element} */
          const element = findElement(params.selector);
          const event = new MouseEvent("mouseover", {
            view: window,
            bubbles: true,
            cancelable: true,
          });
          element.dispatchEvent(event);
          result = { message: `Hovered over: ${params.selector}` };
          break;
        }

        case "drag": {
          /** @type {Element} */
          const startElement = findElement(params.startSelector);
          /** @type {Element} */
          const endElement = findElement(params.endSelector);

          // Simulate drag and drop
          const startRect = startElement.getBoundingClientRect();
          const endRect = endElement.getBoundingClientRect();

          // Drag start
          startElement.dispatchEvent(
            new MouseEvent("mousedown", {
              view: window,
              bubbles: true,
              cancelable: true,
              clientX: startRect.left + startRect.width / 2,
              clientY: startRect.top + startRect.height / 2,
            }),
          );

          // Drag over
          endElement.dispatchEvent(
            new MouseEvent("mousemove", {
              view: window,
              bubbles: true,
              cancelable: true,
              clientX: endRect.left + endRect.width / 2,
              clientY: endRect.top + endRect.height / 2,
            }),
          );

          // Drop
          endElement.dispatchEvent(
            new MouseEvent("mouseup", {
              view: window,
              bubbles: true,
              cancelable: true,
              clientX: endRect.left + endRect.width / 2,
              clientY: endRect.top + endRect.height / 2,
            }),
          );

          result = {
            message:
              `Dragged from ${params.startSelector} to ${params.endSelector}`,
          };
          break;
        }

        case "select": {
          /** @type {HTMLSelectElement} */
          const element =
            /** @type {HTMLSelectElement} */ (findElement(params.selector));
          if (element.tagName === "SELECT") {
            element.value = params.option;
            element.dispatchEvent(new Event("change", { bubbles: true }));
          } else {
            throw new Error("Element is not a select field");
          }
          result = {
            message: `Selected option ${params.option} in ${params.selector}`,
          };
          break;
        }

        case "key": {
          /** @type {Element} */
          const activeElement = document.activeElement || document.body;
          const event = new KeyboardEvent("keydown", {
            key: params.key,
            code: params.key,
            bubbles: true,
            cancelable: true,
          });
          activeElement.dispatchEvent(event);

          // Also dispatch keyup
          activeElement.dispatchEvent(
            new KeyboardEvent("keyup", {
              key: params.key,
              code: params.key,
              bubbles: true,
              cancelable: true,
            }),
          );

          result = { message: `Pressed key: ${params.key}` };
          break;
        }

        case "getConsole": {
          result = { logs: consoleLogs };
          break;
        }

        case "getSnapshot": {
          result = { snapshot: getAriaSnapshot() };
          break;
        }

        default: {
          throw new Error(`Unknown command: ${command}`);
        }
      }

      // Send response back to background script
      browser.runtime.sendMessage({
        messageId,
        result,
        success: true,
      });
    } catch (error) {
      // Send error response
      browser.runtime.sendMessage({
        messageId,
        error: error instanceof Error ? error.message : String(error),
        success: false,
      });
    }
  }

  executeCommand();
  return true; // Keep message channel open for async response
});
