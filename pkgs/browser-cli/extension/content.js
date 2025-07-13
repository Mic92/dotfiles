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

/** @type {HTMLDivElement|undefined} Virtual cursor element */
let virtualCursor;

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
    opacity: 0;
  `;

  // Create cursor inner circle
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

  // Create cursor outer ring
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

  // Add animation styles
  const style = document.createElement("style");
  style.textContent = `
    @keyframes browser-cli-pulse {
      0% {
        transform: translate(-50%, -50%) scale(1);
        opacity: 1;
      }
      100% {
        transform: translate(-50%, -50%) scale(2);
        opacity: 0;
      }
    }
    
    @keyframes browser-cli-click {
      0% {
        transform: translate(-50%, -50%) scale(1);
      }
      50% {
        transform: translate(-50%, -50%) scale(0.8);
      }
      100% {
        transform: translate(-50%, -50%) scale(1);
      }
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

  // Show and move cursor
  if (virtualCursor) {
    virtualCursor.style.left = `${x}px`;
    virtualCursor.style.top = `${y}px`;
    virtualCursor.style.opacity = "1";

    if (showClick) {
      // Add click animation
      const inner = virtualCursor.querySelector("div:first-child");
      if (inner && inner instanceof HTMLElement) {
        inner.style.animation = "browser-cli-click 0.3s ease-out";
        setTimeout(() => {
          inner.style.animation = "";
        }, 300);
      }
    }

    // Hide cursor after 2 seconds
    setTimeout(() => {
      if (virtualCursor) {
        virtualCursor.style.opacity = "0";
      }
    }, 2000);
  }
}

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
 * Handle click command
 * @param {object} params - Command parameters
 * @param {string} params.selector - Element selector
 * @returns {Promise<object>} Result message
 */
async function handleClick(params) {
  /** @type {HTMLElement} */
  const element = /** @type {HTMLElement} */ (findElement(params.selector));
  moveCursorToElement(element, true);
  // Delay click to show animation
  await new Promise((resolve) => setTimeout(resolve, 300));
  element.click();
  return { message: `Clicked element: ${params.selector}` };
}

/**
 * Handle type command
 * @param {object} params - Command parameters
 * @param {string} params.selector - Element selector
 * @param {string} params.text - Text to type
 * @returns {Promise<object>} Result message
 */
async function handleType(params) {
  /** @type {HTMLInputElement|HTMLTextAreaElement} */
  const element =
    /** @type {HTMLInputElement|HTMLTextAreaElement} */ (findElement(
      params.selector,
    ));
  if (element.tagName === "INPUT" || element.tagName === "TEXTAREA") {
    moveCursorToElement(element);
    await new Promise((resolve) => setTimeout(resolve, 300));
    element.focus();
    element.value = params.text;
    element.dispatchEvent(new Event("input", { bubbles: true }));
    element.dispatchEvent(new Event("change", { bubbles: true }));
  } else {
    throw new Error("Element is not an input field");
  }
  return { message: `Typed text into: ${params.selector}` };
}

/**
 * Handle hover command
 * @param {object} params - Command parameters
 * @param {string} params.selector - Element selector
 * @returns {Promise<object>} Result message
 */
async function handleHover(params) {
  /** @type {Element} */
  const element = findElement(params.selector);
  moveCursorToElement(element);
  await new Promise((resolve) => setTimeout(resolve, 300));
  const event = new MouseEvent("mouseover", {
    view: window,
    bubbles: true,
    cancelable: true,
  });
  element.dispatchEvent(event);
  return { message: `Hovered over: ${params.selector}` };
}

/**
 * Handle drag command
 * @param {object} params - Command parameters
 * @param {string} params.startSelector - Start element selector
 * @param {string} params.endSelector - End element selector
 * @returns {Promise<object>} Result message
 */
async function handleDrag(params) {
  /** @type {Element} */
  const startElement = findElement(params.startSelector);
  /** @type {Element} */
  const endElement = findElement(params.endSelector);

  // Simulate drag and drop
  const startRect = startElement.getBoundingClientRect();
  const endRect = endElement.getBoundingClientRect();

  // Move cursor to start position
  moveCursorToElement(startElement, true);
  await new Promise((resolve) => setTimeout(resolve, 300));

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

  // Animate cursor to end position
  if (virtualCursor) {
    virtualCursor.style.transition = "all 0.8s cubic-bezier(0.4, 0, 0.2, 1)";
    moveCursorToElement(endElement);
    await new Promise((resolve) => setTimeout(resolve, 800));
  }

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

  // Reset transition speed
  if (virtualCursor) {
    virtualCursor.style.transition = "all 0.3s cubic-bezier(0.4, 0, 0.2, 1)";
  }

  return {
    message: `Dragged from ${params.startSelector} to ${params.endSelector}`,
  };
}

/**
 * Handle select command
 * @param {object} params - Command parameters
 * @param {string} params.selector - Select element selector
 * @param {string} params.option - Option to select
 * @returns {Promise<object>} Result message
 */
async function handleSelect(params) {
  /** @type {HTMLSelectElement} */
  const element =
    /** @type {HTMLSelectElement} */ (findElement(params.selector));
  if (element.tagName === "SELECT") {
    moveCursorToElement(element, true);
    await new Promise((resolve) => setTimeout(resolve, 300));
    element.value = params.option;
    element.dispatchEvent(new Event("change", { bubbles: true }));
  } else {
    throw new Error("Element is not a select field");
  }
  return {
    message: `Selected option ${params.option} in ${params.selector}`,
  };
}

/**
 * Handle key press command
 * @param {object} params - Command parameters
 * @param {string} params.key - Key to press
 * @returns {object} Result message
 */
function handleKey(params) {
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

  return { message: `Pressed key: ${params.key}` };
}

/**
 * Handle cursor visibility command
 * @param {object} params - Command parameters
 * @param {boolean} params.visible - Whether cursor should be visible
 * @returns {object} Result message
 */
function handleCursorVisibility(params) {
  if (params.visible === false && virtualCursor) {
    virtualCursor.style.display = "none";
  } else if (params.visible === true && virtualCursor) {
    virtualCursor.style.display = "block";
  }
  return { message: `Cursor visibility set to ${params.visible}` };
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

  // Initialize cursor on first command
  if (!virtualCursor && command !== "getConsole" && command !== "getSnapshot") {
    initializeVirtualCursor();
  }

  /**
   * Execute the requested command
   * @returns {Promise<void>}
   */
  async function executeCommand() {
    try {
      let result;

      switch (command) {
        case "click": {
          result = await handleClick(params);
          break;
        }

        case "type": {
          result = await handleType(params);
          break;
        }

        case "hover": {
          result = await handleHover(params);
          break;
        }

        case "drag": {
          result = await handleDrag(params);
          break;
        }

        case "select": {
          result = await handleSelect(params);
          break;
        }

        case "key": {
          result = handleKey(params);
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

        case "setCursorVisibility": {
          result = handleCursorVisibility(params);
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
