/// <reference types="firefox-webext-browser" />

/**
 * Background script for Browser CLI Controller
 * Manages native messaging connection and message routing
 * @typedef {object} Message
 * @property {string} command - The command to execute
 * @property {object} params - Command parameters
 * @property {string} id - Unique message ID
 * @property {string} [tabId] - Target tab ID for the command
 *
 * @typedef {object} CommandResponse
 * @property {string} id - Message ID
 * @property {boolean} success - Whether command succeeded
 * @property {object} [result] - Command result
 * @property {string} [error] - Error message if failed
 *
 * @typedef {object} NativeMessage
 * @property {boolean} [ready] - Whether the native host is ready
 * @property {string} [socket_path] - The socket path for CLI communication
 * @property {string} [command] - The command to execute
 * @property {object} [params] - Command parameters
 * @property {string} [id] - Unique message ID
 */

/** @type {browser.runtime.Port|undefined} Native messaging port */
let nativePort;

/** @type {Record<string, {resolve: Function, reject: Function}>} Message handlers from content scripts */
const messageHandlers = {};

/** @type {Map<string, {tabId: number, url: string, title: string}>} Map of managed tabs with short IDs */
const managedTabs = new Map();

/** @type {string|undefined} Currently active managed tab ID */
let activeTabId;

/**
 * Generate a short random ID for tabs
 * @returns {string} A short random ID (6 characters)
 */
function generateTabId() {
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  for (let i = 0; i < 6; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result;
}

/**
 * Connect to native messaging host
 * @returns {void}
 */
function connectNativeHost() {
  if (nativePort) {
    return;
  }

  console.log("Connecting to native messaging host...");

  try {
    nativePort = browser.runtime.connectNative(
      "io.thalheim.browser_cli.bridge",
    );

    nativePort.onMessage.addListener(
      async (/** @type {NativeMessage} */ message) => {
        console.log("Received from native host:", message);

        // Check if this is the initial ready message
        if (message.ready && message.socket_path) {
          console.log(
            "Native messaging bridge ready, socket at:",
            message.socket_path,
          );
          return;
        }

        // Handle commands from CLI
        if (message.command) {
          await handleCommand(/** @type {Message} */ (message));
        }
      },
    );

    nativePort.onDisconnect.addListener(() => {
      console.log("Native messaging disconnected");
      if (browser.runtime.lastError) {
        console.error("Native messaging error:", browser.runtime.lastError);
      }
      nativePort = undefined;

      // Try to reconnect after 5 seconds
      setTimeout(connectNativeHost, 5000);
    });
  } catch (error) {
    console.error("Failed to connect to native messaging host:", error);
    nativePort = undefined;
  }
}

/**
 * Send response back to native host
 * @param {CommandResponse} response - Response to send
 * @returns {void}
 */
function sendResponse(response) {
  if (nativePort) {
    try {
      nativePort.postMessage(response);
    } catch (error) {
      console.error("Failed to send response:", error);
    }
  }
}

/**
 * Handle commands from CLI
 * @param {Message} message - The command message
 * @returns {Promise<void>}
 */
async function handleCommand(message) {
  const { command, params, id, tabId } = message;

  /** @type {{url?: string, element?: string, text?: string, startElement?: string, endElement?: string, option?: string, seconds?: number, key?: string}} */
  const typedParams = params;

  try {
    let result;

    switch (command) {
      case "navigate": {
        result = await navigate(typedParams.url || "", tabId);
        break;
      }

      case "back": {
        result = await goBack(tabId);
        break;
      }

      case "forward": {
        result = await goForward(tabId);
        break;
      }

      case "click": {
        result = await clickElement(typedParams.element || "", tabId);
        break;
      }

      case "type": {
        result = await typeText(
          typedParams.element || "",
          typedParams.text || "",
          tabId,
        );
        break;
      }

      case "hover": {
        result = await hoverElement(typedParams.element || "", tabId);
        break;
      }

      case "drag": {
        result = await dragElement(
          typedParams.startElement || "",
          typedParams.endElement || "",
          tabId,
        );
        break;
      }

      case "select": {
        result = await selectOption(
          typedParams.element || "",
          typedParams.option || "",
          tabId,
        );
        break;
      }

      case "key": {
        result = await pressKey(typedParams.key || "", tabId);
        break;
      }

      case "screenshot": {
        result = await takeScreenshot(tabId);
        break;
      }

      case "console": {
        result = await getConsoleLogs(tabId);
        break;
      }

      case "snapshot": {
        result = await getSnapshot(tabId);
        break;
      }

      case "list-tabs": {
        result = await listTabs();
        break;
      }

      case "new-tab": {
        result = await createNewTab(typedParams.url);
        break;
      }

      default: {
        throw new Error(`Unknown command: ${command}`);
      }
    }

    // Send response
    sendResponse({
      id,
      result,
      success: true,
    });
  } catch (error) {
    // Send error response
    sendResponse({
      id,
      error: error instanceof Error ? error.message : String(error),
      success: false,
    });
  }
}

/**
 * Get the browser tab ID to use for a command
 * @param {string} [targetTabId] - Specific tab ID requested
 * @returns {Promise<number>} Browser tab ID
 */
async function getTargetTab(targetTabId) {
  if (targetTabId) {
    const managedTab = managedTabs.get(targetTabId);
    if (!managedTab) {
      throw new Error(`Tab ${targetTabId} not found`);
    }
    return managedTab.tabId;
  } else if (activeTabId) {
    const managedTab = managedTabs.get(activeTabId);
    if (!managedTab) {
      throw new Error(`Active tab ${activeTabId} no longer exists`);
    }
    return managedTab.tabId;
  } else {
    throw new Error(
      "No active tab. Create a tab first with 'browser-cli new-tab'",
    );
  }
}

/**
 * Browser command implementations
 * @param {string} url - URL to navigate to
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<{message: string}>}
 */
async function navigate(url, tabId) {
  const browserTabId = await getTargetTab(tabId);
  await browser.tabs.update(browserTabId, { url });

  // Update stored URL
  const managedTabId = tabId || activeTabId;
  if (managedTabId && managedTabs.has(managedTabId)) {
    const managedTab = managedTabs.get(managedTabId);
    if (managedTab) {
      managedTab.url = url;
    }
  }

  return { message: `Navigated to ${url}` };
}

/**
 * Navigate back in browser history
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<{message: string}>}
 */
async function goBack(tabId) {
  const browserTabId = await getTargetTab(tabId);
  await browser.tabs.goBack(browserTabId);
  return { message: "Navigated back" };
}

/**
 * Navigate forward in browser history
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<{message: string}>}
 */
async function goForward(tabId) {
  const browserTabId = await getTargetTab(tabId);
  await browser.tabs.goForward(browserTabId);
  return { message: "Navigated forward" };
}

/**
 * Send command to content script
 * @param {string} command - Command name
 * @param {object} [params={}] - Command parameters
 * @param {string} [targetTabId] - Specific tab ID to target
 * @returns {Promise<object>} Response from content script
 */
async function sendToContentScript(command, params = {}, targetTabId) {
  const tabId = await getTargetTab(targetTabId);

  return new Promise((resolve, reject) => {
    const messageId = Date.now().toString();

    messageHandlers[messageId] = { resolve, reject };

    browser.tabs.sendMessage(tabId, {
      command,
      params,
      messageId,
    });

    // Timeout after 10 seconds
    setTimeout(() => {
      if (messageHandlers[messageId]) {
        delete messageHandlers[messageId];
        reject(new Error("Content script timeout"));
      }
    }, 10_000);
  });
}

/**
 * Click an element
 * @param {string} selector - Element selector
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function clickElement(selector, tabId) {
  return sendToContentScript("click", { selector }, tabId);
}

/**
 * Type text into an element
 * @param {string} selector - Element selector
 * @param {string} text - Text to type
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function typeText(selector, text, tabId) {
  return sendToContentScript("type", { selector, text }, tabId);
}

/**
 * Hover over an element
 * @param {string} selector - Element selector
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function hoverElement(selector, tabId) {
  return sendToContentScript("hover", { selector }, tabId);
}

/**
 * Drag from one element to another
 * @param {string} startSelector - Start element selector
 * @param {string} endSelector - End element selector
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function dragElement(startSelector, endSelector, tabId) {
  return sendToContentScript("drag", { startSelector, endSelector }, tabId);
}

/**
 * Select an option in a dropdown
 * @param {string} selector - Select element selector
 * @param {string} option - Option to select
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function selectOption(selector, option, tabId) {
  return sendToContentScript("select", { selector, option }, tabId);
}

/**
 * Press a keyboard key
 * @param {string} key - Key to press
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function pressKey(key, tabId) {
  return sendToContentScript("key", { key }, tabId);
}

/**
 * Take a screenshot of the current tab
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<{screenshot: string}>} Data URL of screenshot
 */
async function takeScreenshot(tabId) {
  const browserTabId = await getTargetTab(tabId);
  // Make the tab active temporarily to capture it
  await browser.tabs.update(browserTabId, { active: true });
  const dataUrl = await browser.tabs.captureVisibleTab();
  return { screenshot: dataUrl };
}

/**
 * Get console logs from the page
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function getConsoleLogs(tabId) {
  return sendToContentScript("getConsole", {}, tabId);
}

/**
 * Get ARIA snapshot of the page
 * @param {string} [tabId] - Target tab ID
 * @returns {Promise<object>}
 */
async function getSnapshot(tabId) {
  return sendToContentScript("getSnapshot", {}, tabId);
}

/**
 * List all managed tabs
 * @returns {Promise<{tabs: Array<{id: string, url: string, title: string, active: boolean}>}>}
 */
async function listTabs() {
  const tabs = [];
  const activeBrowserTab = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });
  const activeBrowserTabId = activeBrowserTab[0]?.id;

  for (const [shortId, tab] of managedTabs) {
    try {
      // Get updated tab info
      const browserTab = await browser.tabs.get(tab.tabId);
      tabs.push({
        id: shortId,
        url: browserTab.url || "",
        title: browserTab.title || "Untitled",
        active: browserTab.id === activeBrowserTabId,
      });
    } catch {
      // Tab no longer exists, remove it
      managedTabs.delete(shortId);
      if (activeTabId === shortId) {
        activeTabId = undefined;
      }
    }
  }

  return { tabs };
}

/**
 * Create a new managed tab
 * @param {string} [url] - URL to open in the new tab
 * @returns {Promise<{tabId: string, url: string}>}
 */
async function createNewTab(url) {
  const shortId = generateTabId();
  const tabUrl = url || "https://example.com";

  // Create the tab
  const tab = await browser.tabs.create({ url: tabUrl, active: true });

  if (tab.id === undefined) {
    throw new Error("Failed to create tab");
  }

  // Store the tab
  managedTabs.set(shortId, {
    tabId: tab.id,
    url: tabUrl,
    title: tab.title || "New Tab",
  });

  // Set as active tab
  activeTabId = shortId;

  // Enable extension on the new tab
  await enableOnTab(tab.id, shortId);

  return { tabId: shortId, url: tabUrl };
}

// Listen for messages from content scripts
browser.runtime.onMessage.addListener(
  /**
   * @param {any} message
   * @param {browser.runtime.MessageSender} sender
   * @param {(response?: any) => void} _sendResponse
   * @returns {boolean}
   */
  (message, sender, _sendResponse) => {
    if (message.command === "disableCLI" && sender.tab?.id !== undefined) {
      // Find the managed tab and remove it
      for (const [shortId, tab] of managedTabs) {
        if (tab.tabId === sender.tab.id) {
          disableOnTab(sender.tab.id, shortId);
          break;
        }
      }
    } else if (message.messageId && messageHandlers[message.messageId]) {
      const handler = messageHandlers[message.messageId];
      delete messageHandlers[message.messageId];

      if (message.error) {
        handler.reject(new Error(message.error));
      } else {
        handler.resolve(message.result);
      }
    }
    return true;
  },
);

// Initialize on startup
connectNativeHost();

// No context menu needed - tabs are created programmatically

// Update active tab when user switches tabs
browser.tabs.onActivated.addListener(async (activeInfo) => {
  // Check if this is a managed tab
  for (const [shortId, tab] of managedTabs) {
    if (tab.tabId === activeInfo.tabId) {
      activeTabId = shortId;
      return;
    }
  }
  // Not a managed tab, clear active tab
  activeTabId = undefined;
});

// Clean up when tab is closed
browser.tabs.onRemoved.addListener((tabId) => {
  // Find and remove managed tab
  for (const [shortId, tab] of managedTabs) {
    if (tab.tabId === tabId) {
      managedTabs.delete(shortId);
      if (activeTabId === shortId) {
        activeTabId = undefined;
      }
      break;
    }
  }
});

// Browser action creates a new tab
browser.browserAction.onClicked.addListener(async () => {
  await createNewTab();
});

// Re-inject content script when managed tabs navigate
browser.tabs.onUpdated.addListener(async (tabId, changeInfo, tab) => {
  if (changeInfo.status === "complete") {
    // Check if this is a managed tab
    for (const [shortId, managedTab] of managedTabs) {
      if (managedTab.tabId === tabId) {
        // Update stored info
        managedTab.url = tab.url || managedTab.url;
        managedTab.title = tab.title || managedTab.title;

        // Re-inject content script and banner
        await enableOnTab(tabId, shortId);
        break;
      }
    }
  }
});

/**
 * Enable extension on a specific tab
 * @param {number} tabId - Tab ID
 * @param {string} shortId - Short ID for the tab
 * @returns {Promise<void>}
 */
async function enableOnTab(tabId, shortId) {
  // Inject content script
  await browser.tabs.executeScript(tabId, {
    file: "content.js",
  });

  // Inject banner
  await browser.tabs.executeScript(tabId, {
    code: `(${function (/** @type {string} */ tabId) {
      if (!document.querySelector("#browser-cli-banner")) {
        const banner = document.createElement("div");
        banner.id = "browser-cli-banner";

        // Set banner styles
        Object.assign(banner.style, {
          position: "fixed",
          top: "0",
          left: "0",
          right: "0",
          height: "40px",
          background: "#2563eb",
          color: "white",
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          padding: "0 20px",
          fontFamily: "system-ui, sans-serif",
          fontSize: "14px",
          zIndex: "999999",
          boxShadow: "0 2px 4px rgba(0,0,0,0.1)",
        });

        const text = document.createElement("span");
        text.textContent = "🤖 Browser CLI Tab: " + tabId;
        banner.append(text);

        const closeBtn = document.createElement("button");
        closeBtn.textContent = "✕";

        // Set button styles
        Object.assign(closeBtn.style, {
          background: "none",
          border: "none",
          color: "white",
          fontSize: "20px",
          cursor: "pointer",
          padding: "0",
          width: "30px",
          height: "30px",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          borderRadius: "4px",
          transition: "background 0.2s",
        });

        closeBtn.addEventListener(
          "mouseover",
          () => closeBtn.style.background = "rgba(255,255,255,0.2)",
        );
        closeBtn.addEventListener(
          "mouseout",
          () => closeBtn.style.background = "none",
        );
        closeBtn.addEventListener("click", () => {
          browser.runtime.sendMessage({ command: "disableCLI" });
        });
        banner.append(closeBtn);

        document.body.append(banner);
        document.body.style.paddingTop = "40px";
      }
    }})('${shortId}')`,
  });

  // Ensure native host is connected
  connectNativeHost();
}

/**
 * Disable extension on a specific tab
 * @param {number} tabId - Tab ID
 * @param {string} shortId - Short ID for the tab
 * @returns {Promise<void>}
 */
async function disableOnTab(tabId, shortId) {
  // Remove from managed tabs
  managedTabs.delete(shortId);
  if (activeTabId === shortId) {
    activeTabId = undefined;
  }

  // Remove banner
  await browser.tabs.executeScript(tabId, {
    code: `(${function () {
      const banner = document.querySelector("#browser-cli-banner");
      if (banner) {
        banner.remove();
        document.body.style.paddingTop = "";
      }
    }})()`,
  });
}
