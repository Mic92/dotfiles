/// <reference types="firefox-webext-browser" />

/**
 * Background script for Browser CLI Controller
 * Manages native messaging connection and message routing
 *
 * @typedef {object} NativeMessage
 * @property {boolean} [ready] - Whether the native messaging bridge is ready
 * @property {string} [socket_path] - Path to the Unix socket
 * @property {string} [command] - The command to execute
 * @property {CommandParams} [params] - Command parameters
 * @property {string} [id] - Unique message ID
 * @property {string} [tabId] - Target tab ID for the command
 *
 * @typedef {object} Message
 * @property {string} command - The command to execute
 * @property {CommandParams} params - Command parameters
 * @property {string} id - Unique message ID
 * @property {string} [tabId] - Target tab ID for the command
 *
 * @typedef {object} CommandParams
 * @property {string} [url] - URL for navigation or new tab
 * @property {string} [tabId] - Tab ID for close-tab
 * @property {string} [code] - JavaScript code to execute
 * @property {string} [output_path] - Output path for screenshot
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
 * @returns {string}
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
 */
function connectNativeHost() {
  if (nativePort) return;

  console.log("Connecting to native messaging host...");

  try {
    nativePort = browser.runtime.connectNative(
      "io.thalheim.browser_cli.bridge",
    );

    nativePort.onMessage.addListener(async (msg) => {
      const message = /** @type {NativeMessage} */ (msg);
      console.log("Received from native host:", message);

      if (message.ready && message.socket_path) {
        console.log(
          "Native messaging bridge ready, socket at:",
          message.socket_path,
        );
        return;
      }

      if (message.command) {
        await handleCommand(/** @type {Message} */ (message));
      }
    });

    nativePort.onDisconnect.addListener(() => {
      console.log("Native messaging disconnected");
      if (browser.runtime.lastError) {
        console.error("Native messaging error:", browser.runtime.lastError);
      }
      nativePort = undefined;
      setTimeout(connectNativeHost, 5000);
    });
  } catch (error) {
    console.error("Failed to connect to native messaging host:", error);
    nativePort = undefined;
  }
}

/**
 * Send response back to native host
 * @param {object} response
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
 * Get the browser tab ID to use for a command
 * @param {string} [targetTabId]
 * @returns {Promise<number>}
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
      "No active tab. Create a tab first with newTab()",
    );
  }
}

/**
 * Send command to content script
 * @param {string} command
 * @param {object} [params={}]
 * @param {string} [targetTabId]
 * @returns {Promise<object>}
 */
async function sendToContentScript(command, params = {}, targetTabId) {
  const tabId = await getTargetTab(targetTabId);

  return new Promise((resolve, reject) => {
    const messageId = Date.now().toString();
    messageHandlers[messageId] = { resolve, reject };

    browser.tabs.sendMessage(tabId, { command, params, messageId });

    setTimeout(() => {
      if (messageHandlers[messageId]) {
        delete messageHandlers[messageId];
        reject(new Error("Content script timeout"));
      }
    }, 30_000);
  });
}

// ============================================================================
// Browser-level commands (require background script)
// ============================================================================

/**
 * Navigate to URL
 * @param {string} url
 * @param {string} [tabId]
 * @returns {Promise<{message: string}>}
 */
async function navigate(url, tabId) {
  const browserTabId = await getTargetTab(tabId);
  await browser.tabs.update(browserTabId, { url });

  const managedTabId = tabId || activeTabId;
  if (managedTabId && managedTabs.has(managedTabId)) {
    const managedTab = managedTabs.get(managedTabId);
    if (managedTab) managedTab.url = url;
  }

  return { message: `Navigated to ${url}` };
}

/**
 * Navigate back
 * @param {string} [tabId]
 * @returns {Promise<{message: string}>}
 */
async function goBack(tabId) {
  const browserTabId = await getTargetTab(tabId);
  await browser.tabs.goBack(browserTabId);
  return { message: "Navigated back" };
}

/**
 * Navigate forward
 * @param {string} [tabId]
 * @returns {Promise<{message: string}>}
 */
async function goForward(tabId) {
  const browserTabId = await getTargetTab(tabId);
  await browser.tabs.goForward(browserTabId);
  return { message: "Navigated forward" };
}

/**
 * Take a screenshot
 * @param {string} [tabId]
 * @returns {Promise<{screenshot: string}>}
 */
async function takeScreenshot(tabId) {
  const browserTabId = await getTargetTab(tabId);
  await browser.tabs.update(browserTabId, { active: true });
  const dataUrl = await browser.tabs.captureVisibleTab();
  return { screenshot: dataUrl };
}

/**
 * List all managed tabs
 * @returns {Promise<{tabs: Array<{id: string, tabId: number, url: string, title: string, active: boolean}>}>}
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
      const browserTab = await browser.tabs.get(tab.tabId);
      tabs.push({
        id: shortId,
        tabId: tab.tabId,
        url: browserTab.url || "",
        title: browserTab.title || "Untitled",
        active: browserTab.id === activeBrowserTabId,
      });
    } catch {
      managedTabs.delete(shortId);
      if (activeTabId === shortId) activeTabId = undefined;
    }
  }

  return { tabs };
}

/**
 * Create a new managed tab
 * @param {string} [url]
 * @returns {Promise<{tabId: string, url: string}>}
 */
async function createNewTab(url) {
  const shortId = generateTabId();
  const tabUrl = url || "about:blank";

  const tab = await browser.tabs.create({ url: tabUrl, active: true });

  if (tab.id === undefined) {
    throw new Error("Failed to create tab");
  }

  managedTabs.set(shortId, {
    tabId: tab.id,
    url: tabUrl,
    title: tab.title || "New Tab",
  });

  activeTabId = shortId;
  await enableOnTab(tab.id, shortId);

  return { tabId: shortId, url: tabUrl };
}

/**
 * Close a managed tab
 * @param {string} [tabId]
 * @returns {Promise<{message: string}>}
 */
async function closeTab(tabId) {
  const targetId = tabId || activeTabId;
  if (!targetId) {
    throw new Error("No tab to close");
  }

  const managedTab = managedTabs.get(targetId);
  if (!managedTab) {
    throw new Error(`Tab ${targetId} not found`);
  }

  await browser.tabs.remove(managedTab.tabId);
  managedTabs.delete(targetId);

  if (activeTabId === targetId) {
    activeTabId = managedTabs.keys().next().value;
  }

  return { message: `Closed tab ${targetId}` };
}

// ============================================================================
// Command handling
// ============================================================================

/**
 * Handle commands from CLI
 * @param {Message} message
 */
async function handleCommand(message) {
  const { command, params, id, tabId } = message;

  try {
    let result;

    switch (command) {
      // Browser-level commands
      case "navigate": {
        result = await navigate(params.url || "", tabId);
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
      case "screenshot": {
        result = await takeScreenshot(tabId);
        break;
      }
      case "list-tabs": {
        result = await listTabs();
        break;
      }
      case "new-tab": {
        result = await createNewTab(params.url);
        break;
      }
      case "close-tab": {
        result = await closeTab(params.tabId || tabId);
        break;
      }

      // Execute JavaScript in content script
      case "exec": {
        result = await sendToContentScript("exec", params, tabId);
        break;
      }

      default: {
        throw new Error(`Unknown command: ${command}`);
      }
    }

    sendResponse({ id, result, success: true });
  } catch (error) {
    sendResponse({
      id,
      error: error instanceof Error ? error.message : String(error),
      success: false,
    });
  }
}

// ============================================================================
// Tab management
// ============================================================================

/**
 * Enable extension on a specific tab
 * @param {number} tabId
 * @param {string} shortId
 */
async function enableOnTab(tabId, shortId) {
  await browser.tabs.executeScript(tabId, { file: "content.js" });

  await browser.tabs.executeScript(tabId, {
    code: `(${function (/** @type {string} */ id) {
      if (!document.querySelector("#browser-cli-banner")) {
        const banner = document.createElement("div");
        banner.id = "browser-cli-banner";
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
        text.textContent = "🤖 Browser CLI: " + id;
        banner.append(text);

        const closeBtn = document.createElement("button");
        closeBtn.textContent = "✕";
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

  connectNativeHost();
}

/**
 * Disable extension on a specific tab
 * @param {number} tabId
 * @param {string} shortId
 */
async function disableOnTab(tabId, shortId) {
  managedTabs.delete(shortId);
  if (activeTabId === shortId) activeTabId = undefined;

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

// ============================================================================
// Event listeners
// ============================================================================

browser.runtime.onMessage.addListener((message, sender, _sendResponse) => {
  // Handle disable CLI request
  if (message.command === "disableCLI" && sender.tab?.id !== undefined) {
    for (const [shortId, tab] of managedTabs) {
      if (tab.tabId === sender.tab.id) {
        disableOnTab(sender.tab.id, shortId);
        break;
      }
    }
    return true;
  }

  // Handle content script responses
  if (message.messageId && messageHandlers[message.messageId]) {
    const handler = messageHandlers[message.messageId];
    delete messageHandlers[message.messageId];

    if (message.error) {
      handler.reject(new Error(message.error));
    } else {
      handler.resolve(message.result);
    }
    return true;
  }

  // Handle content script requests for browser-level commands
  if (message.bgMessageId) {
    const { command, params, bgMessageId } = message;

    // Find the tab ID for this sender
    let senderTabShortId;
    if (sender.tab?.id !== undefined) {
      for (const [shortId, tab] of managedTabs) {
        if (tab.tabId === sender.tab.id) {
          senderTabShortId = shortId;
          break;
        }
      }
    }

    (async () => {
      try {
        let result;

        switch (command) {
          case "navigate": {
            result = await navigate(params.url, senderTabShortId);
            break;
          }
          case "back": {
            result = await goBack(senderTabShortId);
            break;
          }
          case "forward": {
            result = await goForward(senderTabShortId);
            break;
          }
          case "screenshot": {
            result = await takeScreenshot(senderTabShortId);
            // Save screenshot to file if path provided
            if (params.output_path && result.screenshot) {
              // Send back to CLI via native messaging for file save
              // For now, just return the data URL
            }
            break;
          }
          case "new-tab": {
            result = await createNewTab(params.url);
            break;
          }
          case "close-tab": {
            result = await closeTab(params.tabId || senderTabShortId);
            break;
          }
          case "list-tabs": {
            result = await listTabs();
            break;
          }
          default: {
            throw new Error(`Unknown background command: ${command}`);
          }
        }

        // Send response back to content script
        if (sender.tab?.id !== undefined) {
          browser.tabs.sendMessage(sender.tab.id, {
            bgMessageId,
            result,
          });
        }
      } catch (error) {
        if (sender.tab?.id !== undefined) {
          browser.tabs.sendMessage(sender.tab.id, {
            bgMessageId,
            error: error instanceof Error ? error.message : String(error),
          });
        }
      }
    })();

    return true;
  }

  return true;
});

browser.tabs.onActivated.addListener(async (activeInfo) => {
  for (const [shortId, tab] of managedTabs) {
    if (tab.tabId === activeInfo.tabId) {
      activeTabId = shortId;
      return;
    }
  }
  activeTabId = undefined;
});

browser.tabs.onRemoved.addListener((tabId) => {
  for (const [shortId, tab] of managedTabs) {
    if (tab.tabId === tabId) {
      managedTabs.delete(shortId);
      if (activeTabId === shortId) activeTabId = undefined;
      break;
    }
  }
});

browser.browserAction.onClicked.addListener(async () => {
  await createNewTab();
});

browser.tabs.onUpdated.addListener(async (tabId, changeInfo, tab) => {
  if (changeInfo.status === "complete") {
    for (const [shortId, managedTab] of managedTabs) {
      if (managedTab.tabId === tabId) {
        managedTab.url = tab.url || managedTab.url;
        managedTab.title = tab.title || managedTab.title;
        await enableOnTab(tabId, shortId);
        break;
      }
    }
  }
});

// Initialize
connectNativeHost();
