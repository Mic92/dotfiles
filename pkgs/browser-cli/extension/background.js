/// <reference types="firefox-webext-browser" />

/**
 * Background script for Browser CLI Controller
 * Manages WebSocket connection and message routing
 * @typedef {object} Message
 * @property {string} command - The command to execute
 * @property {object} params - Command parameters
 * @property {string} id - Unique message ID
 *
 * @typedef {object} CommandResponse
 * @property {string} id - Message ID
 * @property {boolean} success - Whether command succeeded
 * @property {object} [result] - Command result
 * @property {string} [error] - Error message if failed
 */

/** @type {WebSocket|undefined} */
let ws;
/** @type {string} */
const SERVER_URL = "ws://localhost:9222";

/** @type {Record<string, {resolve: Function, reject: Function}>} Message handlers from content scripts */
const messageHandlers = {};

/** @type {Set<number>} Set of tab IDs where extension is enabled */
const enabledTabs = new Set();

/**
 * Connect to WebSocket server
 * @returns {void}
 */
function connectWebSocket() {
  if (ws && ws.readyState === WebSocket.OPEN) {
    return;
  }

  console.log("Connecting to WebSocket server...");
  ws = new WebSocket(SERVER_URL);

  ws.addEventListener("open", () => {
    console.log("Connected to WebSocket server");
  });

  ws.addEventListener("message", async (event) => {
    try {
      const message = JSON.parse(event.data);
      console.log("Received message:", message);

      if (message.command) {
        await handleCommand(message);
      }
    } catch (error) {
      console.error("Error processing message:", error);
    }
  });

  ws.addEventListener("error", (error) => {
    console.error("WebSocket error:", error);
  });

  ws.addEventListener("close", () => {
    console.log("WebSocket connection closed");
    ws = undefined;
    // Try to reconnect after 5 seconds
    setTimeout(connectWebSocket, 5000);
  });
}

/**
 * Handle commands from CLI
 * @param {Message} message - The command message
 * @returns {Promise<void>}
 */
async function handleCommand(message) {
  const { command, params, id } = message;

  /** @type {{url?: string, element?: string, text?: string, startElement?: string, endElement?: string, option?: string, seconds?: number, key?: string}} */
  const typedParams = params;

  try {
    let result;

    switch (command) {
      case "navigate": {
        result = await navigate(typedParams.url || "");
        break;
      }

      case "back": {
        result = await goBack();
        break;
      }

      case "forward": {
        result = await goForward();
        break;
      }

      case "click": {
        result = await clickElement(typedParams.element || "");
        break;
      }

      case "type": {
        result = await typeText(
          typedParams.element || "",
          typedParams.text || "",
        );
        break;
      }

      case "hover": {
        result = await hoverElement(typedParams.element || "");
        break;
      }

      case "drag": {
        result = await dragElement(
          typedParams.startElement || "",
          typedParams.endElement || "",
        );
        break;
      }

      case "select": {
        result = await selectOption(
          typedParams.element || "",
          typedParams.option || "",
        );
        break;
      }

      case "wait": {
        result = await wait(typedParams.seconds || 0);
        break;
      }

      case "key": {
        result = await pressKey(typedParams.key || "");
        break;
      }

      case "screenshot": {
        result = await takeScreenshot();
        break;
      }

      case "console": {
        result = await getConsoleLogs();
        break;
      }

      case "snapshot": {
        result = await getSnapshot();
        break;
      }

      default: {
        throw new Error(`Unknown command: ${command}`);
      }
    }

    // Send response
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({
        id,
        result,
        success: true,
      }));
    }
  } catch (error) {
    // Send error response
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({
        id,
        error: error instanceof Error ? error.message : String(error),
        success: false,
      }));
    }
  }
}

/**
 * Browser command implementations
 * @param {string} url - URL to navigate to
 * @returns {Promise<{message: string}>}
 */
async function navigate(url) {
  const [activeTab] = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });
  if (activeTab.id !== undefined) {
    await browser.tabs.update(activeTab.id, { url });
  }
  return { message: `Navigated to ${url}` };
}

/**
 * Navigate back in browser history
 * @returns {Promise<{message: string}>}
 */
async function goBack() {
  const [activeTab] = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });
  await browser.tabs.goBack(activeTab.id);
  return { message: "Navigated back" };
}

/**
 * Navigate forward in browser history
 * @returns {Promise<{message: string}>}
 */
async function goForward() {
  const [activeTab] = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });
  await browser.tabs.goForward(activeTab.id);
  return { message: "Navigated forward" };
}

/**
 * Send command to content script
 * @param {string} command - Command name
 * @param {object} [params={}] - Command parameters
 * @returns {Promise<object>} Response from content script
 */
async function sendToContentScript(command, params = {}) {
  const [activeTab] = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });

  if (activeTab.id === undefined || !enabledTabs.has(activeTab.id)) {
    throw new Error("Browser CLI is not enabled on this tab");
  }

  return new Promise((resolve, reject) => {
    const messageId = Date.now().toString();

    messageHandlers[messageId] = { resolve, reject };

    if (activeTab.id === undefined) {
      reject(new Error("No active tab ID"));
      return;
    }

    browser.tabs.sendMessage(activeTab.id, {
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
 * @returns {Promise<object>}
 */
async function clickElement(selector) {
  return sendToContentScript("click", { selector });
}

/**
 * Type text into an element
 * @param {string} selector - Element selector
 * @param {string} text - Text to type
 * @returns {Promise<object>}
 */
async function typeText(selector, text) {
  return sendToContentScript("type", { selector, text });
}

/**
 * Hover over an element
 * @param {string} selector - Element selector
 * @returns {Promise<object>}
 */
async function hoverElement(selector) {
  return sendToContentScript("hover", { selector });
}

/**
 * Drag from one element to another
 * @param {string} startSelector - Start element selector
 * @param {string} endSelector - End element selector
 * @returns {Promise<object>}
 */
async function dragElement(startSelector, endSelector) {
  return sendToContentScript("drag", { startSelector, endSelector });
}

/**
 * Select an option in a dropdown
 * @param {string} selector - Select element selector
 * @param {string} option - Option to select
 * @returns {Promise<object>}
 */
async function selectOption(selector, option) {
  return sendToContentScript("select", { selector, option });
}

/**
 * Wait for specified time
 * @param {number} seconds - Seconds to wait
 * @returns {Promise<{message: string}>}
 */
async function wait(seconds) {
  await new Promise((resolve) => setTimeout(resolve, seconds * 1000));
  return { message: `Waited ${seconds} seconds` };
}

/**
 * Press a keyboard key
 * @param {string} key - Key to press
 * @returns {Promise<object>}
 */
async function pressKey(key) {
  return sendToContentScript("key", { key });
}

/**
 * Take a screenshot of the current tab
 * @returns {Promise<{screenshot: string}>} Data URL of screenshot
 */
async function takeScreenshot() {
  await browser.tabs.query({ active: true, currentWindow: true });
  const dataUrl = await browser.tabs.captureVisibleTab();
  return { screenshot: dataUrl };
}

/**
 * Get console logs from the page
 * @returns {Promise<object>}
 */
async function getConsoleLogs() {
  return sendToContentScript("getConsole");
}

/**
 * Get ARIA snapshot of the page
 * @returns {Promise<object>}
 */
async function getSnapshot() {
  return sendToContentScript("getSnapshot");
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
      disableOnTab(sender.tab.id);
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

/**
 * Start server and connect
 * @returns {Promise<void>}
 */
async function startServer() {
  // Try to start the bridge server via native messaging
  try {
    const port = browser.runtime.connectNative(
      "io.thalheim.browser_cli.bridge",
    );

    port.onMessage.addListener(
      /** @param {any} message */
      (message) => {
        console.log("Native messaging response:", message);
      },
    );

    port.onDisconnect.addListener(() => {
      console.log("Native messaging disconnected");
      if (browser.runtime.lastError) {
        console.error("Native messaging error:", browser.runtime.lastError);
      }
    });

    // Send a startup message
    port.postMessage({ command: "start" });
  } catch (error) {
    console.error("Failed to connect to native messaging host:", error);
  }

  // Try to connect to WebSocket after a delay
  setTimeout(connectWebSocket, 1000);
}

// Initialize on startup
startServer();

// Create context menu
browser.contextMenus.create({
  id: "toggle-browser-cli",
  title: "Enable Browser CLI on this tab",
  contexts: ["page"],
});

// Handle context menu clicks
browser.contextMenus.onClicked.addListener(async (info, tab) => {
  if (info.menuItemId === "toggle-browser-cli" && tab?.id !== undefined) {
    await (enabledTabs.has(tab.id)
      ? disableOnTab(tab.id)
      : enableOnTab(tab.id));
  }
});

// Update context menu when tab changes
browser.tabs.onActivated.addListener(async (activeInfo) => {
  const enabled = enabledTabs.has(activeInfo.tabId);
  browser.contextMenus.update("toggle-browser-cli", {
    title: enabled
      ? "Disable Browser CLI on this tab"
      : "Enable Browser CLI on this tab",
  });
});

// Clean up when tab is closed
browser.tabs.onRemoved.addListener((tabId) => {
  enabledTabs.delete(tabId);
});

// Reconnect on browser action click
browser.browserAction.onClicked.addListener(async () => {
  const [activeTab] = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });
  if (activeTab.id !== undefined) {
    await (enabledTabs.has(activeTab.id)
      ? disableOnTab(activeTab.id)
      : enableOnTab(activeTab.id));
  }
});

/**
 * Enable extension on a specific tab
 * @param {number} tabId - Tab ID
 * @returns {Promise<void>}
 */
async function enableOnTab(tabId) {
  enabledTabs.add(tabId);

  // Inject content script
  await browser.tabs.executeScript(tabId, {
    file: "content.js",
  });

  // Inject banner
  await browser.tabs.executeScript(tabId, {
    code: `
      if (!document.getElementById('browser-cli-banner')) {
        const banner = document.createElement('div');
        banner.id = 'browser-cli-banner';
        banner.style.cssText = '
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          height: 40px;
          background: #2563eb;
          color: white;
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding: 0 20px;
          font-family: system-ui, sans-serif;
          font-size: 14px;
          z-index: 999999;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        ';
        
        const text = document.createElement('span');
        text.textContent = '🤖 Browser CLI enabled on this tab';
        banner.appendChild(text);
        
        const closeBtn = document.createElement('button');
        closeBtn.textContent = '✕';
        closeBtn.style.cssText = '
          background: none;
          border: none;
          color: white;
          font-size: 20px;
          cursor: pointer;
          padding: 0;
          width: 30px;
          height: 30px;
          display: flex;
          align-items: center;
          justify-content: center;
          border-radius: 4px;
          transition: background 0.2s;
        ';
        closeBtn.onmouseover = () => closeBtn.style.background = 'rgba(255,255,255,0.2)';
        closeBtn.onmouseout = () => closeBtn.style.background = 'none';
        closeBtn.onclick = () => {
          browser.runtime.sendMessage({ command: 'disableCLI' });
        };
        banner.appendChild(closeBtn);
        
        document.body.appendChild(banner);
        document.body.style.paddingTop = '40px';
      }
    `,
  });

  // Update context menu
  browser.contextMenus.update("toggle-browser-cli", {
    title: "Disable Browser CLI on this tab",
  });

  connectWebSocket();
}

/**
 * Disable extension on a specific tab
 * @param {number} tabId - Tab ID
 * @returns {Promise<void>}
 */
async function disableOnTab(tabId) {
  enabledTabs.delete(tabId);

  // Remove banner
  await browser.tabs.executeScript(tabId, {
    code: `
      const banner = document.getElementById('browser-cli-banner');
      if (banner) {
        banner.remove();
        document.body.style.paddingTop = '';
      }
    `,
  });

  // Update context menu
  browser.contextMenus.update("toggle-browser-cli", {
    title: "Enable Browser CLI on this tab",
  });
}
