// @ts-check
/// <reference path="webext.d.ts" />
/// <reference path="defaults.js" />
"use strict";

const SESSION_KEY = "appWindowsRole";
const TAB_KEY = "appWindowsApp";

/** @type {number | null} */
let appWindowId = null;
/** Most recently focused non-app window (for routing external links).
 * @type {number | null} */
let lastMainWindowId = null;

/** @returns {Promise<App[]>} */
async function getApps() {
  const { apps } = await browser.storage.sync.get({ apps: DEFAULT_APPS });
  return apps;
}

/** @param {string} url */
function hostnameOf(url) {
  try {
    return new URL(url).hostname;
  } catch {
    return "";
  }
}

/** @param {App[]} apps @param {string} url */
function inScope(apps, url) {
  const host = hostnameOf(url);
  return apps.some((app) =>
    host === app.scope || host.endsWith("." + app.scope)
  );
}

/** Get or create a container for an app. Returns its cookieStoreId or undefined.
 * @param {App} app */
async function containerFor(app) {
  if (!app.color) return undefined;
  const name = `App: ${app.name}`;
  const existing = await browser.contextualIdentities.query({ name });
  if (existing.length > 0) return existing[0].cookieStoreId;
  const created = await browser.contextualIdentities.create({
    name,
    color: app.color,
    icon: app.icon ?? "circle",
  });
  return created.cookieStoreId;
}

/** @type {Promise<void> | null} */
let ensuring = null;

function ensureAppWindow() {
  if (!ensuring) {
    ensuring = ensureAppWindowImpl().finally(() => {
      ensuring = null;
    });
  }
  return ensuring;
}

async function ensureAppWindowImpl() {
  const apps = await getApps();
  if (apps.length === 0) return;

  if (appWindowId !== null) {
    try {
      await browser.windows.get(appWindowId);
    } catch {
      appWindowId = null;
    }
  }

  if (appWindowId === null) {
    const win = await browser.windows.create({ type: "normal" });
    if (win.id === undefined) return;
    appWindowId = win.id;
    await browser.sessions.setWindowValue(win.id, SESSION_KEY, "app");
    await addAppTabs(win.id, apps, new Set());
    // Close the blank tab the new window started with
    const [blank] = await browser.tabs.query({ windowId: win.id, index: 0 });
    if (blank?.id !== undefined && blank.url === "about:blank") {
      await browser.tabs.remove(blank.id).catch(() => {});
    }
    await sortAppTabs(win.id, apps);
    return;
  }

  // Window exists: add tabs for any apps that don't have one yet
  const tabs = await browser.tabs.query({ windowId: appWindowId });
  /** @type {Map<string, number>} */
  const tabByApp = new Map();
  for (const t of tabs) {
    if (t.id === undefined) continue;
    const name = await browser.sessions.getTabValue(t.id, TAB_KEY);
    if (typeof name === "string") tabByApp.set(name, t.id);
  }
  await addAppTabs(appWindowId, apps, new Set(tabByApp.keys()));
  await sortAppTabs(appWindowId, apps);
}

/** Reorder app tabs to match the configured order.
 * @param {number} windowId @param {App[]} apps */
async function sortAppTabs(windowId, apps) {
  const tabs = await browser.tabs.query({ windowId });
  /** @type {Map<string, number>} */
  const tabByApp = new Map();
  for (const t of tabs) {
    if (t.id === undefined) continue;
    const name = await browser.sessions.getTabValue(t.id, TAB_KEY);
    if (typeof name === "string") tabByApp.set(name, t.id);
  }
  let index = 0;
  for (const app of apps) {
    const tabId = tabByApp.get(app.name);
    if (tabId === undefined) continue;
    await browser.tabs.move(tabId, { index }).catch(console.error);
    index++;
  }
}

/** @param {number} windowId @param {App[]} apps @param {Set<string>} skip */
async function addAppTabs(windowId, apps, skip) {
  for (const app of apps) {
    if (skip.has(app.name)) continue;
    try {
      const cookieStoreId = await containerFor(app);
      const tab = await browser.tabs.create({
        windowId,
        url: app.url,
        cookieStoreId,
        pinned: true,
      });
      if (tab.id !== undefined) {
        await browser.sessions.setTabValue(tab.id, TAB_KEY, app.name);
      }
    } catch (e) {
      console.error("app-windows: failed to open", app.name, e);
    }
  }
}

async function focusAppWindow() {
  await ensureAppWindow();
  if (appWindowId !== null) {
    await browser.windows.update(appWindowId, { focused: true });
  }
}

async function findMainWindow() {
  if (lastMainWindowId !== null && lastMainWindowId !== appWindowId) {
    try {
      return await browser.windows.get(lastMainWindowId);
    } catch {
      lastMainWindowId = null;
    }
  }
  const windows = await browser.windows.getAll({
    populate: false,
    windowTypes: ["normal"],
  });
  return windows.find((w) => w.id !== appWindowId) ?? null;
}

/** @param {number} tabId @param {string} url */
async function moveTabToMain(tabId, url) {
  const main = await findMainWindow();
  if (main?.id !== undefined) {
    await browser.tabs.move(tabId, { windowId: main.id, index: -1 });
    await browser.tabs.update(tabId, { active: true });
    await browser.windows.update(main.id, { focused: true });
  } else {
    const win = await browser.windows.create({ url });
    lastMainWindowId = win.id ?? null;
    await browser.tabs.remove(tabId);
  }
}

async function restoreState() {
  const windows = await browser.windows.getAll({ populate: false });
  for (const win of windows) {
    if (win.id === undefined) continue;
    const role = await browser.sessions.getWindowValue(win.id, SESSION_KEY);
    if (role === "app") {
      appWindowId = win.id;
    } else if (win.focused) {
      lastMainWindowId = win.id;
    }
  }
  await ensureAppWindow();
}

// Move out-of-scope tabs created in the app window to the main window.
browser.tabs.onCreated.addListener(async (tab) => {
  if (tab.windowId !== appWindowId || tab.id === undefined) return;
  const apps = await getApps();
  const tabId = tab.id;

  if (tab.url && tab.url !== "about:blank" && tab.url !== "about:newtab") {
    if (!inScope(apps, tab.url)) {
      moveTabToMain(tabId, tab.url).catch(console.error);
    }
    return;
  }

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  let timer;
  const cleanup = () => {
    browser.tabs.onUpdated.removeListener(onUpdated);
    browser.tabs.onRemoved.removeListener(onRemoved);
    clearTimeout(timer);
  };
  /** @type {(tabId: number, changeInfo: TabChangeInfo, tab: BrowserTab) => void} */
  const onUpdated = (updatedTabId, changeInfo) => {
    if (updatedTabId !== tabId || !changeInfo.url) return;
    if (changeInfo.url === "about:blank" || changeInfo.url === "about:newtab") {
      return;
    }
    cleanup();
    if (!inScope(apps, changeInfo.url)) {
      moveTabToMain(tabId, changeInfo.url).catch(console.error);
    }
  };
  /** @type {(removedTabId: number) => void} */
  const onRemoved = (removedTabId) => {
    if (removedTabId === tabId) cleanup();
  };
  browser.tabs.onUpdated.addListener(onUpdated);
  browser.tabs.onRemoved.addListener(onRemoved);
  timer = setTimeout(cleanup, 10000);
});

browser.windows.onFocusChanged.addListener((windowId) => {
  if (windowId === browser.windows.WINDOW_ID_NONE) return;
  if (windowId !== appWindowId) lastMainWindowId = windowId;
});

browser.windows.onRemoved.addListener((windowId) => {
  if (appWindowId === windowId) appWindowId = null;
  if (lastMainWindowId === windowId) lastMainWindowId = null;
});

browser.browserAction.onClicked.addListener(() => {
  focusAppWindow().catch(console.error);
});

restoreState().catch(console.error);
