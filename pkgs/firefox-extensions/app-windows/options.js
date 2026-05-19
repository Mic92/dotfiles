// @ts-check
/// <reference path="webext.d.ts" />
/// <reference path="defaults.js" />
"use strict";

const ta = /** @type {HTMLTextAreaElement} */ (document.getElementById("apps"));
const statusEl = /** @type {HTMLElement} */ (document.getElementById("status"));

async function load() {
  const { apps } = await browser.storage.sync.get({ apps: DEFAULT_APPS });
  ta.value = JSON.stringify(apps, null, 2);
}

document.getElementById("save")?.addEventListener("click", async () => {
  try {
    const apps = JSON.parse(ta.value);
    await browser.storage.sync.set({ apps });
    statusEl.textContent = "Saved.";
  } catch (e) {
    statusEl.textContent = "Invalid JSON: " +
      (e instanceof Error ? e.message : String(e));
  }
});

document.getElementById("reset")?.addEventListener("click", async () => {
  await browser.storage.sync.set({ apps: DEFAULT_APPS });
  await load();
  statusEl.textContent = "Reset.";
});

load();
