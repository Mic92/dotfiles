// Keeps lastChangelogVersion stable to avoid noise in dotfiles git history
// See: https://github.com/badlogic/pi-mono/issues/720
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { readFileSync, writeFileSync } from "fs";
import { join } from "path";
import { homedir } from "os";

export default function (pi: ExtensionAPI) {
  pi.on("session_start", async () => {
    const settingsPath = join(homedir(), ".pi", "agent", "settings.json");
    try {
      const settings = JSON.parse(readFileSync(settingsPath, "utf-8"));
      settings.lastChangelogVersion = "99.99.99";
      writeFileSync(settingsPath, JSON.stringify(settings, null, 2), "utf-8");
    } catch {
      // ignore errors
    }
  });
}
