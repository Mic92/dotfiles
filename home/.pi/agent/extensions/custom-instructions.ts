/**
 * Custom Instructions Extension - appends CLAUDE.md to system prompt
 */

import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.on("before_agent_start", async (event, _ctx) => {
    const home = process.env.HOME || "";
    const instructionsPath = join(home, ".claude", "CLAUDE.md");

    if (existsSync(instructionsPath)) {
      const instructions = readFileSync(instructionsPath, "utf-8");
      return {
        systemPrompt: event.systemPrompt + "\n\n" + instructions,
      };
    }
  });
}
