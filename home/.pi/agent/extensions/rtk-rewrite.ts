/**
 * RTK Rewrite Extension for Pi
 *
 * Based on https://github.com/sherif-fanous/pi-rtk
 * Adopted from https://git.sr.ht/~r-vdp/nixos-config/tree/main/item/pi-extensions/rtk-rewrite.ts
 *
 * Rewrites bash commands to their rtk equivalents via `rtk rewrite`.
 * Uses the tool_call event instead of registerTool (which can only have
 * one owner) so it composes with other bash tool overrides.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { isToolCallEventType } from "@mariozechner/pi-coding-agent";
import { execFileSync } from "node:child_process";

export default function (pi: ExtensionAPI) {
  let rtkAvailable: boolean | null = null;

  function checkRtk(): boolean {
    if (rtkAvailable !== null) return rtkAvailable;
    try {
      execFileSync("rtk", ["--version"], { stdio: "ignore" });
      rtkAvailable = true;
    } catch {
      rtkAvailable = false;
    }
    return rtkAvailable;
  }

  if (!checkRtk()) {
    pi.on("session_start", async (_event, ctx) => {
      ctx.ui.notify(
        "rtk-rewrite: `rtk` not found on PATH — extension disabled.",
        "warning",
      );
    });
    return;
  }

  // TODO: re-assess once rtk grep gains full rg flag support
  // rtk grep does not support all rg flags (e.g. --type, --count-matches),
  // so skip rewriting for commands containing rg/grep as a subcommand
  // anywhere in the pipeline/chain (after &&, ||, |, ;, or at the start).
  const skipRewritePattern = /(?:^|[;&|]\s*)(rg|grep)\s/;

  pi.on("tool_call", async (event, _ctx) => {
    if (!isToolCallEventType("bash", event)) return;

    const command = event.input.command;
    if (typeof command !== "string" || command.length === 0) return;
    if (skipRewritePattern.test(command)) return;

    try {
      const rewritten = execFileSync("rtk", ["rewrite", command], {
        encoding: "utf-8",
        timeout: 5000,
      }).trimEnd();

      if (rewritten && rewritten !== command) {
        event.input.command = rewritten;
      }
    } catch {
      // rtk rewrite exits non-zero for unsupported commands — keep original
    }
  });
}
