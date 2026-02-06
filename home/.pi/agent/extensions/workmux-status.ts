/**
 * Workmux Status Extension
 *
 * Updates tmux window status indicator via workmux CLI based on agent state:
 * - working: agent is processing (turn in progress)
 * - waiting: permission-gate is prompting user for confirmation
 * - done: agent is idle
 *
 * Listens for permission-gate events via pi.events bus to track the
 * "waiting" state when dangerous commands need user approval.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  const setStatus = async (status: "working" | "waiting" | "done") => {
    try {
      await pi.exec("workmux", ["set-window-status", status], {
        timeout: 5000,
      });
    } catch {
      // Ignore errors - workmux may not be available
    }
  };

  // Agent started processing a user prompt
  pi.on("agent_start", async () => {
    await setStatus("working");
  });

  // Agent finished processing
  pi.on("agent_end", async () => {
    await setStatus("done");
  });

  // Session started - mark as done (idle)
  pi.on("session_start", async () => {
    await setStatus("done");
  });

  // Clean up on shutdown
  pi.on("session_shutdown", async () => {
    await setStatus("done");
  });

  // Permission gate / slow mode prompting user — show waiting state
  pi.events.on("permission-gate:waiting", () => setStatus("waiting"));
  pi.events.on("slow-mode:waiting", () => setStatus("waiting"));

  // Prompt resolved — back to working (agent continues)
  pi.events.on("permission-gate:resolved", () => setStatus("working"));
  pi.events.on("slow-mode:resolved", () => setStatus("working"));
}
