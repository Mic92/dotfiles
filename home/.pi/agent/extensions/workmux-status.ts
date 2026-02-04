/**
 * Workmux Status Extension
 *
 * Updates tmux window status indicator via workmux CLI based on agent state:
 * - working: agent is processing (turn in progress)
 * - done: agent is idle
 *
 * Note: The OpenCode "waiting" state (permission requested) doesn't have a
 * direct equivalent in pi's event model. Permission dialogs happen within
 * tool_call handlers, so tracking them would require wrapping specific tools.
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
}
