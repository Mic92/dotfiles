/**
 * Git Rebase Environment Extension
 *
 * Sets environment variables to allow non-interactive git rebase:
 * - GIT_EDITOR=cat: accepts commit message as-is, prints it so the LLM sees it
 * - GIT_SEQUENCE_EDITOR=cat: accepts default rebase sequence, prints it so the LLM sees the plan
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (_pi: ExtensionAPI) {
  process.env.GIT_EDITOR = "cat";
  process.env.GIT_SEQUENCE_EDITOR = "cat";
}
