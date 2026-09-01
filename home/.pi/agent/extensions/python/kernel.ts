/** Thin client for driver.py: one long-lived child, JSON lines, serialised requests. */

import { type ChildProcess, spawn } from "node:child_process";
import { dirname, join } from "node:path";
import { createInterface } from "node:readline";
import { fileURLToPath } from "node:url";

const DRIVER = join(dirname(fileURLToPath(import.meta.url)), "driver.py");

export interface ExecResult {
  stdout: string;
  stderr: string;
  result: string | null;
  error: string | null;
  images: string[];
}

export class Kernel {
  private proc: ChildProcess | undefined;
  private pending: ((r: ExecResult) => void) | undefined;
  private stderr = "";
  private queue: Promise<unknown> = Promise.resolve();

  constructor(
    readonly python: string,
    readonly cwd: string,
    /** C extensions or `except KeyboardInterrupt` can swallow SIGINT; kill after this. */
    readonly killAfterMs = 3000,
  ) {}

  private start(): ChildProcess {
    // Own process group so kill() also reaches pexpect/subprocess children.
    const proc = spawn(this.python, ["-u", DRIVER], {
      cwd: this.cwd,
      stdio: ["pipe", "pipe", "pipe"],
      detached: true,
    });
    proc.stderr!.on("data", (d) => {
      this.stderr += d.toString();
    });
    createInterface({ input: proc.stdout! }).on("line", (line) => {
      const done = this.pending;
      this.pending = undefined;
      done?.(JSON.parse(line));
    });
    proc.on("exit", (code, sig) => {
      this.proc = undefined;
      const done = this.pending;
      this.pending = undefined;
      done?.({
        stdout: "",
        stderr: this.stderr,
        result: null,
        error: `Python process exited (${sig ?? code}); state was lost.`,
        images: [],
      });
      this.stderr = "";
    });
    return proc;
  }

  exec(code: string, signal?: AbortSignal): Promise<ExecResult> {
    const job = this.queue.then(() => this.execNow(code, signal));
    this.queue = job.catch(() => {});
    return job;
  }

  private execNow(code: string, signal?: AbortSignal): Promise<ExecResult> {
    this.proc ??= this.start();
    const proc = this.proc;
    this.stderr = "";
    return new Promise<ExecResult>((resolve) => {
      let killTimer: ReturnType<typeof setTimeout> | undefined;
      const onAbort = () => {
        proc.kill("SIGINT");
        killTimer = setTimeout(() => killGroup(proc), this.killAfterMs);
      };
      signal?.addEventListener("abort", onAbort, { once: true });
      this.pending = (r) => {
        signal?.removeEventListener("abort", onAbort);
        clearTimeout(killTimer);
        // fd-level writes (subprocesses, C libs) bypass sys.stderr and land here.
        resolve({ ...r, stderr: r.stderr + this.stderr });
      };
      proc.stdin!.write(`${JSON.stringify({ code })}\n`);
    });
  }

  stop(): void {
    if (this.proc) killGroup(this.proc);
    this.proc = undefined;
  }
}

function killGroup(proc: ChildProcess): void {
  try {
    if (proc.pid) process.kill(-proc.pid, "SIGKILL");
  } catch {
    proc.kill("SIGKILL");
  }
}
