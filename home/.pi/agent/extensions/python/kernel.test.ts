/** Run with: bun test python (needs python3) */
import { afterAll, describe, expect, test } from "bun:test";
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";
import { Kernel } from "./kernel.ts";

const python = process.env.PI_PYTHON ?? "python3";
const hasPython = (() => {
  try {
    execFileSync(python, ["--version"]);
    return true;
  } catch {
    return false;
  }
})();

describe.skipIf(!hasPython)("python kernel", () => {
  const k = new Kernel(python, tmpdir());
  afterAll(() => k.stop());

  test("state persists and trailing expression is echoed", async () => {
    await k.exec("import json\nx = 20");
    const r = await k.exec("print('hi')\nx + 22");
    expect(r).toEqual({
      stdout: "hi\n",
      stderr: "",
      result: "42",
      error: null,
      images: [],
    });
    expect((await k.exec("_ * 2")).result).toBe("84");
  });

  test("exceptions come back as tracebacks starting at the cell", async () => {
    const r = await k.exec("def f():\n  raise ValueError('boom')\nf()");
    expect(r.error).toContain("ValueError: boom");
    expect(r.error).toContain('File "<cell>", line 2, in f');
    expect(r.error).not.toContain("driver.py");
    expect((await k.exec("x")).result).toBe("20");
  });

  test("syntax errors do not kill the session", async () => {
    expect((await k.exec("def")).error).toContain("SyntaxError");
    expect((await k.exec("x")).result).toBe("20");
  });

  test("subprocess output cannot corrupt the protocol stream", async () => {
    const r = await k.exec(
      "import subprocess; subprocess.run(['echo', 'raw'], check=True).returncode",
    );
    expect(r.result).toBe("0");
    expect(r.stderr).toContain("raw");
  });

  test("abort interrupts running code but keeps state", async () => {
    const ac = new AbortController();
    const p = k.exec(
      "import time\nfor i in range(100): time.sleep(0.05)",
      ac.signal,
    );
    setTimeout(() => ac.abort(), 100);
    expect((await p).error).toContain("KeyboardInterrupt");
    expect((await k.exec("x")).result).toBe("20");
  });

  test("abort escalates to kill when SIGINT is swallowed", async () => {
    const k2 = new Kernel(python, tmpdir(), 200);
    const ac = new AbortController();
    const p = k2.exec(
      "import time\nwhile True:\n  try: time.sleep(1)\n  except KeyboardInterrupt: pass",
      ac.signal,
    );
    setTimeout(() => ac.abort(), 100);
    expect((await p).error).toContain("state was lost");
    expect((await k2.exec("1")).result).toBe("1");
    k2.stop();
  });

  test("concurrent calls are serialised", async () => {
    const [a, b] = await Promise.all([
      k.exec("import time; time.sleep(0.1); y = 1"),
      k.exec("y"),
    ]);
    expect(a.error).toBeNull();
    expect(b.result).toBe("1");
  });

  test("crash is reported and next call starts a fresh interpreter", async () => {
    const r = await k.exec("import os; os._exit(3)");
    expect(r.error).toContain("exited (3)");
    expect((await k.exec("'x' in globals()")).result).toBe("False");
  });

  const hasMpl = hasPython && (() => {
    try {
      execFileSync(python, ["-c", "import matplotlib"]);
      return true;
    } catch {
      return false;
    }
  })();
  test.skipIf(!hasMpl)("matplotlib figures are returned as png", async () => {
    const r = await k.exec(
      "import matplotlib.pyplot as plt\nplt.plot([1,2,3])\nplt.title('t')",
    );
    expect(r.error).toBeNull();
    expect(r.images).toHaveLength(1);
    expect(Buffer.from(r.images[0], "base64").subarray(1, 4).toString()).toBe(
      "PNG",
    );
    expect((await k.exec("len(plt.get_fignums())")).result).toBe("0");
  });
});
