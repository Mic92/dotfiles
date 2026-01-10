---
name: pexpect-cli
description: Persistent pexpect sessions for automating interactive terminal applications. Use when you need to control interactive programs like ssh or a debugger that requires user input.
---

# Usage

```bash
# Start a new session
pexpect-cli --start
# 888d9bf4

# Spawn bash and run a command, re-using the session id
pexpect-cli 888d9bf4 <<'EOF'
child = pexpect.spawn("bash")
child.sendline("pwd")
child.expect(r"\$")
print(child.before.decode())
EOF
```
