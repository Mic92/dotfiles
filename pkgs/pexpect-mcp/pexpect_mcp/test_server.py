"""Test script for pexpect MCP server."""

import asyncio

from mcp import types

from pexpect_mcp.server import handle_call_tool


async def run_test(code: str, test_timeout: float = 30.0) -> None:
    """Helper to run a test and print results."""
    result = await handle_call_tool(
        "run_pexpect", {"code": code, "timeout": test_timeout}
    )

    # Print all text content from response
    for item in result:
        if isinstance(item, types.TextContent):
            print(item.text)


async def test_pexpect_server() -> None:
    """Test the pexpect server functionality."""
    print("Testing pexpect MCP server...")

    # Test 1: Basic pexpect import and usage
    print("\n1. Testing basic pexpect functionality:")
    await run_test("""
import sys
print(f"pexpect version: {pexpect.__version__}")
print(f"Python version: {sys.version}")
""")

    # Test 2: Creating a child process
    print("\n2. Testing child process creation:")
    await run_test("""
child = pexpect.spawn('echo', ['Hello from pexpect!'])
child.expect(pexpect.EOF)
print(f"Child output: {child.before.decode()}")
print(f"Child is alive: {child.isalive()}")
""")

    # Test 3: Persistent child across calls
    print("\n3. Testing persistent child:")
    await run_test("""
# Start a long-running process
child = pexpect.spawn('python3', ['-c', 'import time; print("Started"); time.sleep(60)'])
child.expect('Started')
print(f"New child PID: {child.pid}")
print(f"Child is alive: {child.isalive()}")
""")

    # Test 4: Access existing child in new call
    print("\n4. Testing access to existing child:")
    await run_test("""
if child:
    print(f"Existing child PID: {child.pid}")
    print(f"Child is still alive: {child.isalive()}")
else:
    print("No child found!")
""")

    # Test 5: Replace child (should kill old one)
    print("\n5. Testing child replacement:")
    await run_test("""
old_pid = child.pid if child else None
child = pexpect.spawn('echo', ['New child process'])
child.expect(pexpect.EOF)
print(f"Old PID: {old_pid}")
print(f"New child output: {child.before.decode()}")
""")

    # Test 6: Test timeout
    print("\n6. Testing timeout (should succeed with 2s timeout):")
    await run_test(
        """
import time
print("Starting sleep...")
time.sleep(1)
print("Finished sleep")
""",
        test_timeout=2.0,
    )

    # Test 7: Test minimum timeout (30s)
    print("\n7. Testing minimum timeout enforcement:")
    await run_test(
        """
print("Testing minimum timeout - this should work even with small timeout value")
""",
        test_timeout=0.1,
    )  # Should be increased to 30s

    # Test 8: Error handling
    print("\n8. Testing error handling:")
    await run_test("""
raise ValueError("Test error")
""")

    # Clean up any remaining child
    print("\n9. Cleaning up:")
    await run_test("""
if child and child.isalive():
    child.terminate(force=True)
    print("Cleaned up remaining child process")
else:
    print("No child to clean up")
""")


if __name__ == "__main__":
    asyncio.run(test_pexpect_server())
