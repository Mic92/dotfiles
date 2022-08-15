from invoke import task

@task
def deploy(c):
    """
    Deploy to eve, eva and localhost
    """
    c.run("$(nix build --builders '' --print-out-paths .#packages.x86_64-linux.headscale-image) | docker load")
    c.run("docker push registry.fly.io/headscale-mic92:latest")
    c.run("flyctl deploy --remote-only -i registry.fly.io/headscale-mic92:latest")
