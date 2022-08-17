from invoke import task

@task
def deploy(c):
    """
    Deploy to eve, eva and localhost
    """
    c.run("$(nix build --builders '' --print-out-paths .#packages.x86_64-linux.headscale-image) | docker load")
    c.run("docker push registry.fly.io/krebscale:latest")
    c.run("flyctl deploy -i registry.fly.io/krebscale:latest")


@task
def provision(c):
    c.run("flyctl apps create --network fra --name krebscale")
    c.run("flyctl volumes create --region fra --size 1 headscale")
    deploy(c)
