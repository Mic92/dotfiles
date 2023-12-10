import subprocess


class BitwardenPassword:
    def __init__(self, name: str) -> None:
        self.password: str | None = None
        self.name = name

    def get(self) -> str:
        if not self.password:
            cmd = ["rbw", "get", self.name]
            res = subprocess.run(cmd, check=True, capture_output=True)
            self.password = res.stdout.decode("utf-8").strip()
        return self.password
