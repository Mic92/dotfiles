from libqtile.command.client import InteractiveCommandClient


def main() -> None:
    c = InteractiveCommandClient()
    for name, group in c.get_groups().items():
        for window in group["windows"]:
            print(c.group[name].info_by_name(window))

if __name__ == "__main__":
    main()
