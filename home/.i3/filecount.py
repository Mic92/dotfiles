import glob

from i3pystatus import IntervalModule


class FileCount(IntervalModule):
    """
    """

    format_up = "{name}"
    format_down = "{name}"
    color_up = "#00FF00"
    color_down = "#FF0000"
    settings = (
        "format_up", "format_down",
        "color_up", "color_down",
        "path", "name",
    )
    required = ("path", "name")

    def run(self):
        count = len(glob.glob(self.path))
        if  count > 0:
            fmt = self.format_up
            color = self.color_up
        else:
            fmt = self.format_down
            color = self.color_down

        self.output = {
            "full_text": fmt.format(name=self.name, count=count),
            "color": color,
            "instance": self.name
        }
