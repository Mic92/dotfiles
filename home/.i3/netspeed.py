from i3pystatus import IntervalModule

class Netspeed(IntervalModule):
    """
    """

    format_up = "{name}: {up}/{down}"
    format_down = "{name}"
    color_up = "#00FF00"
    color_down = "#FF0000"
    settings = (
        "format_up", "format_down",
        "color_up", "color_down",
        "path", "name",
    )
    required = ("name")
#>>> import re
#>>> m = re.search('(?<=abc)def', 'abcdef')
#>>> m.group(0)
#'def'

    def run(self):
        lines = open("/proc/net/dev").readlines()
        _, receiveCols , transmitCols = line[1].split("|")
        receiveCols = map(lambda a:"recv_"+a, receiveCols.split())
        transmitCols = map(lambda a:"trans_"+a, transmitCols.split())
        cols = receiveCols+transmitCols
        faces = {}
        for line in lines[2:]:
            if line.find(":") < 0: continue
            face, data = line.split(":")
            faceData = dict(zip(cols, data.split()))
            faces[face] = faceData

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

#    -- Get NET stats
#    for line in io.lines("/proc/net/dev") do
#        -- Match wmaster0 as well as rt0 (multiple leading spaces)
#        local name = string.match(line, "^[%s]?[%s]?[%s]?[%s]?([%w]+):")
#        if name ~= nil then
#            -- Received bytes, first value after the name
#            local recv = tonumber(string.match(line, ":[%s]*([%d]+)"))
#            -- Transmited bytes, 7 fields from end of the line
#            local send = tonumber(string.match(line,
#             "([%d]+)%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d$"))
#
#            helpers.uformat(args, name .. " rx", recv, unit)
#            helpers.uformat(args, name .. " tx", send, unit)
#
#            -- Operational state and carrier detection
#            local sysnet = helpers.pathtotable("/sys/class/net/" .. name)
#            args["{"..name.." carrier}"] = tonumber(sysnet.carrier) or 0
#
#            local now = os.time()
#            if nets[name] == nil then
#                -- Default values on the first run
#                nets[name] = {}
#                helpers.uformat(args, name .. " down", 0, unit)
#                helpers.uformat(args, name .. " up",   0, unit)
#            else -- Net stats are absolute, substract our last reading
#                local interval = now - nets[name].time
#                if interval <= 0 then interval = 1 end
#
#                local down = (recv - nets[name][1]) / interval
#                local up   = (send - nets[name][2]) / interval
#
#                helpers.uformat(args, name .. " down", down, unit)
#                helpers.uformat(args, name .. " up",   up,   unit)
#            end
#
#            nets[name].time = now
#
#            -- Store totals
#            nets[name][1] = recv
#            nets[name][2] = send
#        end
#    end
