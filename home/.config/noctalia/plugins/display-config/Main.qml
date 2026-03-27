import QtQuick
import Quickshell.Io
import qs.Commons

Item {
  id: root

  property var pluginApi: null

  // Expose service to bar widget / panel via mainInstance
  property alias displayService: displayService

  QtObject {
    id: displayService

    // Normalized output list:
    // { name, make, model, enabled, currentMode, modes[], scale, position{x,y}, transform }
    property var outputs: []
    property int outputCount: 0
    property int enabledCount: 0
    property string fetchState: "idle" // "idle", "loading", "success", "error"
    property string errorMessage: ""

    // Track last count to surface hotplug events in the bar pill
    property int lastEnabledCount: -1
    property bool countChanged: false

    function backend() {
      var cfg = pluginApi?.pluginSettings || {};
      var defaults = pluginApi?.manifest?.metadata?.defaultSettings || {};
      return cfg.backend ?? defaults.backend;
    }

    function queryCmd() {
      switch (backend()) {
      case "niri":
        return ["niri", "msg", "-j", "outputs"];
      case "hyprland":
        return ["hyprctl", "-j", "monitors", "all"];
      case "sway":
        return ["swaymsg", "-t", "get_outputs", "-r"];
      default:
        return ["wlr-randr", "--json"];
      }
    }

    function fetchOutputs() {
      fetchState = "loading";
      fetchProcess.command = queryCmd();
      fetchProcess.running = true;
    }

    // opts: { mode, scale, x, y, transform, enabled }
    function applyOutput(name, opts) {
      var cmds = [];
      var b = backend();

      if (b === "niri") {
        if (opts.enabled === false) {
          cmds.push(["niri", "msg", "output", name, "off"]);
        } else {
          if (opts.enabled === true)
            cmds.push(["niri", "msg", "output", name, "on"]);
          if (opts.mode)
            cmds.push(["niri", "msg", "output", name, "mode", opts.mode]);
          if (opts.scale !== undefined)
            cmds.push(["niri", "msg", "output", name, "scale", String(opts.scale)]);
          if (opts.x !== undefined && opts.y !== undefined)
            cmds.push(["niri", "msg", "output", name, "position", opts.x + "," + opts.y]);
          if (opts.transform)
            cmds.push(["niri", "msg", "output", name, "transform", opts.transform]);
        }
      } else {
        Logger.w("DisplayConfig", "applyOutput not implemented for backend:", b);
        return;
      }

      applyQueue = cmds;
      runNextApply();
    }

    property var applyQueue: []

    function runNextApply() {
      if (applyQueue.length === 0) {
        // Re-query to reflect actual state after compositor applied changes
        fetchOutputs();
        return;
      }
      var cmd = applyQueue.shift();
      Logger.d("DisplayConfig", "Applying:", JSON.stringify(cmd));
      applyProcess.command = cmd;
      applyProcess.running = true;
    }

    function applyPreset(preset) {
      // preset: { name, outputs: [{ name, mode, scale, x, y, transform, enabled }] }
      var all = [];
      for (var i = 0; i < preset.outputs.length; i++) {
        var o = preset.outputs[i];
        var b = backend();
        if (b !== "niri") {
          Logger.w("DisplayConfig", "applyPreset not implemented for backend:", b);
          return;
        }
        if (o.enabled === false) {
          all.push(["niri", "msg", "output", o.name, "off"]);
          continue;
        }
        all.push(["niri", "msg", "output", o.name, "on"]);
        if (o.mode)
          all.push(["niri", "msg", "output", o.name, "mode", o.mode]);
        if (o.scale !== undefined)
          all.push(["niri", "msg", "output", o.name, "scale", String(o.scale)]);
        if (o.x !== undefined && o.y !== undefined)
          all.push(["niri", "msg", "output", o.name, "position", o.x + "," + o.y]);
        if (o.transform)
          all.push(["niri", "msg", "output", o.name, "transform", o.transform]);
      }
      applyQueue = all;
      runNextApply();
    }

    function saveCurrentAsPreset(name) {
      var cfg = pluginApi?.pluginSettings || {};
      if (!cfg.presets)
        cfg.presets = [];
      var snap = [];
      for (var i = 0; i < outputs.length; i++) {
        var o = outputs[i];
        snap.push({
                    "name": o.name,
                    "enabled": o.enabled,
                    "mode": o.currentMode,
                    "scale": o.scale,
                    "x": o.position.x,
                    "y": o.position.y,
                    "transform": o.transform
                  });
      }
      cfg.presets.push({
                         "name": name,
                         "outputs": snap
                       });
      pluginApi?.saveSettings();
    }
  }

  // --- JSON normalizers per backend ------------------------------------

  function normalizeNiri(json) {
    // niri returns an object keyed by connector name
    var list = [];
    for (var name in json) {
      var o = json[name];
      var modes = [];
      if (Array.isArray(o.modes)) {
        for (var i = 0; i < o.modes.length; i++) {
          var m = o.modes[i];
          var hz = (m.refresh_rate / 1000).toFixed(3).replace(/\.?0+$/, "");
          modes.push({
                       "key": m.width + "x" + m.height + "@" + hz,
                       "width": m.width,
                       "height": m.height,
                       "refresh": m.refresh_rate / 1000,
                       "preferred": !!m.is_preferred
                     });
        }
        // Sort: resolution desc, then refresh desc
        modes.sort(function (a, b) {
          var ra = a.width * a.height, rb = b.width * b.height;
          if (ra !== rb)
            return rb - ra;
          return b.refresh - a.refresh;
        });
      }

      // current_mode is an index into the (unsorted) modes array
      var cur = null;
      if (o.current_mode !== null && o.current_mode !== undefined && Array.isArray(o.modes)) {
        var cm = o.modes[o.current_mode];
        if (cm) {
          var chz = (cm.refresh_rate / 1000).toFixed(3).replace(/\.?0+$/, "");
          cur = cm.width + "x" + cm.height + "@" + chz;
        }
      }

      // niri uses capitalized transform names; normalize to lowercase for CLI
      var xform = "normal";
      if (o.logical && o.logical.transform) {
        xform = String(o.logical.transform).toLowerCase().replace(/^flipped(\d)/, "flipped-$1");
      }

      list.push({
                  "name": name,
                  "make": o.make || "",
                  "model": o.model || "",
                  "enabled": cur !== null,
                  "currentMode": cur,
                  "modes": modes,
                  "scale": o.logical ? o.logical.scale : 1.0,
                  "position": o.logical ? {
                                            "x": o.logical.x,
                                            "y": o.logical.y
                                          } : {
                                            "x": 0,
                                            "y": 0
                                          },
                  "transform": xform
                });
    }
    list.sort(function (a, b) {
      return a.name.localeCompare(b.name);
    });
    return list;
  }

  Process {
    id: fetchProcess
    stdout: StdioCollector {}
    stderr: StdioCollector {}

    onExited: function (exitCode) {
      if (exitCode !== 0) {
        displayService.fetchState = "error";
        displayService.errorMessage = stderr.text || ("exit " + exitCode);
        Logger.w("DisplayConfig", "Query failed:", exitCode, stderr.text);
        return;
      }
      var txt = stdout.text;
      if (!txt || txt.trim() === "") {
        displayService.fetchState = "error";
        displayService.errorMessage = "Empty response";
        return;
      }
      try {
        var json = JSON.parse(txt);
        var outs;
        switch (displayService.backend()) {
        case "niri":
          outs = normalizeNiri(json);
          break;
        default:
          // TODO: hyprland/sway/wlr-randr normalizers
          outs = [];
          displayService.errorMessage = "Backend not yet implemented";
          displayService.fetchState = "error";
          return;
        }
        displayService.outputs = outs;
        displayService.outputCount = outs.length;
        var en = 0;
        for (var i = 0; i < outs.length; i++)
          if (outs[i].enabled)
            en++;
        displayService.countChanged = (displayService.lastEnabledCount !== -1 && displayService.lastEnabledCount !== en);
        displayService.lastEnabledCount = en;
        displayService.enabledCount = en;
        displayService.fetchState = "success";
        Logger.d("DisplayConfig", "Fetched", outs.length, "outputs,", en, "enabled");
      } catch (e) {
        displayService.fetchState = "error";
        displayService.errorMessage = "Parse error: " + e;
        Logger.e("DisplayConfig", "Parse error:", e);
      }
    }
  }

  Process {
    id: applyProcess
    stdout: StdioCollector {}
    stderr: StdioCollector {}

    onExited: function (exitCode) {
      if (exitCode !== 0) {
        Logger.w("DisplayConfig", "Apply failed:", exitCode, stderr.text);
      }
      displayService.runNextApply();
    }
  }

  Timer {
    id: pollTimer
    repeat: true
    running: pluginApi !== null
    triggeredOnStart: true
    interval: {
      var cfg = pluginApi?.pluginSettings || {};
      var defaults = pluginApi?.manifest?.metadata?.defaultSettings || {};
      var secs = cfg.pollInterval ?? defaults.pollInterval;
      return secs * 1000;
    }
    onTriggered: displayService.fetchOutputs()
  }

  IpcHandler {
    target: "plugin:display-config"
    function refresh() {
      displayService.fetchOutputs();
    }
    function toggle() {
      if (pluginApi) {
        pluginApi.withCurrentScreen(function (screen) {
          pluginApi.togglePanel(screen);
        });
      }
    }
    function preset(name: string) {
      var cfg = pluginApi?.pluginSettings || {};
      var presets = cfg.presets || [];
      for (var i = 0; i < presets.length; i++) {
        if (presets[i].name === name) {
          displayService.applyPreset(presets[i]);
          return;
        }
      }
      Logger.w("DisplayConfig", "Preset not found:", name);
    }
  }
}
