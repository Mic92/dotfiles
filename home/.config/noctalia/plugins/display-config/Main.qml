import QtQuick
import Quickshell.Io
import qs.Commons
import qs.Services.UI

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

    // Logical (post-scale) width/height for an output given its current/preferred mode.
    // Used by arrangement helpers so we place in logical pixels like niri expects.
    function logicalSize(o) {
      var m = null;
      if (o.currentMode) {
        for (var i = 0; i < o.modes.length; i++)
          if (o.modes[i].key === o.currentMode) {
            m = o.modes[i];
            break;
          }
      }
      if (!m)
        for (var j = 0; j < o.modes.length; j++)
          if (o.modes[j].preferred) {
            m = o.modes[j];
            break;
          }
      if (!m && o.modes.length > 0)
        m = o.modes[0];
      if (!m)
        return {
          "w": 1920,
          "h": 1080
        };
      var s = o.scale || 1.0;
      // Transforms that rotate 90/270 swap width and height.
      var rot = o.transform === "90" || o.transform === "270" || o.transform === "flipped-90" || o.transform === "flipped-270";
      var w = Math.round((rot ? m.height : m.width) / s);
      var h = Math.round((rot ? m.width : m.height) / s);
      return {
        "w": w,
        "h": h
      };
    }

    // Pick primary/secondary for two-output arrangements: prefer the internal
    // panel (eDP/LVDS) as "laptop" and everything else as "external".
    function splitPrimarySecondary() {
      if (outputs.length < 2)
        return null;
      var laptop = null, external = null;
      for (var i = 0; i < outputs.length; i++) {
        var n = outputs[i].name.toLowerCase();
        if (n.indexOf("edp") === 0 || n.indexOf("lvds") === 0 || n.indexOf("dsi") === 0)
          laptop = outputs[i];
        else
          external = outputs[i];
      }
      // Fallback: first two in sort order.
      if (!laptop || !external) {
        laptop = outputs[0];
        external = outputs[1];
      }
      return {
        "primary": laptop,
        "secondary": external
      };
    }

    // kind: "extend-right" | "extend-left" | "stack-above" | "stack-below"
    //       | "external-only" | "internal-only"
    // Computes positions from logical sizes so edges actually touch and the
    // mouse crosses cleanly. All outputs are anchored in the +x/+y quadrant.
    function applyArrangement(kind) {
      if (outputs.length !== 2) {
        Logger.w("DisplayConfig", "applyArrangement only supports exactly 2 outputs, have", outputs.length, "- use a saved preset instead");
        return;
      }
      var pair = splitPrimarySecondary();
      if (!pair) {
        return;
      }
      var a = pair.primary, b = pair.secondary;
      var as = logicalSize(a), bs = logicalSize(b);
      var cmds = [];

      function on(name) {
        cmds.push(["niri", "msg", "output", name, "on"]);
      }
      function off(name) {
        cmds.push(["niri", "msg", "output", name, "off"]);
      }
      function pos(name, x, y) {
        cmds.push(["niri", "msg", "output", name, "position", "set", String(x), String(y)]);
      }

      if (backend() !== "niri") {
        Logger.w("DisplayConfig", "applyArrangement not implemented for backend:", backend());
        return;
      }

      switch (kind) {
      case "extend-right":
        // Laptop at origin, external to its right, tops aligned.
        on(a.name);
        on(b.name);
        pos(a.name, 0, 0);
        pos(b.name, as.w, 0);
        break;
      case "extend-left":
        on(a.name);
        on(b.name);
        pos(b.name, 0, 0);
        pos(a.name, bs.w, 0);
        break;
      case "stack-above":
        // External on top, laptop below, horizontally centred under it.
        on(a.name);
        on(b.name);
        pos(b.name, 0, 0);
        pos(a.name, Math.max(0, Math.round((bs.w - as.w) / 2)), bs.h);
        break;
      case "stack-below":
        on(a.name);
        on(b.name);
        pos(a.name, 0, 0);
        pos(b.name, Math.max(0, Math.round((as.w - bs.w) / 2)), as.h);
        break;
      case "external-only":
        off(a.name);
        on(b.name);
        pos(b.name, 0, 0);
        break;
      case "internal-only":
        on(a.name);
        off(b.name);
        pos(a.name, 0, 0);
        break;
      default:
        Logger.w("DisplayConfig", "Unknown arrangement:", kind);
        return;
      }

      armRevert();
      applyQueue = cmds;
      runNextApply();
    }

    // Snapshot of all outputs before a change, used to revert if the user
    // doesn't confirm the new layout within the countdown.
    property var revertSnapshot: null
    property bool revertConfirmed: false
    // Exposed for the in-panel confirm bar.
    property bool revertPending: false
    property int revertSecondsLeft: 0

    function takeSnapshot() {
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
      return snap;
    }

    function armRevert(seconds) {
      var s = seconds || 12;
      // Keep the earliest snapshot across a burst of changes so revert
      // restores the state before the user started editing, not the state
      // after the previous click in the same burst.
      if (!revertPending || revertSnapshot === null) {
        revertSnapshot = takeSnapshot();
      }
      revertConfirmed = false;
      revertPending = true;
      revertSecondsLeft = s;
      revertTimer.restart();
      ToastService.showNotice("Display changed", "Reverting in " + s + "s unless confirmed", "device-desktop", s * 1000, "Keep", function () {
        displayService.confirmRevert();
      });
    }

    function confirmRevert() {
      revertConfirmed = true;
      revertPending = false;
      revertTimer.stop();
      revertSnapshot = null;
      Logger.d("DisplayConfig", "Change confirmed, revert cancelled");
    }

    function doRevert() {
      revertPending = false;
      revertTimer.stop();
      if (revertConfirmed || !revertSnapshot) {
        return;
      }
      Logger.i("DisplayConfig", "Reverting unconfirmed display change");
      var snap = revertSnapshot;
      revertSnapshot = null;
      // Reuse applyPreset since the snapshot has the same shape.
      applyPreset({
                    "name": "__revert__",
                    "outputs": snap
                  });
      ToastService.showNotice("Display reverted", "Change was not confirmed", "restore");
    }

    // opts: { mode, scale, x, y, transform, enabled }
    // If revert === true, snapshot current state first and start the confirm
    // countdown; a bad mode can black-screen a monitor so changes must be
    // acknowledged or they roll back automatically.
    function applyOutput(name, opts, revert) {
      if (revert) {
        armRevert();
      }
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
            cmds.push(["niri", "msg", "output", name, "position", "set", String(opts.x), String(opts.y)]);
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
          all.push(["niri", "msg", "output", o.name, "position", "set", String(o.x), String(o.y)]);
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
      cfg.presets.push({
                         "name": name,
                         "outputs": takeSnapshot()
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

      // niri JSON uses serde variant names (Normal, 90, Flipped, Flipped90, ...)
      // but the CLI wants lowercase kebab-case (normal, 90, flipped, flipped-90, ...)
      var xform = "normal";
      if (o.logical && o.logical.transform) {
        var t = String(o.logical.transform);
        var map = {
          "Normal": "normal",
          "90": "90",
          "180": "180",
          "270": "270",
          "Flipped": "flipped",
          "Flipped90": "flipped-90",
          "Flipped180": "flipped-180",
          "Flipped270": "flipped-270"
        };
        xform = map[t] || t.toLowerCase();
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
        var hotplug = (displayService.lastEnabledCount !== -1 && displayService.lastEnabledCount !== en);
        displayService.countChanged = hotplug;
        displayService.lastEnabledCount = en;
        displayService.enabledCount = en;
        displayService.fetchState = "success";
        // Auto-open the panel on hotplug so picking an arrangement is one
        // click away instead of a hunt for the bar icon. Skip if a revert is
        // pending — that means the user caused the change themselves.
        if (hotplug && !displayService.revertPending && pluginApi) {
          var cfg = pluginApi.pluginSettings || {};
          var defaults = pluginApi.manifest?.metadata?.defaultSettings || {};
          if (cfg.openOnHotplug ?? defaults.openOnHotplug) {
            pluginApi.withCurrentScreen(function (screen) {
              pluginApi.openPanel(screen);
            });
          }
        }
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

  // Ticks once per second so the in-panel confirm bar can show a live
  // countdown; the revert itself fires when secondsLeft hits zero.
  Timer {
    id: revertTimer
    repeat: true
    interval: 1000
    onTriggered: {
      if (displayService.revertSecondsLeft > 1) {
        displayService.revertSecondsLeft--;
      } else {
        displayService.revertSecondsLeft = 0;
        displayService.doRevert();
      }
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
    function arrange(kind: string) {
      displayService.applyArrangement(kind);
    }
    function keep() {
      displayService.confirmRevert();
    }
  }
}
