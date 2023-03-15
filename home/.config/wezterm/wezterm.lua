local wezterm = require 'wezterm';

return {
  color_scheme = "Dracula",
  tls_clients = {
    {
      -- This name identifies the domain
      name = "eve",
      -- The address to connect to
      remote_address = "eve.thalheim.io:60000",
      bootstrap_via_ssh = "eve.thalheim.io",
      expected_cn = "eve"
    },
  },
  --unix_domains = {
  --  {
  --    name = "unix",
  --    connect_automatically = true,
  --  }
  --},
  --tls_servers = {
  --  {
  --    -- The host:port combination on which the server will listen
  --    -- for connections
  --    bind_address = "eve.thalheim.io:60000"
  --  }
  --},
  exit_behavior = "Close",
  leader = { key="b", mods="CTRL", timeout_milliseconds=1000 },
  mouse_bindings = {
    {
      event={Down={streak=3, button="Left"}},
      action={SelectTextAtMouseCursor="SemanticZone"},
      mods="NONE"
    },
  },
  quick_select_patterns = {
    -- match things that look like sha1 hashes
    -- (this is actually one of the default patterns)
    -- sri-hash
    "sha256-[0-9a-zA-z=/+]{44}",
    "[0-9a-f]{7,40}",
    -- sha256
    "[0-9a-z]{52}"
  },
  keys = {
    {key="UpArrow", mods="SHIFT", action=wezterm.action{ScrollToPrompt=-1}},
    {key="DownArrow", mods="SHIFT", action=wezterm.action{ScrollToPrompt=1}},

    {key="h", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="l", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Right"}},
    {key="j", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Down"}},
    {key="k", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Up"}},
    {key=";", mods="LEADER", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},

    -- vim compat
    {key="v", mods="LEADER", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},

    -- screen/tmux compat
    {key="c", mods="LEADER", action=wezterm.action{SpawnTab="CurrentPaneDomain"}},

    {key="1", mods="LEADER", action=wezterm.action{ActivateTab=0}},
    {key="2", mods="LEADER", action=wezterm.action{ActivateTab=1}},
    {key="3", mods="LEADER", action=wezterm.action{ActivateTab=2}},
    {key="4", mods="LEADER", action=wezterm.action{ActivateTab=3}},
    {key="5", mods="LEADER", action=wezterm.action{ActivateTab=4}},
    {key="6", mods="LEADER", action=wezterm.action{ActivateTab=5}},
    {key="7", mods="LEADER", action=wezterm.action{ActivateTab=6}},
    {key="8", mods="LEADER", action=wezterm.action{ActivateTab=7}},
    {key="9", mods="LEADER", action=wezterm.action{ActivateTab=-1}},
    -- Send "CTRL-A" to the terminal when pressing CTRL-A, CTRL-A
    {key="b", mods="LEADER|CTRL", action=wezterm.action{SendString="\x02"}},
  }
}
