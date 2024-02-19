vim.api.nvim_create_user_command("RaiseTmuxPane", function()
  -- run shell command

  local out = vim.fn.system "tmux list-panes -a -F '#I #{pane_tty}'"
  local tty = vim.env.TTY
  if tty == nil then return end
  local _, _, window_id = string.find(out, "(%d+) " .. tty)
  if window_id == nil then return end
  vim.fn.system("tmux select-window -t '" .. window_id .. "'")
end, {})

vim.api.nvim_create_user_command("Nurl", function()
  local url = vim.fn.input "Enter a URL: "
  local rev = vim.fn.input "Enter the revision (e.g., v0.2.0 or empty string): "

  local cmd = string.format("nurl %s %s 2>/dev/null", url, rev)

  local output = vim.fn.systemlist(cmd)
  if vim.v.shell_error == 0 and #output > 0 then
    vim.api.nvim_put(output, "l", true, true)
  else
    print "Error executing command or command returned empty result."
  end
end, {})

return {
  -- Configure AstroNvim updates
  updater = {
    remote = "origin", -- remote to use
    channel = "stable", -- "stable" or "nightly"
    version = "latest", -- "latest", tag name, or regex search like "v1.*" to only do updates before v2 (STABLE ONLY)
    branch = "nightly", -- branch name (NIGHTLY ONLY)
    commit = nil, -- commit hash (NIGHTLY ONLY)
    pin_plugins = nil, -- nil, true, false (nil will pin plugins on stable only)
    skip_prompts = false, -- skip prompts about breaking changes
    show_changelog = true, -- show the changelog after performing an update
    auto_quit = false, -- automatically quit the current session after a successful update
    remotes = { -- easily add new remotes to track
      --   ["remote_name"] = "https://remote_url.come/repo.git", -- full remote url
      --   ["remote2"] = "github_user/repo", -- GitHub user/repo shortcut,
      --   ["remote3"] = "github_user", -- GitHub user assume AstroNvim fork
    },
  },
  -- Set colorscheme to use
  colorscheme = "NeoSolarized",
  -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
  diagnostics = {
    virtual_text = true,
    underline = true,
  },
  lsp = {
    config = {
      clangd = {
        cmd = { "clangd", "--offset-encoding=utf-16" },
      },
      vale = {
        cmd = { "vale", "--lsp" },
        filetypes = { "markdown", "text" },
      },
    },
    -- customize lsp formatting options
    formatting = {
      -- control auto formatting on save
      format_on_save = {
        enabled = true, -- enable or disable format on save globally
        allow_filetypes = { -- enable format on save for specified filetypes only
          "go",
        },
        ignore_filetypes = { -- disable format on save for specified filetypes
          -- "python",
        },
      },
      disabled = { -- disable formatting capabilities for the listed language servers
        -- disable lua_ls formatting capability if you want to use StyLua to format your lua code
        -- "lua_ls",
      },
      timeout_ms = 1000, -- default format timeout
      -- filter = function(client) -- fully override the default formatting function
      --   return true
      -- end
    },
    -- enable servers that you already have installed without mason
    servers = {
      "clangd",
      "bashls",
      "gopls",
      "taplo",
      "marksman",
      "pyright",
      "nil_ls",
      "terraformls",
      "lua_ls",
      "vale",
      "rust_analyzer",
      "gopls",
      "yamlls",
      "denols",
      "ocamllsp",
    },
  },
  -- Configure require("lazy").setup() options
  lazy = {
    defaults = { lazy = true },
    -- Disable change detection because it caused nvim to constantly need 3-5% CPU, which is bad for battery life
    change_detection = { enabled = false },
    performance = {
      rtp = {
        -- customize default disabled vim plugins
        disabled_plugins = { "tohtml", "gzip", "matchit", "zipPlugin", "netrwPlugin", "tarPlugin" },
      },
    },
  },
  -- This function is run last and is a good place to configuring
  -- augroups/autocommands and custom filetypes also this just pure lua so
  -- anything that doesn't fit in the normal config locations above can go here
  polish = function()
    -- Set up custom filetypes
    -- vim.filetype.add {
    --   extension = {
    --     foo = "fooscript",
    --   },
    --   filename = {
    --     ["Foofile"] = "fooscript",
    --   },
    --   pattern = {
    --     ["~/%.config/foo/.*"] = "fooscript",
    --   },
    -- }
  end,
}
