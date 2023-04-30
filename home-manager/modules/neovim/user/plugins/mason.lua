-- customize mason plugins
local suggested_packages = {}
local utils = require "astronvim.utils"

local disable_auto_install = function(_, opts)
  local data_dir = vim.fn.stdpath("data")
  utils.list_insert_unique(suggested_packages, opts.ensure_installed or {})
  local f = io.open(data_dir .. "/suggested-pkgs.json", "w")
  f:write(vim.fn.json_encode(suggested_packages))
  f:close()

  opts.ensure_installed = {}
end

return {
  -- use mason-lspconfig to configure LSP installations
  {
    "williamboman/mason-lspconfig.nvim",
    -- overrides `require("mason-lspconfig").setup(...)`
    opts = disable_auto_install,
  },
  -- use mason-null-ls to configure Formatters/Linter installation for null-ls sources
  {
    "jay-babu/mason-null-ls.nvim",
    -- overrides `require("mason-null-ls").setup(...)`
    opts = disable_auto_install,
  },
  {
    "jay-babu/mason-nvim-dap.nvim",
    -- overrides `require("mason-nvim-dap").setup(...)`
    opts = disable_auto_install,
  },
}
