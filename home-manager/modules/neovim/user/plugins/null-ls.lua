return {
  "jose-elias-alvarez/null-ls.nvim",
  opts = function(_, config)
    -- config variable is the default configuration table for the setup function call
    local null_ls = require "null-ls"

    -- Check supported formatters and linters
    -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
    -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
    config.sources = {
      -- Set a formatter
      null_ls.builtins.formatting.stylua,

      -- html/css/js/ts/json/yaml/xml/markdown
      null_ls.builtins.formatting.prettier,

      -- go
      null_ls.builtins.formatting.gofumpt,
      null_ls.builtins.formatting.goimports,
      null_ls.builtins.diagnostics.golangci_lint,
      null_ls.builtins.code_actions.impl,

      -- nix
      null_ls.builtins.formatting.nixpkgs_fmt,

      -- python
      null_ls.builtins.formatting.black,
      null_ls.builtins.formatting.isort,
      null_ls.builtins.formatting.ruff,

      -- c/c++
      null_ls.builtins.formatting.clang_format,

      -- rust
      null_ls.builtins.formatting.rustfmt,

      -- bash
      null_ls.builtins.formatting.shfmt.with {
        args = { "-i", "2" },
      },
    }
    return config -- return final config table
  end,
}
