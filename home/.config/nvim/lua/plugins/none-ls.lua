return {
	"nvimtools/none-ls.nvim",
	opts = function(_, config)
		local null_ls = require("null-ls")
		local astrocore = require("astrocore")

		-- Check supported formatters and linters
		-- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/formatting
		-- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
		config.sources = astrocore.list_insert_unique(config.sources, {
			-- Set a formatter
			null_ls.builtins.formatting.stylua,

			-- html/css/js/ts/json/yaml/xml/markdown
			null_ls.builtins.formatting.prettier,

			-- go (gofumpt/goimports handled by gopls, only add extras)
			null_ls.builtins.diagnostics.golangci_lint,
			null_ls.builtins.code_actions.impl,

			-- bash
			null_ls.builtins.formatting.shfmt.with({
				args = { "-i", "2" },
			}),
		})
	end,
}
