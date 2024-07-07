return {
	"AstroNvim/astrolsp",
	-- we must use the function override because table merging
	-- does not play nicely with list-like tables
	---@param opts AstroLSPOpts
	opts = function(plugin, opts)
		-- safely extend the servers list
		opts.servers = opts.servers or {}
		vim.list_extend(opts.servers, {
			"pyright",
			"clangd",
			"bashls",
			"gopls",
			"taplo",
			"marksman",
			"pyright",
			"nil_ls",
			"terraformls",
			"lua_ls",
			"ruff_lsp",
			"rust_analyzer",
			"gopls",
			"yamlls",
			"denols",
			"ocamllsp",
			"phpactor", -- mediawiki
			-- add more servers as needed...
		})
		opts.formatting = opts.formatting or {}
		-- disrupts the workflow
		opts.formatting.format_on_save = false

		opts.config = opts.config or {}
		opts.config.clangd = opts.config.clangd or {}
		opts.config.clangd.capabilities = {
			offsetEncoding = "utf-8",
		}
	end,
}
