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
			-- add more servers as needed...
		})
	end,
}
