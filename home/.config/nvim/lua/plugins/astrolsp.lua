return {
	"AstroNvim/astrolsp",
	-- we must use the function override because table merging
	-- does not play nicely with list-like tables
	---@param opts AstroLSPOpts
	opts = function(plugin, opts)
		-- safely extend the servers list
		opts.servers = opts.servers or {}
		vim.list_extend(opts.servers, {
			"bashls",
			"clangd",
			"denols",
			"gopls",
			"lua_ls",
			"marksman",
			"nil_ls",
			"basedpyright",
			"ruff",
			"rust_analyzer",
			"taplo",
			"terraformls",
			"typos_lsp",
			"yamlls",
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
