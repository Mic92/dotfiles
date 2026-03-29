return {
	"AstroNvim/astrolsp",
	-- we must use the function override because table merging
	-- does not play nicely with list-like tables
	---@param opts AstroLSPOpts
	opts = function(plugin, opts)
		-- use native vim.lsp.config instead of the deprecated
		-- require('lspconfig') framework (removed in nvim-lspconfig v3)
		opts.native_lsp_config = true

		-- safely extend the servers list
		opts.servers = opts.servers or {}
		vim.list_extend(opts.servers, {
			"bashls",
			"clangd",
			"gopls",
			"lua_ls",
			"nil_ls",
			"basedpyright",
			"ruff",
			"rust_analyzer",
			"taplo",
			"terraformls",
			"typos_lsp",
			"yamlls",
			"vtsls",
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
	config = function(_, opts)
		local astrolsp = require("astrolsp")
		astrolsp.setup(opts)
		-- AstroNvim only calls lsp_setup for mason-installed packages. Since
		-- we install LSPs via Nix, iterate opts.servers ourselves. With
		-- native_lsp_config this calls vim.lsp.enable() and picks up the
		-- lsp/<name>.lua configs shipped by nvim-lspconfig.
		for _, server in ipairs(opts.servers or {}) do
			astrolsp.lsp_setup(server)
		end
	end,
}
