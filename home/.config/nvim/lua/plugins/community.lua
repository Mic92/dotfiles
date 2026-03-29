return {
	-- Add the community repository of plugin specifications
	"AstroNvim/astrocommunity",
	branch = "main",
	-- example of importing a plugin, comment out to use it or add your own
	-- available plugins can be found at https://github.com/AstroNvim/astrocommunity
	{ import = "astrocommunity.completion.copilot-lua" },
	{ import = "astrocommunity.completion.copilot-lua-cmp" },
	{ import = "astrocommunity.completion.blink-cmp-tmux" },
	{
		"zbirenbaum/copilot.lua",
		opts = {
			filetypes = {
				gitcommit = true,
			},
		},
	},
	{ import = "astrocommunity.pack.go" },
	{
		"ray-x/go.nvim",
		-- don't install go binaries with the plugin
		-- instead we install these with nix: https://github.com/ray-x/go.nvim#go-binaries-install-and-update
		build = "true",
	},
	{ import = "astrocommunity.pack.bash" },
	{ import = "astrocommunity.pack.html-css" },
	{ import = "astrocommunity.pack.cpp" },
	{ import = "astrocommunity.pack.lua" },
	{ import = "astrocommunity.pack.markdown" },
	{ import = "astrocommunity.pack.nix" },
	{ import = "astrocommunity.pack.python.ruff" },
	{ import = "astrocommunity.pack.rust" },
	{
		-- With native_lsp_config, astrolsp.lsp_opts("rust_analyzer") returns
		-- nvim-lspconfig's root_dir(bufnr, cb) which rustaceanvim calls with
		-- (file_name, cb) — incompatible signature. Strip it so rustaceanvim
		-- falls back to its own cargo-aware root detection.
		-- TODO: remove once https://github.com/AstroNvim/astrocommunity/pull/1747 is merged
		"mrcjkb/rustaceanvim",
		opts = function(_, opts)
			if opts.server then
				opts.server.root_dir = nil
			end
			return opts
		end,
	},
	{ import = "astrocommunity.pack.toml" },
	{ import = "astrocommunity.pack.yaml" },
	{ import = "astrocommunity.pack.zig" },
	{ import = "astrocommunity.editing-support.rainbow-delimiters-nvim" },
	{ import = "astrocommunity.editing-support.auto-save-nvim" },
	{ import = "astrocommunity.motion.nvim-surround" },
	{ import = "astrocommunity.editing-support.undotree" },
}
