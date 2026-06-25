require("lazy").setup({
	{
		"AstroNvim/AstroNvim",
		version = "^6",
		import = "astronvim.plugins",
		opts = {
			-- leader keys must be set before lazy sets up
			mapleader = " ",
			maplocalleader = ",",
		},
	},

	-- make sure we use the commit from nixpkgs that is compatible with our grammars.
	{
		"nvim-treesitter/nvim-treesitter",
		commit = vim.fn.readfile(vim.fn.stdpath("config") .. "/treesitter-rev", "", 1)[1],
	},
	{ import = "plugins" },
} --[[@as LazySpec]], {
	install = { colorscheme = { "tokyonight-night", "habamax" } },
	ui = { backdrop = 100 },
	performance = {
		rtp = {
			-- neo-tree replaces netrw; these builtins are unused
			disabled_plugins = {
				"gzip",
				"netrwPlugin",
				"tarPlugin",
				"tohtml",
				"zipPlugin",
			},
		},
	},
} --[[@as LazyConfig]])
