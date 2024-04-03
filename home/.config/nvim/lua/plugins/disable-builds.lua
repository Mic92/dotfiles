return {
	-- override make command build since we provide the shared library with home-manager already
	{ "nvim-telescope/telescope-fzf-native.nvim", build = "true" },
	-- disable building since it's optional
	{ "L3MON4D3/LuaSnip", build = "true" },
}
