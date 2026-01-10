return {
	"nvim-treesitter/nvim-treesitter",
	build = "nvim-install-treesitter",
	opts = {
		-- Disable auto_install since we provide all grammars through Nix
		auto_install = false,
	},
}
