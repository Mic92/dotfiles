return {
	"julienvincent/hunk.nvim",
	cmd = { "DiffEditor" },
	config = function()
		require("hunk").setup()
	end,
}
