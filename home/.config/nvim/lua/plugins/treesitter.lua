local nvim_data_dir = vim.fn.stdpath("data")
local command = "ln -sf " .. nvim_data_dir .. "/site/parser/*.so " .. nvim_data_dir .. "/lazy/nvim-treesitter/parser"

return {
	"nvim-treesitter/nvim-treesitter",
	build = command,
	opts = function(_, opts)
		vim.fn.system(command)
	end,
}
