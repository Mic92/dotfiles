return {
	{
		"ziontee113/icon-picker.nvim",
		init = function()
			require("icon-picker").setup({ disable_legacy_commands = true })
		end,
	},
}
