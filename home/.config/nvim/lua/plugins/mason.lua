-- Customize Mason

local suggested_packages = {}

---@type LazySpec
return {
	-- use mason-tool-installer for automatically installing Mason packages
	{
		"WhoIsSethDaniel/mason-tool-installer.nvim",
		--opts = {
		--  run_on_start = false
		--},
		opts = function(_, opts)
			local core = require("astrocore")
			local data_dir = vim.fn.stdpath("data")
			core.list_insert_unique(suggested_packages, opts.ensure_installed)
			local f = io.open(data_dir .. "/suggested-pkgs.json", "w")
			f:write(vim.json.encode(suggested_packages))
			f:close()
			opts.run_on_start = false
			opts.integrations["mason-nvim-dap"] = false
		end,
	},
}
