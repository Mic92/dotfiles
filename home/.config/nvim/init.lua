local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  -- stylua: ignore
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable",
    lazypath })
end
vim.opt.rtp:prepend(lazypath)

vim.api.nvim_create_user_command("RaiseTmuxPane", function()
	-- run shell command

	local out = vim.fn.system("tmux list-panes -a -F '#I #{pane_tty}'")
	local tty = vim.env.TTY
	if tty == nil then
		return
	end
	local _, _, window_id = string.find(out, "(%d+) " .. tty)
	if window_id == nil then
		return
	end
	vim.fn.system("tmux select-window -t '" .. window_id .. "'")
end, {})

vim.api.nvim_create_user_command("Nurl", function()
	local url = vim.fn.input("Enter a URL: ")
	local rev = vim.fn.input("Enter the revision (e.g., v0.2.0 or empty string): ")

	local cmd = string.format("nurl %s %s 2>/dev/null", url, rev)

	local output = vim.fn.systemlist(cmd)
	if vim.v.shell_error == 0 and #output > 0 then
		vim.api.nvim_put(output, "l", true, true)
	else
		print("Error executing command or command returned empty result.")
	end
end, {})

require("lazy").setup({
	{
		"AstroNvim/AstroNvim",
		version = "^5",
		import = "astronvim.plugins",
	},

	-- make sure we use the commit from nixpkgs that is compatible with our grammars.
	{
		"nvim-treesitter/nvim-treesitter",
		commit = vim.fn.readfile(vim.fn.stdpath("config") .. "/treesitter-rev", "", 1)[1],
	},
	{ import = "plugins" },
})
