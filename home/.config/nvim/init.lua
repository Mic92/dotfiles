local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath .. "/lua/lazy/init.lua") then
	vim.fn.delete(lazypath, "rf")
	for _, stale in ipairs(vim.fn.glob(lazypath .. ".tmp.*", false, true)) do
		vim.fn.delete(stale, "rf")
	end
	local tmpdir = lazypath .. ".tmp." .. vim.fn.getpid()
  -- stylua: ignore
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable",
    tmpdir })
	if vim.v.shell_error ~= 0 then
		vim.fn.delete(tmpdir, "rf")
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim (will retry on next launch)\n", "ErrorMsg" },
		}, true, {})
		return
	end
	vim.fn.rename(tmpdir, lazypath)
end
vim.opt.rtp:prepend(lazypath)

require("lazy_setup")
require("polish")
