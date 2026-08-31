-- Runs last in the setup process. Pure lua for things that don't fit
-- the normal plugin/config locations.

-- AstroNvim sets clipboard=unnamedplus, and Neovim refuses to auto-select the
-- OSC 52 provider while 'clipboard' is set. Without tmux or X/Wayland tools
-- (e.g. inside herdr over ssh) that leaves no provider at all, so opt in.
local has_gui_clip = (vim.env.WAYLAND_DISPLAY and vim.fn.executable("wl-copy") == 1)
	or (vim.env.DISPLAY and (vim.fn.executable("xclip") == 1 or vim.fn.executable("xsel") == 1))
if vim.env.TMUX == nil and not has_gui_clip then
	vim.g.clipboard = "osc52"
end

vim.api.nvim_create_user_command("RaiseTmuxPane", function()
	-- Get the current tmux pane ID
	local current_pane = vim.fn.environ()["TMUX_PANE"]

	-- If we're not in tmux, notify and exit
	if not current_pane or current_pane == "" then
		vim.notify("Not running inside tmux", vim.log.levels.WARN)
		return
	end

	-- Select the window containing this pane
	vim.fn.system("tmux select-window -t " .. current_pane)

	vim.notify("Raised tmux window", vim.log.levels.INFO)
end, {})

vim.api.nvim_create_user_command("Nurl", function()
	local url = vim.fn.input("Enter a URL: ")
	local rev = vim.fn.input("Enter the revision (e.g., v0.2.0 or empty string): ")

	-- Check if `nurl` command exists
	if vim.fn.executable("nurl") == 0 then
		vim.notify("The 'nurl' command is not installed or not in PATH", vim.log.levels.ERROR)
		return
	end

	local cmd = string.format("nurl %s %s 2>/dev/null", url, rev)

	local output = vim.fn.systemlist(cmd)
	if vim.v.shell_error == 0 and #output > 0 then
		vim.api.nvim_put(output, "l", true, true)
	else
		vim.notify("Error executing 'nurl' command or command returned empty result.", vim.log.levels.ERROR)
	end
end, {})
