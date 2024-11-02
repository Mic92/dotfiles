local function get_recent_commit_messages(count)
	local handle = io.popen("git log -n " .. count .. " --pretty=format:'%s'")
	if not handle then
		return "No recent commit messages found."
	end

	local result = handle:read("*a")
	handle:close()
	return result
end

return {
	"CopilotC-Nvim/CopilotChat.nvim",
	branch = "canary",
	dependencies = {
		{ "zbirenbaum/copilot.lua" }, -- or github/copilot.vim
		{ "nvim-lua/plenary.nvim" }, -- for curl, log wrapper
	},
	lazy = false,
	keys = function()
		local select = require("CopilotChat.select")
		local telescope = require("CopilotChat.integrations.telescope")
		local actions = require("CopilotChat.actions")

		return {
			{
				"<leader>ccq",
				function()
					local input = vim.fn.input("Quick Chat: ")
					if input ~= "" then
						require("CopilotChat").ask(input, { selection = select.buffer })
					end
				end,
				desc = "CopilotChat - Quick chat",
			},
			{
				"<leader>ccr",
				function()
					-- Get recent commit messages for context
					local commit_messages = get_recent_commit_messages(3)
					local input = "Resolve the merge conflict in the selected code. Recent commit messages:\n"
						.. commit_messages
					require("CopilotChat").ask(input, { selection = select.visual })
				end,
				desc = "CopilotChat - Resolve merge conflict in selection with commit context",
			},
			{
				"<leader>ccq",
				function()
					local input = vim.fn.input("Quick Chat: ")
					if input ~= "" then
						require("CopilotChat").ask(input, { selection = select.visual })
					end
				end,
				desc = "CopilotChat - Quick chat",
				mode = "v",
			},
			{
				"<leader>cch",
				function()
					telescope.pick(actions.help_actions(), { selection = select.buffer })
				end,
				desc = "CopilotChat - Help actions",
			},
			{
				"<leader>cch",
				function()
					telescope.pick(actions.help_actions(), { selection = select.visual })
				end,
				desc = "CopilotChat - Help actions",
				mode = "v",
			},
			{
				"<leader>ccp",
				function()
					telescope.pick(actions.prompt_actions(), { selection = select.visual })
				end,
				desc = "CopilotChat - Prompt actions",
			},
			{
				"<leader>ccp",
				function()
					telescope.pick(actions.prompt_actions(), { selection = select.visual })
				end,
				desc = "CopilotChat - Prompt actions",
				mode = "v",
			},
		}
	end,
	opts = {},
}
