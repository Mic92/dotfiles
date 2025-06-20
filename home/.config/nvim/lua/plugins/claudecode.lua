return {
	"coder/claudecode.nvim",
	dependencies = { "folke/snacks.nvim" },
	config = true,
	keys = {
		{ "<leader>kk", "<cmd>ClaudeCode<cr>", desc = "Start Claude Code" },
		{ "<leader>kc", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude Code" },
		{ "<leader>kf", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude Code" },
		{ "<leader>kr", "<cmd>ClaudeCodeResume<cr>", desc = "Resume Claude conversation" },
		{ "<leader>kC", "<cmd>ClaudeCodeContinue<cr>", desc = "Continue Claude conversation" },
		{ "<leader>ks", "<cmd>ClaudeCodeSend<cr>", desc = "Send to Claude Code", mode = { "n", "v" } },
		{ "<leader>ka", "<cmd>ClaudeCodeAdd<cr>", desc = "Add file/selection to context", mode = { "n", "v" } },
		{ "<leader>kA", "<cmd>ClaudeCodeAccept<cr>", desc = "Accept diff changes" },
		{ "<leader>kd", "<cmd>ClaudeCodeDeny<cr>", desc = "Deny/reject diff changes" },
	},
}
