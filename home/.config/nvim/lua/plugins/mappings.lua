function open_or_create_file(tfilename)
	-- complete filepath from the file where this is called
	local parent = vim.fn.expand("%:p:h")
	local newfilepath = parent .. "/" .. vim.fn.expand(tfilename)

	if vim.fn.filereadable(newfilepath) ~= 1 then
		-- create parent directory
		vim.fn.system("mkdir -p " .. vim.fn.shellescape(parent))
		vim.fn.system("touch " .. vim.fn.shellescape(newfilepath))
	end
	vim.cmd(":e " .. newfilepath)
end

return {
	"AstroNvim/astrocore",
	---@type AstroCoreOpts
	opts = {
		-- Mapping data with "desc" stored directly by vim.keymap.set().
		--
		-- Please use this mappings table to set keyboard mapping since this is the
		-- lower level configuration and more robust one. (which-key will
		-- automatically pick-up stored data by this setting.)
		mappings = {
			-- first key is the mode
			n = {
				-- Neotree: remember last source and make git the default
				["<Leader>e"] = { "<Cmd>Neotree toggle source=git_status<CR>", desc = "Toggle Explorer" },
				["<Leader>o"] = {
					function()
						if vim.bo.filetype == "neo-tree" then
							vim.cmd.wincmd("p")
						else
							vim.cmd.Neotree({ "focus", "source=last" })
						end
					end,
					desc = "Toggle Explorer Focus",
				},
				-- Switch between tabs
				["<S-Tab>"] = { ":bprev<CR>" },
				["<Tab>"] = { ":bnext<CR>" },

				-- Don't error if file doesn't exist
				["gf"] = {
					function()
						open_or_create_file(vim.fn.expand("<cfile>"))
					end,
					desc = "Create file",
				},
				["<leader>*"] = {
					function()
						require("snacks").picker.grep_word()
					end,
					desc = "Find for word under cursor",
				},
				["<leader><leader>"] = {
					function()
						require("snacks").picker.files({
							hidden = vim.tbl_get((vim.uv or vim.loop).fs_stat(".git") or {}, "type") == "directory",
						})
					end,
					desc = "Find files",
				},
			},
			t = {
				-- setting a mapping to false will disable it
				-- ["<esc>"] = false,
			},
			c = {
				-- setting a mapping to false will disable it
				["w!!"] = { "w !sudo tee > /dev/null %", desc = "Write as sudo" },
			},
		},
	},
}
