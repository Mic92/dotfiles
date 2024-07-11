return {
	"AstroNvim/astrocore",
	lazy = false, -- disable lazy loading
	priority = 10000, -- load AstroCore first
	opts = {
		options = {
			opt = {
				-- set configuration options  as described below
				relativenumber = false, -- sets vim.opt.relativenumber
			},
		},
	},
}
