local nvim_data_dir = vim.fn.stdpath("data")
local telescope_dir = nvim_data_dir .. "/lazy/telescope-fzf-native.nvim"
local build = "mkdir -p "
	.. telescope_dir
	.. "/build && ln -sf "
	.. nvim_data_dir
	.. "/lib/libfzf.so "
	.. telescope_dir
	.. "/build/libfzf.so"
-- override make command build since we provide the shared library with home-manager already
return {
	"nvim-telescope/telescope-fzf-native.nvim",
	build = build,
}
