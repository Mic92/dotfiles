function open_or_create_file(tfilename)
    -- complete filepath from the file where this is called
    local parent = vim.fn.expand('%:p:h')
    local newfilepath = parent .. '/' .. vim.fn.expand(tfilename)

    if vim.fn.filereadable(newfilepath) ~= 1 then
        -- create parent directory
        vim.fn.system('mkdir -p ' .. vim.fn.shellescape(parent))
        vim.fn.system('touch ' .. vim.fn.shellescape(newfilepath))
    end
    vim.cmd(':e ' .. newfilepath)
end

-- Mapping data with "desc" stored directly by vim.keymap.set().
--
-- Please use this mappings table to set keyboard mapping since this is the
-- lower level configuration and more robust one. (which-key will
-- automatically pick-up stored data by this setting.)
return {
  -- first key is the mode
  n = {
    -- Don't error if file doesn't exist
    ["gf"] = { function() open_or_create_file(vim.fn.expand("<cfile>")) end, desc = "Create file" },
    ["<leader>*"] = { function() require("telescope.builtin").grep_string() end, desc = "Find for word under cursor", },
    ["<leader><leader>"] = { function() require("telescope.builtin").find_files() end, desc = "Find files" },
    -- second key is the lefthand side of the map
    -- mappings seen under group name "Buffer"
    ["<leader>bn"] = { "<cmd>tabnew<cr>", desc = "New tab" },
    ["<leader>bD"] = {
      function()
        require("astronvim.utils.status").heirline.buffer_picker(function(bufnr) require("astronvim.utils.buffer").close(bufnr) end)
      end,
      desc = "Pick to close",
    },
    -- tables with the `name` key will be registered with which-key if it's installed
    -- this is useful for naming menus
    ["<leader>b"] = { name = "Buffers" },
    -- quick save
    -- ["<C-s>"] = { ":w!<cr>", desc = "Save File" },  -- change description but the same command
  },
  t = {
    -- setting a mapping to false will disable it
    -- ["<esc>"] = false,
  },
  c = {
    -- setting a mapping to false will disable it
    ["w!!"] = { "w !sudo tee > /dev/null %", desc = "Write as sudo" },
  }
}
