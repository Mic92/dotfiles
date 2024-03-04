return {
  {
    "lewis6991/spaceless.nvim",
    init = function() require("spaceless").setup() end,
  },
  {
    "ziontee113/icon-picker.nvim",
    init = function() require("icon-picker").setup { disable_legacy_commands = true } end,
  },
  -- Doesn't work, wait for the lua rewrite
  --{
  --  "CopilotC-Nvim/CopilotChat.nvim",
  --  -- Not sure how "VeryLazy" is supposed to work?
  --  event = "VeryLazy",
  --  keys = {
  --    { "<leader>ccb", ":CopilotChatBuffer ",         desc = "CopilotChat - Chat with current buffer" },
  --    { "<leader>cce", "<cmd>CopilotChatExplain<cr>", desc = "CopilotChat - Explain code" },
  --    { "<leader>cct", "<cmd>CopilotChatTests<cr>",   desc = "CopilotChat - Generate tests" },
  --    {
  --      "<leader>ccT",
  --      "<cmd>CopilotChatVsplitToggle<cr>",
  --      desc = "CopilotChat - Toggle Vsplit", -- Toggle vertical split
  --    },
  --    {
  --      "<leader>ccv",
  --      ":CopilotChatVisual ",
  --      mode = "x",
  --      desc = "CopilotChat - Open in vertical split",
  --    },
  --    {
  --      "<leader>ccx",
  --      ":CopilotChatInPlace<cr>",
  --      mode = "x",
  --      desc = "CopilotChat - Run in-place code",
  --    },
  --    {
  --      "<leader>ccf",
  --      "<cmd>CopilotChatFixDiagnostic<cr>", -- Get a fix for the diagnostic message under the cursor.
  --      desc = "CopilotChat - Fix diagnostic",
  --    },
  --    {
  --      "<leader>ccr",
  --      "<cmd>CopilotChatReset<cr>", -- Reset chat history and clear buffer.
  --      desc = "CopilotChat - Reset chat history and clear buffer",
  --    },
  --  },
  --},
}
