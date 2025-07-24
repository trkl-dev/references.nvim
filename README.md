# `references.nvim`
Get deeper references
Requires telescope and an language servers providing `textDocument/references` and `textDocument/documentSymbol`


# Installation
## With Lazy
```lua
return {
  {
    "trkl-dev/references.nvim",
    dependencies = {
        'nvim-telescope/telescope.nvim',
    },
    config = function()
      local references = require "references"

      vim.keymap.set('n', '<leader>rr', references.recursive_references, {})
    end
  }
}
```
