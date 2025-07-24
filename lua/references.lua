local M = {}

local function dump(o)
  if type(o) == 'table' then
    local s = '{ '
    for k, v in pairs(o) do
      s = s .. '"' .. k .. '"' .. ': ' .. dump(v) .. ','
    end
    return s .. '} '
  else
    return tostring('"' .. o .. '"')
  end
end

---@param func Func
local function func_key(func)
  return table.concat({ func.file, func.func_name, func.line }, ":")
end

---@alias LocationTable table<integer, Location>
---@alias Location {uri: string, range: LSPRange}

---@param bufnr integer
---@param position Position
---@return table<integer, {error: lsp.ResponseError?, result: LocationTable}>? result Map of client_id:request_result.
local function get_references_sync(bufnr, uri, position)
  return vim.lsp.buf_request_sync(bufnr, "textDocument/references", {
    textDocument = { uri = uri },
    position = position,
    context = { includeDeclaration = false }
  }, 1000)
end

---@enum SymbolKind
local SYMBOLKINDS = {
  File = 1,
  Module = 2,
  Namespace = 3,
  Package = 4,
  Class = 5,
  Method = 6,
  Property = 7,
  Field = 8,
  Constructor = 9,
  Enum = 10,
  Interface = 11,
  Function = 12,
  Variable = 13,
  Constant = 14,
  String = 15,
  Number = 16,
  Boolean = 17,
  Array = 18,
  Object = 19,
  Key = 20,
  Null = 21,
  EnumMember = 22,
  Struct = 23,
  Event = 24,
  Operator = 25,
  TypeParameter = 26,
}
---@alias Position {line: integer, character: integer}
---@alias LSPRange {start: Position, end: Position}
---@alias Symbol {name: string, detail: string?, kind: SymbolKind, range: LSPRange, selectionRange: LSPRange, children: table<integer, Symbol>?}
---@alias SymbolTable table<integer, Symbol>

---@param bufnr integer
---@param uri string
---@return table<integer, {error: lsp.ResponseError?, result: SymbolTable}>? result Map of client_id:request_result.
local function get_document_symbols_sync(bufnr, uri)
  return vim.lsp.buf_request_sync(bufnr, "textDocument/documentSymbol", {
    textDocument = { uri = uri }
  }, 1000)
end

---@param symbols SymbolTable
---@param ref Location
---@return Symbol
local function find_enclosing_function(symbols, ref)
  if ref.range == nil then
    print("range is nil: " .. dump(ref))
    return {}
  end
  local line = ref.range.start.line
  for _, symbol in ipairs(symbols) do
    -- if symbol.kind == SYMBOLKINDS.Function or symbol.kind == SYMBOLKINDS.Class or symbol.kind == SYMBOLKINDS.Method then -- function or method
    -- print("symbol: " .. dump(symbol))
    local range = symbol.range
    -- print("line: " .. line .. " range: " .. dump(range))
    if line >= range.start.line and line <= range["end"].line then
      -- print("symbol found: " .. dump(symbol))
      return symbol
    end
    -- end
  end
  print("Unable to find symbol for ref: " .. dump(ref))
  return {}
end

---@alias Func {file: string, func_name: string, line: integer}
---@alias ReferenceNode {func: Func, children: table<string, ReferenceNode>}

---@type table<string, ReferenceNode>
local nodes = {}

-- The main recursive function
---@param bufnr     integer buffer number
---@param uri       string file uri
---@param position  Position cursor position
local function get_references(bufnr, uri, position, parent_node)
  local reference_locations = get_references_sync(bufnr, uri, position)
  if #reference_locations == 0 then
    return
  end
  if reference_locations == nil then
    return
  end

  for _, outer_reference in pairs(reference_locations) do
    if outer_reference.error ~= nil then
      print("error: " .. dump(outer_reference.error))
      goto outer_continue
    end

    -- FIX: remove the 'or {}'?
    for _, reference in ipairs(outer_reference.result or {}) do
      local outer_symbols = get_document_symbols_sync(bufnr, reference.uri)
      if outer_symbols == nil then
        print("outer symbols nil")
        goto reference_continue
      end

      for _, symbols in ipairs(outer_symbols) do
        if symbols.error ~= nil then
          print("error: " .. dump(symbols.error))
        end

        local enclosing_function = find_enclosing_function(symbols.result, reference)
        if enclosing_function == nil then
          print("enc func nil")
          goto continue
        end

        -- print("enc func: " .. dump(enclosing_function))
        local func = {
          file = reference.uri,
          func_name = enclosing_function.name,
          line = reference.range.start.line + 1
        }

        -- print("func: " .. dump(func))
        local node = nodes[func_key(func)]
        if not node then
          ---@type ReferenceNode
          node = {
            func = func,
            children = {},
          }
          nodes[func_key(func)] = node
        end

        node.children[func_key(parent_node.func)] = parent_node

        get_references(bufnr, reference.uri, enclosing_function.selectionRange.start, node)
        ::continue::
      end
      ::reference_continue::
    end
  end
  ::outer_continue::
end

---@alias TelescopeNode {file: string, func_name: string, line: integer, col: integer, display_name: string, ordinal: string}

---@type TelescopeNode[]
local flat_nodes = {}

---@param node ReferenceNode
---@param indent nil|string
---@param root_processed nil|boolean
local function print_tree(node, indent, root_processed)
  indent = indent or ""
  root_processed = root_processed or false

  local reference_string = ""

  if not root_processed then
    reference_string = node.func.func_name .. "(): " .. node.func.file .. ":" .. node.func.line
    root_processed = true
  else
    reference_string = indent .. "└─" .. node.func.func_name .. "(): " .. node.func.file .. ":" .. node.func.line
  end
  -- print(reference_string)
  ---@type TelescopeNode
  local new_node = {
    file = node.func.file,
    func_name = node.func.func_name,
    line = node.func.line,
    display_name = reference_string,
    col = 0,
    ordinal = reference_string,
  }
  table.insert(flat_nodes, new_node)

  for _, child in pairs(node.children) do
    print_tree(child, indent .. "  ", root_processed)
  end
end

function M.recursive_references()
  local bufnr = vim.api.nvim_get_current_buf()
  local position = vim.api.nvim_win_get_cursor(0)
  local pos = { line = position[1] - 1, character = position[2] }
  local uri = vim.uri_from_bufnr(bufnr)

  -- QUES: Does this kind of thing run constantly? Such that this would grow constantly if not emptied?
  nodes = {}
  flat_nodes = {}

  -- TODO: This is probably a good place to handle stuff anyway
  local initial_references = get_references_sync(bufnr, uri, pos)
  if initial_references == nil then
    print("initial references nil")
    return
  end

  -- for _, outer_reference in pairs(initial_references) do
  --   if outer_reference.error ~= nil then
  --     print("error: " .. dump(outer_reference.error))
  --     return
  --   end
  --
  --   for _, reference in ipairs(outer_reference.result) do
  --     print(reference.range.
  --   end
  --
  -- end

  local symbols = get_document_symbols_sync(bufnr, uri)
  if symbols == nil then
    print("symbols nil")
    return
  end

  ---@type nil|Symbol
  local root_func_symbol = nil

  for _, outer_symbol in ipairs(symbols) do
    if outer_symbol.error ~= nil then
      print("error: " .. dump(outer_symbol.error))
    end

    for _, symbol in ipairs(outer_symbol.result) do
      local range = symbol.range
      if pos.line >= range.start.line and pos.line <= range["end"].line then
        root_func_symbol = symbol
      end
    end
  end

  if root_func_symbol == nil then
    print("root func symbol nil")
    return
  end

  local root_func = {
    file = uri,
    func_name = root_func_symbol.name,
    line = pos.line + 1,
  }

  local root_node = {
    func = root_func,
    children = {},
  }
  nodes[func_key(root_func)] = root_node

  get_references(bufnr, uri, pos, root_node)

  ---@type table<string, ReferenceNode>
  local all_nodes = {}
  for _, node in pairs(nodes) do
    all_nodes[func_key(node.func)] = node
  end

  for _, node in pairs(nodes) do
    for key, _ in pairs(node.children) do
      all_nodes[key] = nil
    end
  end

  for _, node in pairs(all_nodes) do
    print_tree(node)
  end

  for i = 1, math.floor(#flat_nodes / 2) do
    local j = #flat_nodes - i + 1
    flat_nodes[i], flat_nodes[j] = flat_nodes[j], flat_nodes[i]
  end

  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values

  local opts = {}

  pickers.new(opts, {
    prompt_title = "Reference Graph",
    finder = finders.new_table {
      results = flat_nodes,
      ---@param entry TelescopeNode
      entry_maker = function(entry)
        print(entry.file)
        return {
          value = entry,
          display = entry.display_name,
          ordinal = entry.ordinal,
          filename = vim.uri_to_fname(entry.file),
          lnum = entry.line,
          col = entry.col,
          text = entry.display_name,
        }
      end
    },
    sorter = conf.file_sorter(opts),
    previewer = conf.grep_previewer(opts),
  }):find()
end

return M
