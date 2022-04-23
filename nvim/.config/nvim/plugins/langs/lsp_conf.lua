vim.o.completeopt = 'menuone,noselect'

-- Setup nvim-cmp.
local cmp = require('cmp')

local source_mapping = {
  nvim_lsp = "[LSP]",
  --cmp_tabnine = "[TN]",
  path = "[Path]",
  ultisnips = "[Snip]"
}
local lspkind = require('lspkind')

cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body)
    end,
  },
  mapping = {
    ['<C-n>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
 		["<C-u>"] = cmp.mapping.scroll_docs(-4),
		["<C-d>"] = cmp.mapping.scroll_docs(4),
    ['<C-p>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
    --['<C-Space>'] = cmp.mapping.complete(),
    ["<C-Space>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        if vim.fn["UltiSnips#CanExpandSnippet"]() == 1 then
          return press("<C-R>=UltiSnips#ExpandSnippet()<CR>")
        end

        cmp.select_next_item()
      elseif has_any_words_before() then
        press("<Space>")
      else
        fallback()
      end
    end, {
      "i",
      "s",
    }),
    ['<C-e>'] = cmp.mapping.close(),
    ['<Tab>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    }),
  },

  --formatting = {
		--format = function(entry, vim_item)
			--vim_item.kind = lspkind.presets.default[vim_item.kind]
			--local menu = source_mapping[entry.source.name]
			--if entry.source.name == "cmp_tabnine" then
				--if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
					--menu = entry.completion_item.data.detail .. " " .. menu
				--end
				--vim_item.kind = ""
			--end
			--vim_item.menu = menu
			--return vim_item
		--end,
	--},

  sources = {
    --{ name = "cmp_tabnine" },
    { name = 'nvim_lsp' },
    { name = 'buffer' },
    { name = 'ultisnips' },
    { name = 'path' },
  }
})

--local tabnine = require("cmp_tabnine.config")
--tabnine:setup({
	--max_lines = 1000,
	--max_num_results = 20,
	--sort = true,
	--run_on_every_keystroke = true,
	--snippet_placeholder = "..",
--})

-- Setup nvim lspconfig
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- Setup of my servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

require('lspconfig').clangd.setup {
  capabilities = capabilities,
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  }
}

require('lspconfig').rust_analyzer.setup {
  capabilities = capabilities,
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  settings = {
      ["rust-analyzer"] = {
          assist = {
              importGranularity = "module",
              importPrefix = "by_self",
          },
          cargo = {
              loadOutDirsFromCheck = true
          },
          procMacro = {
              enable = true
          },
      }
  }
}

require('lspconfig').pyright.setup {
  capabilities = capabilities,
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  }
}

require('lspconfig').hls.setup {
  capabilities = capabilities,
  on_attach = on_attach,
}
