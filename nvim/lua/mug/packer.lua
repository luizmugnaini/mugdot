vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  -- Packer itself
  use("wbthomason/packer.nvim")

  -- File browsing
  use("theprimeagen/harpoon")
  use({
    'nvim-telescope/telescope.nvim', tag = '0.1.1',
    requires = { {'nvim-lua/plenary.nvim'} }
  })

  -- Theme
  use({
    "sainnhe/gruvbox-material",
    as = "gruvbox-material",
    config = function()
	    vim.cmd("colorscheme gruvbox-material")
    end
  })

  -- Linemode
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'nvim-tree/nvim-web-devicons', opt = true }
  }

  -- Transparency
  use("xiyaowong/transparent.nvim")

  -- Treesitter
  use("nvim-treesitter/nvim-treesitter", { run = ":TSUpdate" })

  -- Undo history
  use("mbbill/undotree")

  -- Git integration
  use("tpope/vim-fugitive")

  -- Commenting
  use {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end
  }

  -- LSP integration 
  use {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    requires = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {                                      -- Optional
        'williamboman/mason.nvim',
        run = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      {'williamboman/mason-lspconfig.nvim'}, -- Optional
  
      -- Autocompletion
      {'hrsh7th/nvim-cmp'},     -- Required
      {'hrsh7th/cmp-nvim-lsp'}, -- Required
      {'L3MON4D3/LuaSnip'},     -- Required
    }
  }
end)

--[[ TODO:
Plug 'SirVer/ultisnips'
    let g:UltiSnipsExpandTrigger='<tab>'
    let g:UltiSnipsJumpForwardTrigger='<tab>'
    let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

Plug 'lervag/vimtex'
    let g:tex_flavor='latex'
    let g:vimtex_view_method='zathura'
    let maplocalleader=' '
    let g:vimtex_quickfix_mode=0
    let g:vimtex_compiler_progname='/home/luiz/.local/bin/nvr'

Plug 'KeitaNakamura/tex-conceal.vim'
    set conceallevel=0
    let g:tex_conceal='abdmg'
    hi Conceal ctermbg=none
--]]
