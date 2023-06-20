vim.cmd [[packadd packer.nvim]]

return require("packer").startup(function(use)
    -- Packer itself
    use("wbthomason/packer.nvim")

    -- File browsing
    use("theprimeagen/harpoon")
    use {
        "nvim-telescope/telescope.nvim",
        tag = "0.1.1",
        requires = { { "nvim-lua/plenary.nvim" } }
    }

    use { "folke/tokyonight.nvim" }

    -- Linemode
    use {
        "nvim-lualine/lualine.nvim",
        requires = { "kyazdani42/nvim-web-devicons", opt = true },
    }

    -- Transparency
    use("xiyaowong/transparent.nvim")

    -- Undo history
    use("mbbill/undotree")

    -- Git integration
    use("tpope/vim-fugitive")

    -- Commenting
    use {
        "numToStr/Comment.nvim",
        config = function()
            require("Comment").setup()
        end
    }

    -- Trimming whitespaces
    use {
        "cappyzawa/trim.nvim",
        config = function()
            require("trim").setup({})
        end
    }

    -- Dealing with todos
    use {
        "folke/todo-comments.nvim",
        requires = { "nvim-lua/plenary.nvim", opt = true },
        config = function()
            require("todo-comments").setup()
        end
    }

    -- Diagnostics list
    use {
        "folke/trouble.nvim",
        requires = { "nvim-tree/nvim-web-devicons", opt = true }
    }

    -- Treesitter
    use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" }

    -- LSP integration
    use {
        "VonHeikemen/lsp-zero.nvim",
        branch = "v2.x",
        requires = {
            { "neovim/nvim-lspconfig" },
            {
                "williamboman/mason.nvim",
                run = function()
                    pcall(vim.cmd, "MasonUpdate")
                end
            },
            { "williamboman/mason-lspconfig.nvim" },
            { "hrsh7th/nvim-cmp" },
            { "hrsh7th/cmp-nvim-lsp" },
            { "L3MON4D3/LuaSnip" }
        }
    }

    -- Latex stuff
    use("lervag/vimtex")
end)
