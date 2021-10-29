lua << EOF

require'nvim-treesitter.configs'.setup {
  -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = { "rust", "haskell", "c", "cpp", "python", "latex", "vim", "lua" },
  highlight = {
    custom_captures = {
      -- Highlight the @foo.bar capture group with the "Identifier" highlight group.
      ["foo.bar"] = "Identifier",
    },
    enable = true, -- false will disable the whole extension
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = { "rust", "haskell", "c", "cpp", "python", "latex", "vim", "lua" },
  },
  indent = {
      enable = false --{ "rust", "haskell", "c", "cpp", "python", "latex", "vim", "lua" },
  }
}

EOF
