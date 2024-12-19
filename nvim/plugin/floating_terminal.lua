local terminal = {
    buf = -1,
    win = -1,
}

local function make_floating_terminal(terminal_buf)
    local instance_columns = vim.o.columns
    local instance_lines   = vim.o.lines

    local win_config    = {}
    win_config.relative = "editor"
    win_config.width    = math.floor(instance_columns * 0.8)
    win_config.height   = math.floor(instance_lines * 0.8)
    win_config.col      = math.floor((instance_columns - win_config.width) / 2.0)
    win_config.row      = math.floor((instance_lines - win_config.height) / 2.0)
    win_config.style    = "minimal"
    win_config.border   = "rounded"

    local buf = vim.api.nvim_buf_is_valid(terminal_buf) and terminal_buf or vim.api.nvim_create_buf(false, true)
    local win = vim.api.nvim_open_win(buf, true, win_config)

    if vim.bo[buf].buftype ~= "terminal" then
        vim.cmd.terminal()
    end

    return { buf = buf, win = win }
end

local function toggle_floating_terminal_visibility()
    if not vim.api.nvim_win_is_valid(terminal.win) then
        terminal = make_floating_terminal(terminal.buf)
    else
        vim.api.nvim_win_hide(terminal.win)
    end
end

vim.api.nvim_create_user_command("FloatingTerminalToggle", toggle_floating_terminal_visibility, {})
