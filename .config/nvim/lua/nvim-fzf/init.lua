vim.g.fzf_layout = {window = { width = 0.95, height = 0.95,yoffset = 0.5,xoffset = 0.5, highlight = 'Todo', border = 'sharp' } }
vim.cmd([[let $FZF_DEFAULT_OPTS = '--layout=reverse --info=inline'
	 let $FZF_DEFAULT_COMMAND = 'fd -t f -H -E .local -E .cache -E .git -E external -E .thunderbird -E .dot'
	 ]])
vim.g.fzf_colors = {
  fg      = { 'fg', 'Normal' },
  bg      = { 'bg', 'Normal' },
  hl      = { 'fg', 'Comment' },
  ['fg+'] = { 'fg', 'CursorLine', 'CursorColumn', 'Normal' },
  ['bg+'] = { 'bg', 'CursorLine', 'CursorColumn' },
  ['hl+'] = { 'fg', 'Statement' },
  info    = { 'fg', 'PreProc' },
  border  = { 'fg', 'Ignore' },
  prompt  = { 'fg', 'Conditional' },
  pointer = { 'fg', 'Exception' },
  marker  = { 'fg', 'Keyword' },
  spinner = { 'fg', 'Label' },
  header  = { 'fg', 'Comment' }
}
