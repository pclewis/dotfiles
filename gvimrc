" colors
colorscheme candy
hi Comment guifg=#ff99cc

" syntax highlight always on
syntax on

"antialias font
set antialias

" line numbers
set number

" no toolbar
set guioptions-=T

" horizontal scoll bar
set guioptions+=b

" copy to clipboard
set cb=unnamed

" subtle color of current line/col
hi CursorLine guibg=#202020
hi CursorColumn guibg=#101010
" TODO: figure out how to make this not nuke other backgrounds
set cursorline
" set cursorcolumn

hi User1 guibg=#00FF00 guifg=#000000
hi User2 guibg=#FF0000 guifg=#000000

call SourceCustomFiles("~/dotfiles/gvimrc")
