"==============================================================================="
"  _   _ _____ _____     _____ __  __ 
" | \ | | ____/ _ \ \   / /_ _|  \/  |
" |  \| |  _|| | | \ \ / / | || |\/| |
" | |\  | |__| |_| |\ V /  | || |  | |
" |_| \_|_____\___/  \_/  |___|_|  |_|
"==============================================================================="
                                    
"==============================================================================="
" VIM CONFIGURATION 
"==============================================================================="
set nocompatible 
filetype plugin on
syntax on

" Indenting
set autoindent

"Enable Mouse support
set mouse=a

" Show commands 
set showcmd

" Highlight connecting brackets
set showmatch

" Searching
set path+=**
set wildmenu

call plug#begin()

	" Aesthetics
"	Plug 'albertomontesg/lightline-asyncrun'
"	Plug 'itchyny/lightline.vim'
"	Plug 'mhinz/vim-startify'
"	Plug 'skywind3000/asyncrun.vim'
"	Plug 'morhetz/gruvbox'
"	Plug 'shinchu/lightline-gruvbox.vim'
"	Plug 'junegunn/goyo.vim'
"	Plug 'junegunn/limelight.vim'
"
"	" LSP & Auto Complete
"	" Plug 'neoclide/coc.nvim', {'branch': 'release'}
"	Plug 'neovim/nvim-lsp'
"	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"	Plug 'Shougo/deoplete-lsp'
"
"	" Utility
"	Plug 'airblade/vim-gitgutter'
"	Plug 'godlygeek/tabular'
"	Plug 'skywind3000/asyncrun.vim'
"	Plug 'tpope/vim-fugitive'
"	Plug 'vim-scripts/c.vim'
"	
"	" File Management
"	Plug 'vimwiki/vimwiki'
"	Plug 'vifm/vifm.vim'
"
"	" Syntax
"	Plug 'dart-lang/dart-vim-plugin'
"	Plug 'lervag/vimtex'
"	Plug 'thosakwe/vim-flutter'
"	Plug 'scrooloose/nerdcommenter'

call plug#end()

"==============================================================================="
" AUTO COMMANDS
"==============================================================================="

" Autocompile
" autocmd BufWrite *.md :silent exec 'AsyncRun compile %:p'
autocmd BufWrite *.md,*.tex :exec 'AsyncRun compile %:p'

" Auto Correct Spelling
autocmd BufEnter,FocusGained,InsertLeave *.md,*.tex nnoremap <silent> <space><space> ms[s1z=`s
autocmd BufEnter,FocusGained,InsertLeave *.cpp,*.c nnoremap <silent> <space><space> :call OpenOther()<CR>

" Disable Word Wrap
autocmd BufEnter,FocusGained,InsertLeave *.cpp,*.c,*.h,*.hpp,*.vim set nowrap

" Goyo
autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width = 80 
autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo

" Limelight
autocmd! User GoyoEnter Limelight
" autocmd! User GoyoLeave Limelight!
autocmd! User GoyoLeave source ~/.config/nvim/init.vim

" Save Sessions
autocmd BufWrite *.cpp :mksession! .vs
autocmd BufWrite *.c   :mksession! .vs
autocmd BufWrite *.h   :mksession! .vs
autocmd BufWrite *.hpp :mksession! .vs

" Sets numbering style on the left hand side
:set number relativenumber
:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

" Spell Check
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.tex setlocal spell

"==============================================================================="
" COLORS
"==============================================================================="

"Color Scheme Settings
set termguicolors
set background=dark
colorscheme gruvbox
hi Normal guibg=NONE ctermbg=NONE

" Cursor Line
"set cursorline
"highlight CursorLine ctermbg=DarkGrey cterm=none

" Toggle Search Highlighting
set hlsearch!
nnoremap <F3> :set hlsearch!<CR>

" Vertical Bar
set colorcolumn=81

"==============================================================================="
" COMMANDS
"==============================================================================="

" Auto Complete Hotkeys
inoremap <C-j> <C-n>
inoremap <C-k> <C-p>

" Ctags
command! MakeTags !ctags -R .

" Copy/Paste to clipboard
vnoremap <space>y "+y
nnoremap <space>Y "+yg_
nnoremap <space>y "+y
nnoremap <space>yy "+yy

nnoremap <space>p "+p
nnoremap <space>P "+P
vnoremap <space>p "+p
vnoremap <space>P "+P

" Goyo
nnoremap <space>gg :Goyo \| set linebreak<CR>

" Open current pdf in zathura
nnoremap <silent> <space>z :!zathura %:r.pdf&<CR>

" Reload Vim
nnoremap <space>rv :source ~/.config/nvim/init.vim<CR>

" Terminal
tnoremap <Esc><Esc> <C-\><C-n>
tnoremap <F12> <C-\><C-n>:q!<CR>
nnoremap <silent> <space>ts :vsplit term://zsh<CR>

" Toggle Sidebar
nnoremap <space>sb :20Lex<CR>

"==============================================================================="
" INDENTATION "
"==============================================================================="
" Swap between h and c files
function! OpenOther()
  if expand("%:e") == "cpp"
    :e %:r.hpp
  elseif expand("%:e") == "hpp"
    :e %:r.cpp
  elseif expand("%:e") == "c"
    :e %:r.h
  elseif expand("%:e") == "h"
    :e %:r.c
  endif
endfunction

"==============================================================================="
" INDENTATION "
"==============================================================================="

" C/C++ indent style
autocmd Filetype cpp setlocal expandtab shiftwidth=2 softtabstop=2
autocmd Filetype hpp setlocal expandtab shiftwidth=2 softtabstop=2
autocmd Filetype c setlocal expandtab shiftwidth=2 softtabstop=2
autocmd Filetype h setlocal expandtab shiftwidth=2 softtabstop=2

" Dart indent style
autocmd Filetype dart setlocal expandtab shiftwidth=2 softtabstop=2

"==============================================================================="
" PLUGGINS 
"==============================================================================="
" Deoplete
let g:deoplete#enable_at_startup = 1

" Limelight 
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_default_coefficient = 0.5

" Nerd Commentor Settings
let g:NERDSpaceDelims = 1
let g:NERDToggleCheckAllLines = 1

" Nvim LSP 
lua << END
require'nvim_lsp'.bashls.setup{}
require'nvim_lsp'.clangd.setup{}
require'nvim_lsp'.dartls.setup{}
require'nvim_lsp'.pyls.setup{}
require'nvim_lsp'.texlab.setup{}
require'nvim_lsp'.vimls.setup{}
END

nnoremap <silent> gd        <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> <c-]>     <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> <space>k  <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gi        <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k>     <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD       <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr        <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> <space>rn <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> g0        <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW        <cmd>lua vim.lsp.buf.workspace_symbol()<CR>

" Lightline
let g:lightline = {
	\ 'colorscheme': 'gruvbox',
	\ 'active': {
	\   'left': [ [ 'mode', 'paste' ],
	\             ['readonly', 'filename', 'modified', 'fileformat'] ],
	\   'right': [ [ 'lineinfo' ],
	\              [ 'percent' ],
	\	       ['asyncrun_status', 'gitbranch']],
	\ },
	\ 'component_function': {
	\   'gitbranch': 'FugitiveHead',
	\ },
\ }

let g:lightline.component_expand = {
        \ 'asyncrun_status': 'lightline#asyncrun#status',
        \ }

" Startify
let g:startify_custom_header = [
	\'	 _   _ _____ _____     _____ __  __ ',
	\'	| \ | | ____/ _ \ \   / /_ _|  \/  |',
	\'	|  \| |  _|| | | \ \ / / | || |\/| |',
	\'	| |\  | |__| |_| |\ V /  | || |  | |',
	\'	|_| \_|_____\___/  \_/  |___|_|  |_|',
	\ ]

" Vifm.vim
nnoremap <space>ff :Vifm<CR>
nnoremap <space>fv :VsplitVifm<CR>
nnoremap <space>fh :SplitVifm<CR>
nnoremap <space>ft :TabVifm<CR>

" VimWiki
let g:vimwiki_list = [{'path': '~/Documents/Wiki/src/',
                      \ 'path_html': '~/Documents/Wiki/html/',
		      \ 'syntax': 'markdown', 'ext': '.md',}]

nnoremap <silent> <space>vwt :VimwikiTable<CR>

"==============================================================================="
" SNIPPETS
"==============================================================================="

nnoremap <leader>post :r ~/Templates/blogPost.md<Enter>kddjj2w

"==============================================================================="
" WORKSPACE MANAGEMENT 
"==============================================================================="
" Scrolling 
" Vertically 
nnoremap <S-k> <C-y>
nnoremap <S-j> <C-e>
" Horizontally
set sidescroll=1
nnoremap <S-l> zl
nnoremap <S-h> zh

" Create window panes
nnoremap <space>sv :vsplit<Enter>
nnoremap <space>sh :split<Enter>

" Change size of window panes
nnoremap <silent> <Up> <C-w>+
nnoremap <silent> <Down> <C-w>-
nnoremap <silent> <Right> <C-w><
nnoremap <silent> <Left> <C-w>>

" Changing window pane
nnoremap <space>h <C-w>h
nnoremap <space>l <C-w>l
nnoremap <space>k <C-w>k
nnoremap <space>j <C-w>j

" Create/Close/Duplicate Tabs
nnoremap <space>tt :tabe<CR>
nnoremap <space>tc :tabclose<CR>
nnoremap <space>td :let @" = expand("%")<CR>:tabe<CR>:e <C-r>"<CR>

" Change Tabs
nnoremap <C-h> gT
nnoremap <C-l> gt 
