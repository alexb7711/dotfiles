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

" Clear previous auto commands
autocmd!

set nocompatible 
filetype plugin on
filetype plugin indent on
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
	Plug 'Yggdroot/indentLine'
	Plug 'albertomontesg/lightline-asyncrun'
	Plug 'itchyny/lightline.vim'
	Plug 'mhinz/vim-startify'
	Plug 'morhetz/gruvbox'
	Plug 'shinchu/lightline-gruvbox.vim'

	" LSP & Auto Complete
	"Plug 'Shougo/deoplete-lsp'
	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
	Plug 'alexb7711/deoplete-lsp'
	Plug 'neovim/nvim-lspconfig'
	Plug 'taketwo/vim-ros'

	" Utility
	Plug 'airblade/vim-gitgutter'
	Plug 'daeyun/vim-matlab', { 'do': ':UpdateRemotePlugins' }
	Plug 'godlygeek/tabular'
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'skywind3000/asyncrun.vim'
	Plug 'tpope/vim-fugitive'
	Plug 'vim-scripts/c.vim'
	
	" File Management
	Plug 'vifm/vifm.vim'
	Plug 'vimwiki/vimwiki'

	" Syntax
	Plug 'aklt/plantuml-syntax'
	Plug 'lervag/vimtex'
	Plug 'scrooloose/nerdcommenter'

call plug#end()

"==============================================================================="
" AUTO COMMANDS
"==============================================================================="

" Autocompile
autocmd BufWrite *.md,*.markdown,*.tex :exec 'AsyncRun compile %:p'
autocmd BufWrite *.puml :exec 'AsyncRun compile %:p'

" Auto Correct Spelling
autocmd BufEnter,FocusGained,InsertLeave *.md,*.markdown,*.tex nnoremap <silent> <space><space> ms[s1z=`s
autocmd BufEnter,FocusGained,InsertLeave *.cpp,*.c nnoremap <silent> <space><space> :call OpenOther()<CR>
autocmd BufEnter,FocusGained,InsertLeave *.puml nnoremap <silent> <space><space> :split<CR>:e %:r.utxt<CR>

" Disable Word Wrap
autocmd BufEnter,FocusGained,InsertLeave *.cpp,*.c,*.h,*.hpp,*.vim set nowrap

" Compile Hotkey
autocmd Filetype python nnoremap <buffer> <F9> :update<bar>!python %<CR>
autocmd Filetype python nnoremap <buffer> <F9> :update<bar>!python %<CR>

" Reload Document when window gains focus
autocmd FocusGained,BufEnter * :silent! !

" Save Sessions
autocmd BufWrite *.cpp,*.c,*.h,*.hpp,*.py :mksession! .vs
    
" Sets numbering style on the left hand side
set number relativenumber
autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber

" Spell Check
autocmd BufRead,BufNewFile *.md,*.markdown,*.tex setlocal spell

" Enable XML Syntax Highlighting
autocmd BufRead,BufNewFile *.launch set syntax=xml

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
autocmd Filetype * inoremap <buffer> <C-j> <C-n>
autocmd Filetype * inoremap <buffer> <C-k> <C-p>

" inoremap <buffer> <C-j> <C-n>
" inoremap <buffer> <C-k> <C-p>

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

" Open current pdf in zathura
nnoremap <silent> <space>z :!zathura %:r.pdf&<CR>

" Reload Vim
nnoremap <space>rv :source ~/.config/nvim/init.vim<CR>

" Searching
augroup search
	autocmd!
	autocmd FileType c,cpp,h,hpp nnoremap <buffer> <space>w yiw:silent<space>grep!<space>-Ri<space>"<C-r>0"<space>*.c<space>*.h<CR>:cope<CR><C-l>
	autocmd FileType c,cpp,h,hpp nnoremap <buffer> <space>s :silent grep!<space>-Ri<space>""<space>*.c<space>*.h<C-l><left><left><left><left><left>
augroup END

" Terminal
tnoremap <Esc><Esc> <C-\><C-n>
tnoremap <F12> <C-\><C-n>:q!<CR>
nnoremap <silent> <space>ts :vsplit term://zsh<CR>

" Toggle Sidebar
nnoremap <space>sb :20Lex<CR>

"==============================================================================="
" FUNCTIONS "
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

" C/C++ and YAML indent style
autocmd Filetype cpp,c,h,hpp,yaml setlocal expandtab shiftwidth=2 softtabstop=2

" C/C++, YAML, XML indent style
autocmd BufEnter,BufNewFile,BufRead *.launch setlocal expandtab shiftwidth=2 softtabstop=2

" Matlab indent style
autocmd Filetype matlab setlocal shiftwidth=4 tabstop=4

"==============================================================================="
" PLUGGINS 
"==============================================================================="
" Deoplete
let g:deoplete#enable_at_startup = 1

" IndentLine
let g:indentLine_char = '¦'

" Matlab
" Open Split in Vim
let g:matlab_server_launcher = 'vim'  "launch the server in a Neovim terminal buffer

" Nerd Commentor Settings
let g:NERDSpaceDelims = 1
let g:NERDToggleCheckAllLines = 1

" Nvim LSP 
lua << END
require'lspconfig'.bashls.setup{}
require'lspconfig'.clangd.setup{}
require'lspconfig'.pyls.setup{}
require'lspconfig'.texlab.setup{}
require'lspconfig'.vimls.setup{}
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

" ros-vim
let g:ycm_semantic_triggers = {
\   'roslaunch' : ['="', '$(', '/'],
\   'rosmsg,rossrv,rosaction' : ['re!^', '/'],
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

" Vimtex
let g:tex_flavor = 'latex'

" VimWiki
let g:vimwiki_list = [{'path': '~/Documents/Wiki/src/',
		      \ 'path_html': '~/Documents/Wiki/html/',
		      \ 'syntax': 'markdown', 'ext': '.md',}]

let g:vimwiki_global_ext = 0

nnoremap <silent> <space>vwt :VimwikiTable<CR>

"==============================================================================="
" SNIPPETS
"==============================================================================="

nnoremap <space>post :r ~/Templates/blogPost.md<Enter>kddjj2w
nnoremap <space>note :r ~/Templates/Markdown/notes

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
