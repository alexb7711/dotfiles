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
	if has('nvim-0.5')
	Plug 'neovim/nvim-lspconfig'
	Plug 'nvim-lua/completion-nvim'
	endif
	Plug 'taketwo/vim-ros'

	" Utility
	Plug 'airblade/vim-gitgutter'
	Plug 'yinflying/matlab.vim'
	Plug 'godlygeek/tabular'
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'
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
" Auto Complete Hotkeys

" Autocompile
autocmd BufWrite *.md,*.markdown,*.tex :exec 'AsyncRun compile %:p'
autocmd BufWrite *.puml :exec 'AsyncRun compile %:p'

" Auto Correct Spelling
autocmd BufEnter,FocusGained,InsertLeave *.md,*.markdown,*.tex nnoremap <silent> <space><space> ms[s1z=`s
autocmd BufEnter,FocusGained,InsertLeave *.puml nnoremap <silent> <space><space> :vsplit<CR>:e %:r.utxt<CR>

" Auto Save When Leaving Buffer
au BufLeave * silent! wall

" Disable Word Wrap
autocmd BufEnter,FocusGained,InsertLeave *.cpp,*.c,*.h,*.hpp,*.vim set nowrap

" Compile Hotkey
autocmd Filetype python nnoremap <buffer> <F9> :update<bar>!python %<CR>
autocmd Filetype python nnoremap <buffer> <F9> :update<bar>!python %<CR>

" Open Complimenting C/H File
autocmd BufEnter,FocusGained,InsertLeave *.cpp,*.c nnoremap <silent> <space><space> :call OpenOther()<CR>

" Reload Document when window gains focus
autocmd FocusGained,BufEnter * :silent! !

" Save Sessions
autocmd BufWrite *.cpp,*.c,*.h,*.hpp,*.py :mksession! .vs

" Searching
autocmd FileType c,cpp,h,hpp nnoremap <buffer> <space>w yiw:silent<space>grep!<space>-Ri<space>"<C-r>0"<space>*.c<space>*.h<CR>:cope<CR><C-l>
autocmd FileType c,cpp,h,hpp nnoremap <buffer> <space>s :silent grep!<space>-Ri<space>""<space>*.c<space>*.h<C-l><left><left><left><left><left>
autocmd FileType py nnoremap <buffer> <space>w yiw:silent<space>grep!<space>-Ri<space>"<C-r>0"<space>*.py<CR>:cope<CR><C-l>
autocmd FileType py nnoremap <buffer> <space>s :silent grep!<space>-Ri<space>""<space>*.py<C-l><left><left><left>
    
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

" Reload Syntax
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

" Reload Vim
nnoremap <space>rv :source ~/.config/nvim/init.vim<CR>

" Search and Replace
command! -nargs=* SAR call SearchAndReplace(<f-args>)

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

" Project Search and Replace
function! SearchAndReplace(search,replace)
	silent execute "grep! -R " . a:search . " ."
	copen
	execute 'cfdo! %s/' . a:search . '/'. a:replace . '/gc' 
endfunction

"==============================================================================="
" INDENTATION "
"==============================================================================="

" C/C++ and YAML indent style
autocmd Filetype cpp,c,h,hpp,yaml setlocal expandtab shiftwidth=2 softtabstop=2

" C/C++, YAML, XML indent style
autocmd BufEnter,BufNewFile,BufRead *.launch setlocal expandtab shiftwidth=2 softtabstop=2

" Matlab indent style
autocmd BufEnter *.m compiler mlint
" autocmd Filetype matlab setlocal shiftwidth=4 tabstop=4

"==============================================================================="
" PLUGGINS 
"==============================================================================="
" Deoplete

" Auto swap completion sources
let g:completion_auto_change_source = 1

" Avoid showing message extra message when using completion
set shortmess+=c

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Use completion-nvim in every buffer

" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" IndentLine
let g:indentLine_char = '¦'

" Nerd Commentor Settings
let g:NERDSpaceDelims = 1
let g:NERDToggleCheckAllLines = 1

" Nvim LSP 
if has('nvim-0.5')

autocmd BufEnter * lua require'completion'.on_attach()

lua << EOF
require'lspconfig'.bashls.setup{}
require'lspconfig'.clangd.setup{}
require'lspconfig'.pyls.setup{}
require'lspconfig'.texlab.setup{}
require'lspconfig'.vimls.setup{}

local nvim_lsp = require('lspconfig')
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)

  -- Set some keybinds conditional on server capabilities
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  end

  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      augroup lsp_document_highlight
        autocmd!
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end
end

-- Use a loop to conveniently both setup defined servers 
-- and map buffer local keybindings when the language server attaches
local servers = { "bashls", "clangd", "pyls", "texlab", "vimls" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach }
end
EOF
endif

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
" SKELETON
"==============================================================================="

" Markdown Notes
autocmd BufNewFile *.md r ~/Templates/Markdown/skeleton_notes

" Markdown Slides
autocmd BufNewFile *.markdown r ~/Templates/Markdown/skeleton_slides

" Python
autocmd BufNewFile *.py r ~/Templates/Python/skeleton


"==============================================================================="
" SNIPPETS
"==============================================================================="
nnoremap <space>;post :r ~/Templates/blogPost.md<Enter>kddjj2w
nnoremap <space>;note :r ~/Templates/Markdown/notes

"==============================================================================="
" WORKSPACE MANAGEMENT 
"==============================================================================="
" Scrolling 
" Vertically 
nnoremap <S-k> <C-y>
nnoremap <S-j> <C-e>

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
