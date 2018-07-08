" 配置 ctags 的参数
let g:gutentags_ctags_extra_args = ['--fields=+aimlS', '--languages=ruby', '--exclude="\.git"', '--exclude=node_modules', '--exclude=config', '--RUBY-kinds=+cfmS']

function! TmuxNewWindow(...)
  let options = a:0 ? a:1 : {}
  let text = get(options, 'text', '')
  let title = get(options, 'title', '')
  let directory = get(options, 'directory', getcwd())
  let method = get(options, 'method', 'new-window')
  let size = get(options, 'size', '40')
  let remember_pane = get(options, 'remember_pane', 0)
  let pane = ''

  if method == 'last'
    if !exists('s:last_tmux_pane') || empty(s:last_tmux_pane)
      echohl WarningMsg | echomsg "Can't find last tmux pane. Continue with 'horizontal-split'." | echohl None
      let method = 'h'
    else
      let pane = s:last_tmux_pane
    endif
  elseif method == 'quit'
    if !exists('s:last_tmux_pane') || empty(s:last_tmux_pane)
      echohl WarningMsg | echomsg "Can't find last used pane." | echohl None
      return
    else
      call system('tmux kill-pane -t ' . matchstr(s:last_tmux_pane, '%\d\+'))
      unlet! s:last_tmux_pane
      return
    endif
  endif

  if empty(pane) && method != 'new-window'
    " use splitted pane if available
    let pane = matchstr(
          \   system('tmux list-pane -F "#{window_id}#{pane_id}:#{pane_active}" | egrep 0$'),
          \   '\zs@\d\+%\d\+\ze'
          \ )
  endif

  if empty(pane)
    if method == 'new-window'
      let cmd = 'tmux new-window -a '
            \ . (empty(title) ? '' : printf('-n %s', shellescape(title)))
            \ . printf(' -c %s', shellescape(directory))
    elseif method == 'v'
      let cmd = 'tmux split-window -d -v '
            \ . printf('-p %s -c %s ', size, shellescape(directory))
    elseif method == 'h'
      let cmd = 'tmux split-window -d -h '
            \ . printf(' -c %s ', shellescape(directory))
    endif

    let pane = substitute(
          \   system(cmd . ' -P -F "#{window_id}#{pane_id}"'), '\n$', '', ''
          \ )
  endif

  if remember_pane
    let s:last_tmux_pane = pane
  endif

  let window_id = matchstr(pane, '@\d\+')
  let pane_id = matchstr(pane, '%\d\+')

  if !empty(text)
    let cmd = printf(
          \   'tmux set-buffer %s \; paste-buffer -t %s -d \; send-keys -t %s Enter',
          \   shellescape(text),
          \   pane_id,
          \   pane_id
          \ )
    sleep 300m
    call system('tmux select-window -t ' . window_id)
    call system(cmd)
  endif
endfunction
" }}}

function! s:rspec_test_tmux(method)
  let type = rails#buffer().type_name()
  let line_num = get(s:, 'last_test_line', '')
  let path = get(s:, 'last_test_path', '')

  if (type =~ '^test' || type =~ '^spec') && a:method != 'last'
    let path = rails#buffer().relative()
    let line_num = line('.')
    let s:last_test_line = line_num
    let s:last_test_path = path
  end

  if a:method == 'all_file'
    let test_command = printf('rspec %s', path)
  else
    let test_command = printf('rspec %s:%s', path, line_num)
  endif

  call TmuxNewWindow({
        \   "text": test_command,
        \   "directory": rails#app().root,
        \   "remember_pane": 1,
        \   "method": a:method
        \ })
endfunction

" Rails {{{
function! s:rails_test_helpers()
  let type = rails#buffer().type_name()
  let relative = rails#buffer().relative()
  if type =~ '^test' || type =~ '^spec' || (type == 'javascript-coffee' && relative =~ '^test/')
    nmap \t [rtest]
    nnoremap <silent> [rtest]j :call <SID>rspec_test_tmux('v')<CR>
    nnoremap <silent> [rtest]l :call <SID>rspec_test_tmux('h')<CR>
    nnoremap <silent> [rtest]t :call <SID>rspec_test_tmux('last')<CR>
    nnoremap <silent> [rtest]w :call <SID>rspec_test_tmux('new-window')<CR>
    nnoremap <silent> [rtest]q :call <SID>rspec_test_tmux('quit')<CR>
    nnoremap <silent> [rtest]a :call <SID>rspec_test_tmux('all_file')<CR>
    nnoremap <silent> [rtest]t :call <SID>rspec_test_tmux('last')<CR>
  endif
endfunction

autocmd User Rails call s:rails_test_helpers()
" }}}

