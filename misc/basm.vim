au BufRead,BufNewFile *.basm set filetype=basm

command! -nargs=1 BasmJumpToAddr call BasmJumpToAddr(<f-args>)

function! BasmTokenizeLine(line)
  let tokens = matchlist(a:line, '\v^\s*%(\w+:\s*)*(\w+)?\s*(\()?(\w+)?')
  return {
    \ 'cmd': tokens[1],
    \ 'has_indirect_addr': (tokens[2] == '('),
    \ 'addr': tokens[3] }
endfunction

function! BasmJumpToAddr(addr)
  let current_addr = 0
  let target_addr = str2nr(a:addr, 16)
  
  let line_no_iter = 1

  while line_no_iter != 0
    let contents = BasmTokenizeLine(getline(line_no_iter))
    
    if contents['cmd'] == 'ORG'
      let current_addr = str2nr(contents['addr'], 16) - 1
    elseif !empty(contents['cmd'])
      let current_addr += 1
    endif

    if current_addr == target_addr
      execute printf('normal %dG', line_no_iter)
      return
    endif

    let line_no_iter = nextnonblank(line_no_iter + 1)
  endwhile

  echo 'Адрес ' . a:addr . ' в программе не найден'
endfunction

function! BasmPrintAddr()
  let start_addr = '000'
  let commands_before = 0

  let current_line = line('.')

  while current_line > 0
    let current_line -= 1
    let contents = BasmTokenizeLine(getline(current_line))

    if empty(contents['cmd'])
      continue
    elseif contents['cmd'] == 'ORG'
      let start_addr = contents['addr']
      break
    else
      let commands_before += 1
    endif
  endwhile

  let start_addr_dec = str2nr(start_addr, 16)
  let line_addr = printf('%03X', start_addr_dec + commands_before)

  echo 'Адрес: ' . line_addr
endfunction

function! BasmJumpToLabel()
  let label = BasmTokenizeLine(getline('.'))['addr']

  if !empty(label)
    execute search('\<' . label . ':')
  endif
endfunction
