function! SourceIfExists(file)
  let file = expand(a:file)
  if file_readable(file)
    exec 'source' file
  endif
endfunction

function! SourceParts(prefix, parts)
  let f = [a:prefix]
  for part in a:parts
    let f += [part]
    call SourceIfExists(join(f, '.'))
  endfor
endfunction

function! SourceOSFiles(prefix)
  let OS = GetOS()
  let parts = [OS[0]] + split(OS[1], '\.')
  call SourceParts( a:prefix, parts )
endfunction

function! SourceHostFiles(prefix)
  let parts = split(tolower(hostname()), '\.')
  call SourceParts( a:prefix, parts )
endfunction

function! SourceCustomFiles(prefix)
  call SourceOSFiles( a:prefix . '.' . 'os' )
  call SourceHostFiles( a:prefix . '.' . 'host' )
endfunction
