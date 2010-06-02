let s:os = []
" Return OS and version/distro, ex:
"  ['Windows', 'XP']
"  ['Linux', 'gentoo']
"  ['MacOS', '10.6']
function! GetOS()
  if s:os == []
    let s:os = s:GetOS()
  endif
  return s:os
endfunction

function! s:GetOS()
  let uname = system('uname')
  if v:shell_error != 0       " Probably Windows
    let ver = system('ver')
    if stridx(ver, 'Version 5.1') != -1
      return ['windows', 'xp']
    elseif stridx(ver, 'Version 6.0') != -1
      return ['windows', 'vista']
    elseif stridx(ver, 'Version 6.1') != -1
      return ['windows', '7']
    else
      return ['unknown', 'unknown']
    endif
  elseif uname=="Darwin\n"
    let ver = system('sw_vers -productVersion')
    let m = matchstr(ver, '^\d\+\.\d\+')
    return ['macos', (v:shell_error != 0 || m == "") ? 'unknown' : m]
  elseif uname=="FreeBSD\n"
    let ver = system('uname -r')
    let m = matchstr(ver, '^\d\+\.\d\+')
    return ['freebsd', (v:shell_error != 0 || m == "") ? 'unknown' : m]
  elseif uname=="Linux\n"
    let sub = "unknown"
    let release = glob('/etc/*-release').glob('/etc/*_release').glob('/etc/*-version').glob('/etc/*_version')
    let m = matchlist(release, '/etc/\([a-z]\+\)')
    if m != []
      let sub = tolower(m[1])
      if sub == 'lsb'
        let sub = 'ubuntu'
      endif
    endif
    return ['linux', sub]
  endif
endfunction
