
augroup talon
    autocmd!
    autocmd BufEnter * call TalonSetTitle()
    autocmd BufWritePost *.elm silent call TalonDocumentOpen()
    autocmd BufEnter * silent call TalonDocumentOpen()
    " autocmd InsertLeave,TextChanged * call TalonDocumentChange()
augroup END

" Make title readable by talon
function! TalonSetTitle()
    " let &titlestring ='VIM MODE:%{mode()} RPC:%{v:servername} | (%{getpos(".")[1]}, %{getpos(".")[2]}) | %F'
    let &titlestring ='VIM MODE:%{mode()} | (%{getpos(".")[1]},%{getpos(".")[2]}) | %t'
    " let &titlestring ='VIM MODE:%{mode()} RPC:%{v:servername} - (%f) %t'
    " let &titlestring ='VIM MODE:%{mode()} - (%f) %t'
    set title
endfunction

function TalonDocumentOpen()
    if &ft=="elm"
        execute "!curl 'localhost:8080/document-open?file=%:p'"
    else
        execute "!curl 'localhost:8080/document-close'"
    endif
endfunction

function TalonDocumentChange() " not used
    let start = getpos("'[")[1:2]
    let end = getpos("']")[1:2]
    echo start+end
endfunction
