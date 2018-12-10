# source ./haskell.fish


function exit-script
    echo ERROR $argv
    exit 1
end

function is-dir-src
    set dir (basename $PWD)
    set pdir (basename (realpath ..))

    ls .. | grep -q "$pdir.cabal"
    and test $dir = "src"
    or false # exit-script "Not in src folder."
end


#--
function printh
    bat -l haskell
end


#--

function preview
    set sp (string split ':' (echo "$argv" | awk '{print $1}'))
    test (count $sp) -ge 2
    and begin
        set file $sp[1]
        set line $sp[2]
        bat --color=always -l haskell --line-range $line:(math "$line + 50") $file
    end
end

function view
    fzf --ansi --reverse --multi --preview='fish -c "source ~/haskell.fish; and preview {}"'
end

function print-data
    egrep -nair "^data " . | printh
end

function print-instances
    egrep -nair "^instance " . | printh
end

function print-operators
    egrep -nair "^infix " . | printh
end

function print-classes
    egrep -nair "^class " . | printh
end

function print-classes
    egrep -nair "^class " . | printh
end

#is-dir-src
#and eval "$argv"




















