#!/usr/bin/env fish

# utility stuff
alias list-func='cat ide.fish | egrep \'^(function\ |#)\''
alias slashToDot="string replace --all '/' '.'"

function ign
    eval $argv >/dev/null
end

function drop
    tail -n +(math "$argv[1] + 1")
end

function list-tail
    switch (count $argv)
        case 0
        case 1
            echo ''
        case '*'
            echo $argv[2..-1]
    end
end

function print-argv
    for x in $argv
        echo $x
    end
end

function str-non-empty
    string length $argv >/dev/null
end

function read-confirm
    while true
        read -l -P $argv[1]' [y/N] ' confirm

        switch $confirm
            case Y y
                return 0
            case '' N n
                return 1
        end
    end
end

function select-col
    switch $argv[1]
        case -1
            awk '{print $NF}'
        case '*'
            awk '{print $'$argv[1]'}'
    end
end

function select-tree
    tree -CDFtrf $argv[1] | fz --multi --preview='cat $(echo {} | perl -ne \'/]  (.*)/; print "$1"\') | nl' --preview-window=right:50% | read -l selection
    and echo $selection | perl -ne '/]  (.*)/; print "$1"'
end

function cmp-path
    test (realpath $argv[1]) = (realpath $argv[2])
end

function rm-empty-parents
    set dir (dirname $argv[1])
    while not count (ls $dir) >/dev/null ^/dev/null
        cmp-path $dir $argv[2]
        and break
        rm -d $dir
        set dir (dirname $dir)
    end
end

function editor-args
    set MICRO_ARGS -softwrap true
    set MICRO_ARGS $MICRO_ARGS -autosave false
    set MICRO_ARGS $MICRO_ARGS -colorscheme atom-dark-tc
    set MICRO_ARGS $MICRO_ARGS -ignorecase true
    set MICRO_ARGS $MICRO_ARGS -tabstospaces true
    # set MICRO_ARGS $MICRO_ARGS 
    echo $MICRO_ARGS
end

function editor-cmd
    echo micro (editor-args)
end

function editor
    # micro (editor-args) $argv
    micro $argv
end

# vcs functionality
function vcs-log
    set COMMIT_ID_MATCHER '/[^\w\d]+ ([\w\d]+) /; print "$1\n"'
    set FZF_ARGS --ansi --multi --reverse --preview='git diff --color $(echo {} | perl -ne \''$COMMIT_ID_MATCHER'\') | diff-so-fancy'
    set GITLOG_ARGS log --all --graph --oneline --decorate --color=always
    git $GITLOG_ARGS | nl | fzf $FZF_ARGS | perl -ne $COMMIT_ID_MATCHER | uniq
end

function vcs-diff
    # git-icdiff
    set argv (vcs-log)
    git diff --name-only $argv | fzf --ansi --reverse --multi --preview='git diff --color '$argv' {} | diff-so-fancy' --preview-window=right:80%
end

# ide config
set SRC_DIR src

function qsr-func-template
    echo '-- '$argv
    echo '\ args -> {'
    echo '    '
    echo '}'
end

# commands
function new-node
    if str-non-empty $argv[1]
        read -P 'node-name: ' -c $argv[1] filename
        test -f $filename
        or begin
            mkdir -p (dirname $filename)
            qsr-func-template $filename >$filename
        end
        editor $filename
    end
end

function del-node
    test -f $argv[1]
    and begin
        set f $argv[1]
        rm -i $f
        rm-empty-parents $f $SRC_DIR
    end
end

function fz
    set FZF_ARGS --ansi
    set FZF_ARGS $FZF_ARGS --reverse
    set FZF_ARGS $FZF_ARGS --multi
    set FZF_ARGS $FZF_ARGS --preview-window=right:50%
    set FZF_ARGS $FZF_ARGS --height=80%
    set FZF_ARGS $FZF_ARGS --min-height=10
    set FZF_ARGS $FZF_ARGS (list-tail $argv)

    tree -CDFtrf $argv[1] | drop 1 | eval "fzf $FZF_ARGS"
end

function repl
    set NAME_EXTRACT_CMD '$(echo {} | perl -ne \\\'/]  (.*)/; print "$1"\\\')'
    set CAT_FILE_CMD '(test -f '$NAME_EXTRACT_CMD' && cat '$NAME_EXTRACT_CMD')'
    set CAT_DIR_CMD '(test -d '$NAME_EXTRACT_CMD' && cat '$NAME_EXTRACT_CMD'/*)'

    set FZF_ARGS --prompt=\'$SRC_DIR :: \'
    set FZF_ARGS $FZF_ARGS --preview=\'$CAT_FILE_CMD' || '$CAT_DIR_CMD' | nl'\'
    set FZF_ARGS $FZF_ARGS --bind '\'enter:execute('(editor-cmd)' '$NAME_EXTRACT_CMD')\''
    set FZF_ARGS $FZF_ARGS --expect='\'ctrl-n,ctrl-d\''

    set fzf_output (fz $SRC_DIR $FZF_ARGS)
    and for file in $fzf_output[2..-1]
        set file (echo $file | perl -ne '/]  (.*)/; print "$1"')
        switch "$fzf_output[1]" # the key pressed
            case 'ctrl-n'
                new-node $file
            case 'ctrl-d'
                del-node $file
        end
    end
end

function fman
    set NAME_EXTRACT_CMD '$(echo {} | perl -ne \\\'/]  (.*)/; print "$1"\\\')'
    set CAT_FILE_CMD '(test -f '$NAME_EXTRACT_CMD' && cat '$NAME_EXTRACT_CMD')'
    set CAT_DIR_CMD '(test -d '$NAME_EXTRACT_CMD' && cat '$NAME_EXTRACT_CMD'/*)'

    set FZF_ARGS --prompt=\'$PWD :: \'
    set FZF_ARGS $FZF_ARGS --preview=\'$CAT_FILE_CMD' || '$CAT_DIR_CMD' | nl'\'
    set FZF_ARGS $FZF_ARGS --bind '\'enter:execute('(editor-cmd)' '$NAME_EXTRACT_CMD')\''
    set FZF_ARGS $FZF_ARGS --expect='\'ctrl-n,ctrl-d\''

    set fzf_output (fz . $FZF_ARGS)
    and for file in $fzf_output[2..-1]
        set file (echo $file | perl -ne '/]  (.*)/; print "$1"')
        switch "$fzf_output[1]" # the key pressed
            case 'ctrl-n'
                new-node $file
            case 'ctrl-d'
                del-node $file
        end
    end
end


# operations
function init
    set name (basename $PWD)
    if test (count (ls $PWD)) -gt 1
        read-confirm 'Directory not empty. Proceed?'
        or return 1
    end

    git init
    touch .gitignore

    mkdir doc src tst
    mkdir -p __ide/{data,code,tmp}

    qsr-func-template main >src/main
    echo '# '$name >README
    touch default.nix
end

function build
    echo build called: $argv
end

function debug
    echo build called: $argv
end

function run
    echo run called: $argv
end

# main
function main
    ign count $argv
    or return 0

    switch "$argv[1]"
        case init
            init
        case fman
            fman
        case new
            new-node $argv[2]
        case del
            del-node $argv[2]
        case build
            build $argv[2]
        case debug
            debug $argv[2]
        case run
            run $argv[2]
        case repl
            repl
        case quit
            exit
        case '*'
            echo ERROR No command found, argv: $argv
    end
end

main $argv
