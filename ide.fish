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
        micro $filename
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
    # set FZF_ARGS $FZF_ARGS --no-extended --exact -i
    set FZF_ARGS $FZF_ARGS --no-sort
    set FZF_ARGS $FZF_ARGS (list-tail $argv)

    tree -CDtrf $argv[1] | drop 1 | eval "fzf $FZF_ARGS"
end

set TREE_EXTRACT_NAME_CMD 'tree-extract-name-cmd \"\\\'{}\\\'\"'
set IDE_ROOT_DIR '/Users/singhpdz/quasar-ide'
function ide-file-tree
    set FZF_ARGS $FZF_ARGS --preview="'$IDE_ROOT_DIR/ide-fzf.fish $TREE_EXTRACT_NAME_CMD preview'"
    set FZF_ARGS $FZF_ARGS --bind "'enter:execute($IDE_ROOT_DIR/ide-fzf.fish $TREE_EXTRACT_NAME_CMD execute)'"
    set FZF_ARGS $FZF_ARGS --expect='\'ctrl-n,ctrl-d\''
    set FZF_ARGS $FZF_ARGS $argv[2..-1]

    set IDE_FZF_DIR $argv[1]

    set fzf_output (fz $IDE_FZF_DIR $FZF_ARGS)
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

function repl
    switch (count $argv)
        case 0
            ide-file-tree $SRC_DIR --prompt="'$SRC_DIR :: '"
        case '*'
            ide-file-tree "$argv" --prompt="'$argv[1] :: '"
    end
end

function fman
    ide-file-tree . --prompt="'$PWD :: '"
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

    set remargs

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

        case __ide_execute
            echo $argv
            echo repl $argv[2..-1]
        case '*'
            echo ERROR No command found, argv: $argv
    end
end

main $argv
