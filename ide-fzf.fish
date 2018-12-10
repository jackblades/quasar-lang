#! /usr/bin/env fish

set IDE_ROOT_DIR '/Users/singhpdz/quasar-ide'

function tree-extract-name
    echo $argv | perl -ne '/] (.*)/; print "$1"'
end

function editor
    set MICRO_ARGS --softwrap true --autosave false --savecursor true --colorscheme atom-dark-tc --ignorecase true --tabstospaces true
    micro $MICRO_ARGS $argv
end

function preview
    set file $argv[1]

    test -f $file
    and bat --color=always -l nix $file

    test -d $file
    and find $file -type f | tail -n +1 | while read -l f
        bat --color=always -l nix $f
        echo
    end
end

function execute
    set file $argv[1]

    test -f $file
    and editor $file

    test -d $file
    and $IDE_ROOT_DIR/ide.fish __ide_execute $file
end

function tree-extract-name-cmd
    set file (tree-extract-name "$argv[1]")
    string length "$file" >/dev/null
    and eval "$argv[2..-1] $file"
end

eval "$argv"
