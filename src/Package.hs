{-# LANGUAGE OverloadedStrings #-}

module Package where

import Turtle
import Shell
import Utils
import qualified Data.Text as T
import qualified Data.ByteString as BS

newpkg name = ifNotExists name $ mkcd name
delpkg name = rmtree name

foldir f d = "(test -f {} && "<>f<>") || (test -d {} && "<>d<>")"
foldirfn fn f d = "_foldirfn_name=\\$(" <> fn <> "); (test -f $_foldirfn_name && "<>f<>") || (test -d $_foldirfn_name && "<>d<>")"
explore pkg =
    fzfpm fpreview $ cat $ 
        [lsByTime "."] <> replicate 2 (pure " ") <> [hrule, tree empty]
    where fpreview = foldir "cat" "tree -f -L 2" <> " | nl "

selectFile pkg =
    fzfpm fpreview $ tree empty where
    -- fzfpm fpreview $ tree empty where
    fpreview = foldirfn "echo {} | grep -o ' .*'" "echo $_foldirfn_name" "tree -f -L 2 $_foldirfn_name" <> " | nl "



-- shell utilities
hrule :: Shell Line
hrule = pure $ conv $ replicate 60 '-'

mkcd dirname = do
    mktree dirname
    cd dirname

-- even more basic primitivies
ifNotExists path f = do
    exists <- testpath path
    if exists 
    then undefined -- log ERROR ("directory exists: " <> conv path)
    else f
