{-# LANGUAGE OverloadedStrings #-}

module Shell where

import Utils
import Data.Monoid ((<>))
import Turtle

pipe []       = empty
pipe (c:cmds) = c $ pipe cmds 

tree = inshell "tree -f -L 2"

-- fzf
fzf = inshell ("fzf" <> fzfOpts)
fzfm = inshell ("fzf" <> fzfmOpts)
fzfp cmd = inshell ("fzf" <> fzfOpts <> " --preview=\"" <> cmd <> "\"") 
fzfpm cmd = inshell ("fzf" <> fzfmOpts <> " --preview=\"" <> cmd <> "\"") 
--
fzfOpts = " --reverse" <> " --ansi"
fzfmOpts = fzfOpts <> " --multi"

--
lsByTime = inproc "/bin/ls" ["-tu"]