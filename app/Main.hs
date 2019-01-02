{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude
import Text.Pretty.Simple (pPrint)
import System.Environment (getArgs)
import Control.Lens
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Tree as T

main :: IO ()
main = undefined



--
data UUID = UUID deriving (Ord, Eq, Show)
data Name = Name deriving (Ord, Eq, Show)
data Program a = Program
  { _uuid :: M.Map Name UUID
  , _name :: M.Map UUID Name
  , _calls :: M.Map UUID [UUID]
  , _calledBy :: M.Map UUID [UUID]
  , _expr :: M.Map UUID a
  } deriving (Show, Functor)

makeLenses ''Program

atomically = id
rename prog a b = case prog ^?! uuid . at a of
    Nothing    -> Left "KeyError"
    Just uuidA -> atomically $ return $
        prog & name . at uuidA .~ Just b
             & uuid . at a     .~ Just uuidA
