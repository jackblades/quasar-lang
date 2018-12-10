{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Redis where

import Database.Redis
import Control.Monad.IO.Class (liftIO)
import Data.ByteString as BS
import Data.Monoid ((<>))
import Utils

projectsPath = "quasar.projects"

existsProject name = hexists projectsPath name

deleteProject name = do
    hdel projectsPath [name]
    existsProject name

createProject :: RedisCtx m (Either t) => ByteString -> m ()
createProject name = do
    existsProject name >>= foldRedis 
        (\a -> void) 
        (\b -> if b 
            then void 
            else hset projectsPath name (conv (1 :: Int)) >> void) 
    where void = return ()

{- src tst ex doc build editor
-}
relative proj name = proj <> "." <> name

--
readExpr proj name = do
    expr <- get $ relative proj name
    parse expr where parse = id

writeExpr proj name expr = do
    set (relative proj name) (serialize expr) where
        serialize = id


run :: Redis b -> IO b
run x = do
    conn <- checkedConnect defaultConnectInfo
    runRedis conn x

foldRedis :: (a -> c) -> (b -> c) -> Either a b -> c
foldRedis lf rf v =
    case v of
        Left a -> lf a
        Right b -> rf b 

-- hexists = run .: R.hexists
-- hdel = run .: R.hdel
-- hset = run .:. R.hset
-- get = run . R.get
-- set = run .: R.set