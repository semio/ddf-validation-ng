module Utils where

import Prelude

import Data.Array (concatMap, partition, filterA, concat)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff, launchAff_, message)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isDirectory, isFile)
import Node.FS.Sync (readTextFile, readdir, writeTextFile, stat)
import Node.Path as Path


getFiles :: String -> Effect (Array String)
getFiles x = do
  fs <- readdir x
  let fsFullPath = map (\f -> Path.concat [ x, f ]) fs
  files <- filterA (\f -> isFile <$> stat f) fsFullPath
  dirs <- filterA (\f -> isDirectory <$> stat f) fsFullPath
  fsInDirs <- concat <$> traverse (\d -> getFiles d) dirs
  pure $ files <> fsInDirs


arrayOfRight :: forall a b. Either a b -> Array b
arrayOfRight (Right b) = [b]
arrayOfRight _ = []

arrayOfLeft :: forall a b. Either a b -> Array a
arrayOfLeft (Left a) = [a]
arrayOfLeft _ = []