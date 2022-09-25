module Utils where

import Prelude

import Data.Array (concatMap, partition, filterA, concat)
import Data.Either (Either(..))
import Data.List (List(..), singleton)
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


arrayOfRight :: forall a b. Either a b -> List b
arrayOfRight (Right b) = singleton b
arrayOfRight _ = Nil

arrayOfLeft :: forall a b. Either a b -> List a
arrayOfLeft (Left a) = singleton a
arrayOfLeft _ = Nil

-- | create a counter
-- counter :: forall a. Ord a => Eq a => NonEmptyArray a -> NonEmptyArray (Tuple a Int)
-- counter xs = map (\x -> (Tuple (NArr.head x) (NArr.length x))) <<< NArr.group <<< NArr.sort $ xs

-- -- | check if there are duplicated entries in a list
-- checkDups :: forall a. Show a => Eq a => Ord a => NonEmptyArray a -> V Errors (NonEmptyArray a)
-- checkDups xs =
--   let
--     ns = counter xs

--     hasDups = NArr.filter (\x -> (snd x) > 1) ns
--   in
--     case Arr.head hasDups of
--       Nothing -> pure xs
--       Just _ -> do
--         let
--           allDups = map fst hasDups

--           msg = "duplicated entry: " <> show allDups
--         invalid [ Error msg ]