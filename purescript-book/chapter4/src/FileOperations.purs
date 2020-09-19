module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, foldl, head, (:))
import Data.Maybe (Maybe(..))
import Data.Path (Path, filename, isDirectory, ls, root, size)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Exercise 4.17.1
onlyFiles :: Path -> Array Path
onlyFiles = allFiles
  >>> filter (\file -> not $ isDirectory file)

-- Exercise 4.17.2
largestFile :: Path -> Maybe Path
largestFile = foldl compare Nothing <<< onlyFiles
  where
    compare Nothing file = Just file
    compare (Just max) file =
      if size file > size max
        then Just file
        else Just max

smallestFile :: Path -> Maybe Path
smallestFile = foldl compare Nothing <<< onlyFiles
  where
    compare Nothing file = Just file
    compare (Just min) file =
      if size file < size min
        then Just file
        else Just min

-- Exercise 4.17.3
whereIs :: String -> Maybe Path
whereIs file = head $ do
  directory <- allFiles root
  guard $ isDirectory directory
  child <- ls directory
  guard $ filename child == file
  pure $ directory
