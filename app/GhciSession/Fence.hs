-- | The write boundary of a target (@"write_paths"@): where the agent's write, edit and edits may put a file.
-- A path is judged by where it REALLY is -- a symlink that leads out of the allowed places is out of them, as is
-- one that does not exist yet whose directory does.
module GhciSession.Fence (within, realPath, writeAllowed) where

import Control.Exception (IOException, try)
import Data.List (intercalate, isPrefixOf)
import System.Directory (canonicalizePath, doesPathExist, getSymbolicLinkTarget, pathIsSymbolicLink)
import System.FilePath (dropTrailingPathSeparator, makeRelative, normalise, splitDirectories, takeDirectory, takeFileName, (</>))

-- | Is a path one of the allowed, or inside one? (Whole directories: @src@ does not hold @src2@.)
within :: [FilePath] -> FilePath -> Bool
within allowed p = any (\a -> splitDirectories a `isPrefixOf` splitDirectories p) allowed

-- | Where a path really is: its symlinks followed as far as it exists (a link to nothing too), the rest as it is written.
realPath :: FilePath -> IO FilePath
realPath = go (20 :: Int) . dropTrailingPathSeparator . normalise
  where
    go 0 p = pure p
    go n p = do
      there <- doesPathExist p
      if there then canonicalizePath p else do
        link <- try (pathIsSymbolicLink p) :: IO (Either IOException Bool)
        case link of
          Right True -> do
            t <- getSymbolicLinkTarget p
            go (n - 1) (dropTrailingPathSeparator (normalise (takeDirectory p </> t)))
          _ | takeDirectory p == p -> pure p
            | otherwise -> do
                d <- go n (takeDirectory p)
                pure (d </> takeFileName p)

-- | May the agent write this file (a full path inside the project root)? Else why not, saying where writing is allowed.
writeAllowed :: FilePath -> Maybe [FilePath] -> FilePath -> IO (Either String ())
writeAllowed _ Nothing _ = pure (Right ())
writeAllowed root (Just allowed) full = do
  real <- realPath full
  places <- mapM (realPath . (root </>)) allowed
  pure (if within places real then Right ()
        else Left (makeRelative root full ++ ": outside write_paths -- this project allows writing only in "
                   ++ (if null allowed then "nothing (write_paths is empty)" else intercalate ", " allowed)
                   ++ " (relative to the project; \"write_paths\" of its target in ghci-session.json)"))
