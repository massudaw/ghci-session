{-# LANGUAGE CPP #-}
-- | Custom Setup: make a C header change -- or a cc-options / cpp-options change, which is what the flags are
-- (@-DHAVE_FFF@ for @fff@, @-DGVT_STATIC@ for ghostty-vt's @static@) -- rebuild the package's C sources.
-- One file for the packages that have C with headers or flags (this one, and @ghostty-vt/@ by a link).
--
-- Cabal compiles @c-sources@ but does no header dependency scanning, and does not take a changed
-- @cc-options@ for a reason to recompile a @.c@ whose contents did not change. So after a header moves, or a
-- flag flips, the build says "Up to date" (or links) with the OLD objects: the engine without the change in
-- @hygiene/c/rts_syms.h@, undefined @_ghostty_*@ symbols in a build that was static, a library looked for at run
-- time that is linked in.
--
-- Both halves are needed: @extra-source-files@ in the .cabal (what makes cabal-install notice a header at all)
-- and this hook, which hashes the headers and the C options, compares the hash with a stamp under the build
-- directory, and when it moved deletes the object files of the package's own C sources (every suffix cabal
-- emits for one: @.dyn_o@ does not end in @.o@). Cabal then recompiles them and everything downstream relinks.
-- It only ever deletes build artefacts, so the worst case is a redundant recompile.
module Main (main) where

import           Control.Monad                   (forM_, unless, when)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as BS8
import           Data.Bits                       (xor)
import           Data.List                       (isSuffixOf, nub, sort)
import           Data.Word                       (Word64)
import           Distribution.PackageDescription (PackageDescription, allBuildInfo, cSources, ccOptions, cppOptions,
                                                  includeDirs)
import           Distribution.Simple             (defaultMainWithHooks, simpleUserHooks)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import           Distribution.Simple.UserHooks   (UserHooks (..))
#if MIN_VERSION_Cabal(3,14,0)
import           Distribution.Utils.Path         (getSymbolicPath)
#endif
import           System.Directory                (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                                  listDirectory, removeFile)
import           System.FilePath                 (takeDirectory, takeFileName, (</>))
import           System.IO                       (hPutStrLn, stderr)

#if !MIN_VERSION_Cabal(3,14,0)
-- (Cabal 3.14 made these paths symbolic; before it they are plain)
getSymbolicPath :: FilePath -> FilePath
getSymbolicPath = id
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pd lbi hooks flags -> do
      invalidateStaleCObjects pd lbi
      buildHook simpleUserHooks pd lbi hooks flags
  }

-- | Every directory that could hold a header the C includes: the declared @include-dirs@ and the directory of
-- each C source.
headerDirs :: PackageDescription -> [FilePath]
headerDirs pd = nub $ map getSymbolicPath (concatMap includeDirs bis) ++ map (takeDirectory . getSymbolicPath) (concatMap cSources bis)
  where bis = allBuildInfo pd

-- | All headers under those directories, nested ones too (libghostty-vt's are in @ghostty/vt/@), sorted so the
-- hash is order-stable.
findHeaders :: PackageDescription -> IO [FilePath]
findHeaders pd = fmap (sort . nub . concat) . mapM listHeaders $ headerDirs pd
  where
    listHeaders d = do
      ok <- doesDirectoryExist d
      if not ok then pure [] else do
        es <- listDirectory d
        fmap concat . mapM (\e -> do
          let p = d </> e
          isDir <- doesDirectoryExist p
          if isDir then listHeaders p else pure [ p | ".h" `isSuffixOf` e ]) $ es

-- | The options that reach the C compiler; the @static@ flag adds @-DGVT_STATIC@.
optionSalt :: PackageDescription -> String
optionSalt pd = show (sort (nub (concatMap ccOptions bis ++ concatMap cppOptions bis)))
  where bis = allBuildInfo pd

-- | FNV-1a over the salt and the concatenated contents: change detection only, and @setup-depends@ stays
-- boot libraries.
hashFiles :: String -> [FilePath] -> IO Word64
hashFiles salt fps = do
  bs <- BS.concat <$> mapM BS.readFile fps
  pure (BS.foldl' step (BS.foldl' step 14695981039346656037 (BS8.pack salt)) bs)
  where step h w = (h `xor` fromIntegral w) * 1099511628211

-- | The suffixes cabal emits for one C source (vanilla, dynamic, profiling), all invalidated together.
objSuffixes :: [String]
objSuffixes = [".o", ".dyn_o", ".p_o"]

-- | The object files of this package's own C sources, wherever they landed under the build directory.
staleObjects :: PackageDescription -> LocalBuildInfo -> IO [FilePath]
staleObjects pd lbi = do
  let names = nub [ takeFileName (getSymbolicPath c) | c <- concatMap cSources (allBuildInfo pd) ]
      wanted = concat [ map (take (length n - 2) n ++) objSuffixes | n <- names ]
  os <- walk (getSymbolicPath (buildDir lbi))
  pure [ o | o <- os, takeFileName o `elem` wanted ]
  where
    walk d = do
      ok <- doesDirectoryExist d
      if not ok then pure [] else do
        es <- listDirectory d
        fmap concat . mapM (\e -> do
          let p = d </> e
          isDir <- doesDirectoryExist p
          if isDir then walk p else pure [ p | any (`isSuffixOf` p) objSuffixes ]) $ es

invalidateStaleCObjects :: PackageDescription -> LocalBuildInfo -> IO ()
invalidateStaleCObjects pd lbi = do
  hs <- findHeaders pd
  let salt = optionSalt pd
  unless (null hs && null salt) $ do
    h <- hashFiles salt hs
    let stampDir = getSymbolicPath (buildDir lbi)
        stamp    = stampDir </> "c-headers.stamp"
    createDirectoryIfMissing True stampDir
    -- (a strict read: a lazy one leaves the handle open and the write below fails with "resource busy")
    old <- do
      ok <- doesFileExist stamp
      if ok then Just <$> BS.readFile stamp else pure Nothing
    let new = BS8.pack (show h)
    when (old /= Just new) $ do
      objs <- staleObjects pd lbi
      forM_ objs $ \o -> do
        ok <- doesFileExist o
        when ok $ removeFile o
      unless (null objs) $
        hPutStrLn stderr ("Setup: C headers or cc-options changed -- recompiling " ++ show (length objs) ++ " C object(s)")
      BS.writeFile stamp new
