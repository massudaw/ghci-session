-- | __What an import need not read again__: a session file whose size and modification time are as they were
-- when it was last decided on (taken, or passed over as nobody's) holds nothing new -- Claude Code and Codex
-- only append to it. Those two numbers of each file are kept beside @history\/imported.json@
-- (@imported-files.json@), and the import that runs at the end of every turn then reads only the file of
-- the turn, not the thirty before it.
module GhciSession.ImportStamp
  ( Stamp, fileStamp, loadStamps, saveStamps, stampsIn
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (getFileSize, getModificationTime, renameFile)

import GhciSession.Json

-- | A file as it was: its size in bytes, and its modification time in whole microseconds (a whole number,
-- so that it is written and read back as it was -- a fraction of seconds would not be).
type Stamp = (Integer, Integer)

-- | The stamp of a file now; 'Nothing' when it cannot be asked.
fileStamp :: FilePath -> IO (Maybe Stamp)
fileStamp f = do
  r <- try ((,) <$> getFileSize f <*> getModificationTime f)
  pure (case r of
    Left (_ :: IOException) -> Nothing
    Right (n, t) -> Just (n, floor (utcTimeToPOSIXSeconds t * 1000000)))

-- | The stamps kept; none when the file is not there or is not read (the import is then of everything).
loadStamps :: FilePath -> IO (M.Map FilePath Stamp)
loadStamps file = do
  r <- try (B.readFile file)
  pure (case r of
    Left (_ :: IOException) -> M.empty
    Right b -> either (const M.empty) (\j -> M.fromList [ (k, (round n, round m)) | (k, JArr [JNum n, JNum m]) <- fromMaybe [] (obj j) ]) (parseJsonBS b))

-- | The stamps written, whole or not at all (a file written aside, then moved).
saveStamps :: FilePath -> M.Map FilePath Stamp -> IO ()
saveStamps file m = do
  B.writeFile (file ++ ".new") (encodeBS (JObj [ (k, JArr [JNum (fromInteger n), JNum (fromInteger t)]) | (k, (n, t)) <- M.toList m ]))
  renameFile (file ++ ".new") file

-- | Of the files, those whose stamp now is the one kept (so not to be read), and the stamps now of the others.
stampsIn :: M.Map FilePath Stamp -> [(FilePath, Maybe Stamp)] -> ([FilePath], [(FilePath, Stamp)])
stampsIn kept fs =
  ( [ f | (f, Just s) <- fs, M.lookup f kept == Just s ]
  , [ (f, s) | (f, Just s) <- fs, M.lookup f kept /= Just s ] )
