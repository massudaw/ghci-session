-- | __Images, for a model that sees them.__ An image is kept once, in the session's state, under a name made of
-- its bytes; what stands for it in a text -- a message typed, a tool's answer, a line of the history -- is a
-- line of its own, @[image NAME]@. So the history holds only names (a summary can mention one, and costs
-- nothing), and wherever a text goes to a model that takes images, the lines that name one are followed by the
-- image itself ('attach' for the Messages API, 'toolBlocks' for a tool server's answer). An agent that opens a
-- message again (@zoom(id, 1)@) is answered with its text, the line in it, and so sees the image again.
--
-- An image larger than a model makes use of (its long side over 'fitSide', or over 'fitBytes') is kept with a
-- smaller copy beside it (@NAME.jpg@), which is what is sent -- made by @sips@ or ImageMagick, whichever the
-- machine has; with neither the image goes as it is, unless it is over what the API takes at all.
module GhciSession.Image
  ( kindOf, sizeOf, keep, marker, namesIn, isMarker
  , load, attach, apiBlock, toolBlocks, typed
  , base64, perMessage
  ) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (filterM, when)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Data.Char (isHexDigit, isSpace, toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getHomeDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

import GhciSession.Json

-- | The long side a model makes use of, and the bytes over which a smaller copy is made anyway.
fitSide, fitBytes :: Int
fitSide = 1568
fitBytes = 2 ^ (20 :: Int)

-- | What the API takes at all: an image's bytes (five megabytes once encoded) and its sides.
maxBytes, maxSide :: Int
maxBytes = 3700000
maxSide = 8000

-- | How many images go with one message: its last ones.
perMessage :: Int
perMessage = 20

-- | What kind of image these bytes are, by how they begin: its media type.
kindOf :: B.ByteString -> Maybe String
kindOf b
  | B.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] `B.isPrefixOf` b = Just "image/png"
  | B.pack [0xFF, 0xD8, 0xFF] `B.isPrefixOf` b = Just "image/jpeg"
  | BC.pack "GIF87a" `B.isPrefixOf` b || BC.pack "GIF89a" `B.isPrefixOf` b = Just "image/gif"
  | BC.pack "RIFF" `B.isPrefixOf` b && B.take 4 (B.drop 8 b) == BC.pack "WEBP" = Just "image/webp"
  | otherwise = Nothing

-- | An image's width and height, where its head says them.
sizeOf :: B.ByteString -> Maybe (Int, Int)
sizeOf b = case kindOf b of
  Just "image/png" | B.length b >= 24 -> Just (be 16 4, be 20 4)
  Just "image/gif" | B.length b >= 10 -> Just (le 6 2, le 8 2)
  Just "image/jpeg" -> jpeg 2
  Just "image/webp" | B.length b >= 30 -> case BC.unpack (B.take 4 (B.drop 12 b)) of
    "VP8X" -> Just (le 24 3 + 1, le 27 3 + 1)
    "VP8 " -> Just (le 26 2 .&. 0x3FFF, le 28 2 .&. 0x3FFF)
    "VP8L" -> let v = le 21 4 in Just ((v .&. 0x3FFF) + 1, ((v `shiftR` 14) .&. 0x3FFF) + 1)
    _ -> Nothing
  _ -> Nothing
  where
    at i = fromIntegral (B.index b i) :: Int
    be o n = foldl (\a k -> a `shiftL` 8 .|. at (o + k)) 0 [0 .. n - 1]
    le o n = foldl (\a k -> a `shiftL` 8 .|. at (o + k)) 0 [n - 1, n - 2 .. 0]
    -- a JPEG's segments, to the one that starts a frame
    jpeg o
      | o + 9 >= B.length b || at o /= 0xFF = Nothing
      | m == 0xFF = jpeg (o + 1)
      | m >= 0xC0 && m <= 0xCF && m `notElem` [0xC4, 0xC8, 0xCC] = Just (be (o + 7) 2, be (o + 5) 2)
      | m == 0xD8 || (m >= 0xD0 && m <= 0xD7) || m == 0x01 = jpeg (o + 2)
      | otherwise = jpeg (o + 2 + be (o + 2) 2)
      where m = at (o + 1)

extOf :: String -> String
extOf kind = case kind of { "image/jpeg" -> "jpg"; k -> drop 6 k }

-- | A name for these bytes: sixteen hexadecimal digits of them, and what kind they are.
nameOf :: String -> B.ByteString -> String
nameOf kind b = printf "%016x" (B.foldl' (\h w -> (h `xor` fromIntegral w) * 1099511628211) (14695981039346656037 :: Word64) b) ++ "." ++ extOf kind

-- | Is this a name 'keep' gives? (Only such a name is ever looked for in the directory.)
isName :: String -> Bool
isName n = let (h, e) = splitAt 16 n in length h == 16 && all (\c -> isHexDigit c && c == toLower c) h && e `elem` [".png", ".jpg", ".gif", ".webp"]

-- | Keep an image in the directory: its name and what it is in words (@image\/png, 800x600, 120 KB@), or why
-- it cannot be shown. A smaller copy is made beside it when it is larger than is of use.
keep :: FilePath -> B.ByteString -> IO (Either String (String, String))
keep dir bytes = case kindOf bytes of
  Nothing -> pure (Left "not an image this reads (png, jpeg, gif, webp)")
  Just kind -> do
    let name = nameOf kind bytes
        size = sizeOf bytes
        side = maybe 0 (uncurry max) size
        over = B.length bytes > maxBytes || side > maxSide
        large = side > fitSide || B.length bytes > fitBytes
        file = dir </> name
        said = kind ++ maybe "" (\(w, h) -> printf ", %dx%d" w h) size ++ printf ", %d KB" ((B.length bytes + 1023) `div` 1024)
    createDirectoryIfMissing True dir
    have <- doesFileExist file
    when (not have) (B.writeFile file bytes)
    -- (an animation is left as it is: a copy would be its first picture only)
    fit <- if large && kind /= "image/gif" then shrink file (file ++ ".jpg") else pure False
    pure (if over && not fit
            then Left (said ++ ": too large to show (over " ++ show (maxBytes `div` 1000000) ++ " MB or " ++ show maxSide ++ " pixels a side), and neither sips nor ImageMagick is here to make it smaller")
            else Right (name, said ++ (if fit then printf ", shown at %d pixels" fitSide else "")))

-- | A copy of an image no larger than is of use, as a JPEG. False when no program here makes one.
shrink :: FilePath -> FilePath -> IO Bool
shrink from to = do
  have <- doesFileExist to
  if have then pure True else do
    sips <- findExecutable "sips"
    magick <- findExecutable "magick"
    convert <- findExecutable "convert"
    let side = show fitSide
        box = side ++ "x" ++ side ++ ">"
        tries = [ (p, ["-Z", side, "-s", "format", "jpeg", "-s", "formatOptions", "85", from, "--out", to]) | Just p <- [sips] ]
             ++ [ (p, [from ++ "[0]", "-resize", box, "-quality", "85", "jpeg:" ++ to]) | Just p <- [magick, convert] ]
        go [] = pure False
        go ((p, args) : rest) = do
          r <- try (readProcessWithExitCode p args "") :: IO (Either IOException (ExitCode, String, String))
          ok <- doesFileExist to
          made <- if ok then (\b -> kindOf b == Just "image/jpeg" && B.length b <= maxBytes) <$> B.readFile to else pure False
          case r of
            Right (ExitSuccess, _, _) | made -> pure True
            _ -> when ok (removeFile to) >> go rest
    go tries

-- | The line that stands for an image in a text.
marker :: String -> T.Text
marker name = T.pack ("[image " ++ name ++ "]")

-- | Is this line one that stands for an image? Its name.
isMarker :: T.Text -> Maybe String
isMarker l = do
  r <- T.stripPrefix (T.pack "[image ") (T.strip l)
  n <- T.unpack <$> T.stripSuffix (T.pack "]") r
  if isName n then Just n else Nothing

-- | The images a text names, each on a line of its own: its last 'perMessage', each once.
namesIn :: T.Text -> [String]
namesIn t
  | not (T.pack "[image " `T.isInfixOf` t) = []
  | otherwise = let ns = nub (catMaybes (map isMarker (T.lines t))) in drop (length ns - perMessage) ns

-- (what was read and encoded, by file: a conversation's images go with every call of a turn)
{-# NOINLINE loaded #-}
loaded :: IORef (M.Map FilePath (String, T.Text))
loaded = unsafePerformIO (newIORef M.empty)

-- | An image of the directory as it is sent: its media type and its bytes in base64 (the smaller copy, where
-- there is one). Nothing when no such image is kept.
load :: FilePath -> String -> IO (Maybe (String, T.Text))
load dir name
  | not (isName name) = pure Nothing
  | otherwise = do
      let file = dir </> name
      seen <- M.lookup file <$> readIORef loaded
      case seen of
        Just v -> pure (Just v)
        Nothing -> do
          fit <- doesFileExist (file ++ ".jpg")
          r <- try (B.readFile (if fit then file ++ ".jpg" else file)) :: IO (Either IOException B.ByteString)
          case r of
            Right b | Just kind <- kindOf b -> do
              let v = (kind, TE.decodeLatin1 (base64 b))
              atomicModifyIORef' loaded (\m -> (M.insert file v (if M.size m >= 64 then M.empty else m), ()))
              pure (Just v)
            _ -> pure Nothing

-- | An image as a block of the Messages API.
apiBlock :: (String, T.Text) -> Json
apiBlock (kind, dat) = JObj [("type", JStr "image"), ("source", JObj [("type", JStr "base64"), ("media_type", JStr kind), ("data", JText dat)])]

-- | The images a text names, as blocks of the Messages API.
blocksOf :: FilePath -> T.Text -> IO [Json]
blocksOf dir t = map apiBlock . catMaybes <$> mapM (load dir) (namesIn t)

-- | A conversation (in the shape the chat keeps it) with the images its messages name: a @user@ or a @tool@
-- message that names some gets them as @images@, which the Messages API's request puts after its text.
attach :: FilePath -> [Json] -> IO [Json]
attach dir = mapM one
  where
    one m | lookupStr "role" m `elem` [Just "user", Just "tool"], Just t <- lookupText "content" m, not (null (namesIn t)) = do
              bs <- blocksOf dir t
              pure (if null bs then m else set "images" (JArr bs) m)
          | otherwise = pure m

-- | The images a tool's answer names, as a tool server answers with them.
toolBlocks :: FilePath -> T.Text -> IO [Json]
toolBlocks dir t = map (\(kind, dat) -> JObj [("type", JStr "image"), ("data", JText dat), ("mimeType", JStr kind)]) . catMaybes <$> mapM (load dir) (namesIn t)

-- | A line typed, with the images it names kept and their lines after it. A word of it names one when it is
-- the path of a file that is an image (as a file dragged onto a terminal is written: a space in it escaped, or
-- the whole in quotes; @~@ for the home directory), in the project or anywhere.
typed :: FilePath -> FilePath -> T.Text -> IO T.Text
typed dir project line
  | not (any (`T.isInfixOf` T.toLower line) (map T.pack exts)) = pure line
  | otherwise = do
      home <- either (\(_ :: SomeException) -> "") id <$> try getHomeDirectory
      let place p = case p of
            ('~' : '/' : r) -> home </> r
            ('/' : _) -> p
            _ -> project </> p
          cands = nub [ place w | w <- pathWords (T.unpack line), map toLower (takeExtension w) `elem` exts ]
      files <- filterM doesFileExist cands
      names <- fmap catMaybes . mapM (\f -> do
        r <- try (B.readFile f) :: IO (Either IOException B.ByteString)
        case r of
          Right b -> either (const Nothing) (Just . fst) <$> keep dir b
          Left _ -> pure Nothing) $ files
      pure (if null names then line else T.intercalate (T.pack "\n") (T.stripEnd line : map marker (nub names)))
  where exts = [".png", ".jpg", ".jpeg", ".gif", ".webp"]

-- | A line's words as a shell would take them, roughly: a backslash keeps the next character, quotes keep spaces.
pathWords :: String -> [String]
pathWords = go ""
  where
    go acc [] = [ reverse acc | not (null acc) ]
    go acc ('\\' : c : r) = go (c : acc) r
    go acc (q : r) | q `elem` "'\"", (inside, _ : after) <- break (== q) r = go (reverse inside ++ acc) after
    go acc (c : r) | isSpace c = [ reverse acc | not (null acc) ] ++ go "" r
                   | otherwise = go (c : acc) r

-- | Bytes in base64.
base64 :: B.ByteString -> B.ByteString
base64 b = fst (B.unfoldrN (4 * ((n + 2) `div` 3)) (\k -> Just (out k, k + 1)) 0)
  where
    n = B.length b
    at i = if i < n then fromIntegral (BU.unsafeIndex b i) else 0 :: Int
    out k =
      let (g, p) = k `divMod` 4
          i = 3 * g
          v = at i `shiftL` 16 .|. at (i + 1) `shiftL` 8 .|. at (i + 2)
          pad = (p == 2 && i + 1 >= n) || (p == 3 && i + 2 >= n)
      in if pad then 61 else BU.unsafeIndex alphabet ((v `shiftR` (18 - 6 * p)) .&. 63) :: Word8
    alphabet = BC.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
