-- | The checks of the round that gave a project tools of its own (declared tools, the write boundary, the built-in
-- tools a target offers, instructions by target): a list of (what is checked, whether it holds), run by
-- "GhciSession.SelfTest" with the rest.
module GhciSession.SelfTools (checks, checksIO) where

import Control.Exception (IOException, try)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isInfixOf, nub, sort)
import Data.Either (isLeft)
import System.Directory (createDirectoryIfMissing, createDirectoryLink, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)

import GhciSession.Chat (chatTools)
import GhciSession.Config
import GhciSession.Declared
import GhciSession.Fence (writeAllowed)
import GhciSession.Json
import GhciSession.Mcp (Tool (..), tools)

-- | A declaration, parsed (it is a test's own: it parses).
decl :: String -> Declared
decl s = case parseJson s >>= \j -> either Left Right (parseDeclared (JArr [j])) of
  Right [d] -> d
  other -> error ("SelfTools.decl: " ++ show other)

look :: Declared
look = decl "{\"name\":\"look\",\"description\":\"d\",\"params\":{\"view\":{\"type\":\"string\"},\"box\":{\"type\":\"integer\"},\"scale\":{\"type\":\"number\"},\"flag\":{\"type\":\"boolean\"}},\"required\":[\"view\"],\"expr\":\"Demo.look {view} {box} {scale} {flag}\"}"

with :: [(String, Json)] -> Either String String
with kvs = substitute look (JObj kvs)

refused :: String -> Either String String -> Bool
refused why = either (why `isInfixOf`) (const False)

bad :: String -> String -> Bool
bad why s = either (why `isInfixOf`) (const False) (parseJson s >>= parseDeclared)

checks :: [(String, Bool)]
checks =
  [ ("tools: a string is substituted as show writes it: quotes, backslashes, newlines and } stay inside it, and it reads back whole"
    , let s = "a\"b\\c\nd}e{f}\" ; Prelude.undefined \"\233"
      in case with [("view", JStr s)] of
           Right e -> take 11 e == "Demo.look \"" && (case reads (takeWhile (/= '\n') (drop 10 e)) :: [(String, String)] of { [(back, rest)] -> back == s && rest == " Nothing Nothing Nothing"; _ -> False }) && '\n' `notElem` e
           Left _ -> False)
  , ("tools: an optional parameter not given is Nothing, one given is (Just literal); a required one is the literal"
    , with [("view", JStr "v")] == Right "Demo.look \"v\" Nothing Nothing Nothing"
      && with [("view", JStr "v"), ("box", JNum 3), ("scale", JNum 0.5), ("flag", JBool False)] == Right "Demo.look \"v\" (Just 3) (Just 0.5) (Just False)"
      && with [("view", JStr "v"), ("box", JNull)] == Right "Demo.look \"v\" Nothing Nothing Nothing")
  , ("tools: a negative number is in parentheses, an integral number is written as an integer, an integer must be one"
    , with [("view", JStr "v"), ("scale", JNum (-2.5)), ("box", JNum (-3))] == Right "Demo.look \"v\" (Just (-3)) (Just (-2.5)) Nothing"
      && with [("view", JStr "v"), ("scale", JNum 4)] == Right "Demo.look \"v\" Nothing (Just 4) Nothing"
      && refused "box is an integer" (with [("view", JStr "v"), ("box", JNum 2.5)]))
  , ("tools: a boolean is True or False"
    , with [("view", JStr "v"), ("flag", JBool True)] == Right "Demo.look \"v\" Nothing Nothing (Just True)")
  , ("tools: a missing required parameter is refused, and so is an unknown one, each saying what the tool takes"
    , refused "missing required parameter(s) view; it takes view, box, scale, flag" (with [("box", JNum 1)])
      && refused "unknown parameter(s) zzz; it takes view" (with [("view", JStr "v"), ("zzz", JStr "x")]))
  , ("tools: a value of another type is refused (a string for a number, a number for a string), not turned into one"
    , refused "scale is a number" (with [("view", JStr "v"), ("scale", JStr "1")]) && refused "view is a string" (with [("view", JNum 1)])
      && refused "flag is a boolean" (with [("view", JStr "v"), ("flag", JStr "True")]))
  , ("tools: a brace that is not a name in braces is part of the expression; a name is a placeholder wherever it is"
    , placeholders "Foo { x = 1 } {a} {b}{a} {}" == ["a", "b"] && substitute (decl "{\"name\":\"r\",\"params\":{\"a\":{\"type\":\"boolean\"}},\"required\":[\"a\"],\"expr\":\"f (R { x = 1 }) {a}\"}") (JObj [("a", JBool True)]) == Right "f (R { x = 1 }) True")
  , ("tools: a declaration is refused for a built-in tool's name, a {param} that is not declared, a type that is not one of the four"
    , bad "built-in" "[{\"name\":\"sh\",\"expr\":\"x\"}]" && bad "{b} in \"expr\" is not a declared parameter" "[{\"name\":\"t\",\"params\":{\"a\":{\"type\":\"string\"}},\"expr\":\"f {a} {b}\"}]"
      && bad "type \"float\"" "[{\"name\":\"t\",\"params\":{\"a\":{\"type\":\"float\"}},\"expr\":\"f {a}\"}]")
  , ("tools: also refused: two with a name, a required one not declared, no expr, a name that is not one, a timeout that is not seconds"
    , bad "declared twice" "[{\"name\":\"t\",\"expr\":\"x\"},{\"name\":\"t\",\"expr\":\"y\"}]" && bad "required parameter \"q\"" "[{\"name\":\"t\",\"required\":[\"q\"],\"expr\":\"x\"}]"
      && bad "no \"expr\"" "[{\"name\":\"t\"}]" && bad "a name is letters" "[{\"name\":\"a b\",\"expr\":\"x\"}]" && bad "\"timeout\"" "[{\"name\":\"t\",\"expr\":\"x\",\"timeout\":0}]"
      && (parseJson "[{\"name\":\"t\",\"expr\":\"x\",\"timeout\":5}]" >>= parseDeclared) == Right [Declared "t" "" [] [] "x" (Just 5)])
  , ("tools: the names a declared tool may not take are the chat's and the server's tools, no fewer, no more"
    , sort builtinNames == sort (nub (map tName (chatTools ++ tools))))
  , ("fence: write_paths are relative to the project and inside it; builtin_tools name tools there are"
    , isLeft (validWritePaths (JArr [JStr "../x"])) && isLeft (validWritePaths (JArr [JStr "/etc"])) && isLeft (validWritePaths (JStr "src"))
      && validWritePaths (JArr [JStr "src", JStr "notes.md"]) == Right ["src", "notes.md"]
      && isLeft (validBuiltin (JArr [JStr "frobnicate"])) && validBuiltin (JArr [JStr "read", JStr "grep"]) == Right ["read", "grep"])
  ]

-- | What is on disk: the boundary against real paths and symlinks, and the configuration as it is loaded.
checksIO :: IO [(String, Bool)]
checksIO = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  let root = tmp </> ("ghs-selftools-" ++ show pid)
      outside = tmp </> ("ghs-selftools-out-" ++ show pid)
  mapM_ (createDirectoryIfMissing True) [root </> "src", root </> "doc", root </> "src2", outside]
  _ <- try (createDirectoryLink outside (root </> "src" </> "leak")) :: IO (Either IOException ())
  _ <- try (createDirectoryLink (root </> "doc") (root </> "src" </> "tolink")) :: IO (Either IOException ())
  let ok allowed p = either (const False) (const True) <$> writeAllowed root allowed (root </> p)
      why allowed p = either id (const "") <$> writeAllowed root allowed (root </> p)
  a <- mapM (ok (Just ["src", "notes.md"])) ["src/a.hs", "src/deep/new/b.hs", "notes.md"]
  b <- mapM (ok (Just ["src", "notes.md"])) ["doc/x.md", "src2/y.hs", "other.md", "notes.md.bak"]
  leak <- mapM (ok (Just ["src"])) ["src/leak/pwned.txt", "src/leak"]
  linkIn <- ok (Just ["src"]) "src/tolink/z.md"      -- (a link to a place the project allows nothing of)
  free <- ok Nothing "anything/at/all"
  none <- ok (Just []) "src/a.hs"
  msg <- why (Just ["src", "notes.md"]) "doc/x.md"
  -- the configuration: what is refused at load, and the first member's keys
  n <- newIORef (0 :: Int)
  -- (a directory for each: the file is read lazily, and one still open is locked against a write)
  let conf s = do
        k <- atomicModifyIORef' n (\x -> (x + 1, x))
        let dir = root </> ("conf" ++ show k)
        createDirectoryIfMissing True dir
        writeFile (dir </> "ghci-session.json") s
        loadConf dir
      target extra = "{\"targets\":{\"a\":{\"units\":[\"lib:a\"]" ++ extra ++ "},\"b\":{\"units\":[\"lib:b\"],\"write_paths\":[\"b\"],\"instructions\":\"b.md\"}},\"sessions\":{\"ab\":[\"a\",\"b\"],\"ba\":[\"b\",\"a\"]}}"
      refusedBy bit extra = either (bit `isInfixOf`) (const False) <$> conf (target extra)
  r1 <- refusedBy "built-in" ",\"tools\":[{\"name\":\"edit\",\"expr\":\"1\"}]"
  r2 <- refusedBy "{c} in \"expr\" is not a declared parameter" ",\"tools\":[{\"name\":\"t\",\"expr\":\"f {c}\"}]"
  r3 <- refusedBy "type \"float\"" ",\"tools\":[{\"name\":\"t\",\"params\":{\"c\":{\"type\":\"float\"}},\"expr\":\"f {c}\"}]"
  r4 <- refusedBy "not a built-in tool" ",\"builtin_tools\":[\"nope\"]"
  r5 <- refusedBy "write_paths" ",\"write_paths\":[\"../x\"]"
  good <- conf (target ",\"tools\":[{\"name\":\"t\",\"expr\":\"1\"}],\"builtin_tools\":[\"read\"],\"instructions\":\"a.md\"")
  let keys = case good of
        Right c -> ( map dName (declaredOf c "a"), declaredOf c "b" == [], builtinToolsOf c "a", builtinToolsOf c "b", writePathsOf c "ab", writePathsOf c "ba", writePathsOf c "a"
                   , instructionsOf c "ab", instructionsOf c "ba", instructionsOf c "b" )
        Left e -> error e
  removeDirectoryRecursive root
  removeDirectoryRecursive outside
  pure
    [ ("fence: a file inside an allowed directory (new, and in directories not made yet) or an allowed file may be written"
      , and a)
    , ("fence: a file elsewhere is refused, as is a neighbour whose name begins with an allowed one; and the refusal says where writing is allowed"
      , not (or b) && "src, notes.md" `isInfixOf` msg && "outside write_paths" `isInfixOf` msg)
    , ("fence: a symlink out of the allowed places does not lead out of them (written through, or itself), and none given is anywhere in the project"
      , not (or leak) && not linkIn && free && not none)
    , ("config: a target is refused at load for a declared tool taking a built-in's name, an undeclared {param}, a type of none of the four, an unknown built-in, a write path outside the project"
      , and [r1, r2, r3, r4, r5])
    , ("config: tools, builtin_tools, write_paths and instructions are the first member of a composed session that sets them"
      , keys == (["t"], True, Just ["read"], Nothing, Just ["b"], Just ["b"], Nothing, Just "a.md", Just "b.md", Just "b.md"))
    ]
