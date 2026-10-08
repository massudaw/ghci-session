{-# LANGUAGE TemplateHaskell #-}
-- | Splices that run code of another module of the same package: the case a no-code typecheck must still handle.
module Splice.Use (answer, greeting) where

import Splice.Gen (mkGreeting, scaled)

-- | An expression splice.
answer :: Integer
answer = $(scaled 4)

-- | A declaration splice.
mkGreeting "hi"
