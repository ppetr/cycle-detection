{-# LANGUAGE TemplateHaskell #-}
module MyTests where

import Floyd

import qualified Data.Stream as S
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.All

prop_floyd :: Word16 -> Word16 -> Property
prop_floyd u v =
   counterexample ("Head: " ++ show head ++ ", total length: " ++ show len) $
   -- counterexample ("Head: " ++ show hs ++ ", period: " ++ show ts) $
   floyd xs === fromIntegral head
  where
    head = min u v
    len = 1 + max u v
    hs = [1..head]
    ts = [(head + 1)..len]
    xs = hs  `S.prefix` S.cycle ts

--------------------------
return []
main = $quickCheckAll
