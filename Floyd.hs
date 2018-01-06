module Floyd where

import qualified Data.Stream as S

floyd :: (Eq a) => S.Stream a -> Int
floyd xs = let find = S.findIndex (uncurry (==)) . S.zip xs
               period = 1 + find (alternate xs)
            in find (S.drop period xs)

alternate :: S.Stream a -> S.Stream a
alternate (S.Cons _ (S.Cons x xs)) = S.Cons x (alternate xs)
