-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Text.ShowF
-- Copyright   :  (c) Conal Elliott
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Showable functors
----------------------------------------------------------------------

module Text.ShowF where

import Data.Functor ((<$>))

-- TODO: explicit exports

class ShowF f where
  showF      :: Show a =>        f a -> String
  showsPrecF :: Show a => Int -> f a -> ShowS
  showsPrecF _ x s = showF x ++ s
  showF x          = showsF x ""

newtype WrapShowF f a = WrapShowF (f a)

instance (ShowF f, Show a) => Show (WrapShowF f a) where
  showsPrec p (WrapShowF fa) = showsPrecF p fa

showsF :: (ShowF f, Show a) => f a -> ShowS
showsF =  showsPrecF 0

showsApp1 :: Show a => String -> Int -> a -> ShowS
showsApp1 s p a = strApp1 s p (showsPrec 11 a)

showsFApp1 :: (ShowF f, Show a) => String -> Int -> f a -> ShowS
showsFApp1 s p fa = strApp1 s p (showsPrecF 11 fa)

showsFComp1 :: (Functor g, ShowF g, ShowF f, Show a) => String -> Int -> g (f a) -> ShowS
showsFComp1 s p gfa = showsFApp1 s p (WrapShowF <$> gfa)

strApp1 :: String -> Int -> ShowS -> ShowS
strApp1 s p sh = showParen (p >= 11) $ showString s . showChar ' ' . sh

-- To do: Refactor more elegantly?
