-- | Tests for readers

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Read
    ( testRead
    ) where

import Data.Char (isDigit, isHexDigit)
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Tests.QuickCheckUtils ()
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.Read as T

-- Reading.

t_decimal (n::Int) s =
    T.signed T.decimal (T.pack (show n) `T.append` t) === Right (n,t)
    where t = T.dropWhile isDigit s
tl_decimal (n::Int) s =
    TL.signed TL.decimal (TL.pack (show n) `TL.append` t) === Right (n,t)
    where t = TL.dropWhile isDigit s
t_hexadecimal m s ox =
    T.hexadecimal (T.concat [p, T.pack (showHex n ""), t]) === Right (n,t)
    where t = T.dropWhile isHexDigit s
          p = if ox then "0x" else ""
          n = getPositive m :: Int
tl_hexadecimal m s ox =
    TL.hexadecimal (TL.concat [p, TL.pack (showHex n ""), t]) === Right (n,t)
    where t = TL.dropWhile isHexDigit s
          p = if ox then "0x" else ""
          n = getPositive m :: Int

isFloaty c = c `elem` ("+-.0123456789eE" :: String)

t_read_rational p tol (n::Double) s =
    case p (T.pack (show n) `T.append` t) of
      Left err      -> counterexample err $ property False
      Right (n',t') -> t === t' .&&. property (abs (n-n') <= tol)
    where t = T.dropWhile isFloaty s

tl_read_rational p tol (n::Double) s =
    case p (TL.pack (show n) `TL.append` t) of
      Left err      -> counterexample err $ property False
      Right (n',t') -> t === t' .&&. property (abs (n-n') <= tol)
    where t = TL.dropWhile isFloaty s

t_double = t_read_rational T.double 1e-13
tl_double = tl_read_rational TL.double 1e-13
t_rational = t_read_rational T.rational 1e-16
tl_rational = tl_read_rational TL.rational 1e-16


testRead :: TestTree
testRead =
  testGroup "read" [
    testProperty "t_decimal" t_decimal,
    testProperty "tl_decimal" tl_decimal,
    testProperty "t_hexadecimal" t_hexadecimal,
    testProperty "tl_hexadecimal" tl_hexadecimal,
    testProperty "t_double" t_double,
    testProperty "tl_double" tl_double,
    testProperty "t_rational" t_rational,
    testProperty "tl_rational" tl_rational
  ]
