-- | Test instances

{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Instances
    ( testInstances
    ) where

import Data.String (IsString(fromString))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Tests.QuickCheckUtils
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion.Common as S
import qualified Data.Text.Lazy as TL

s_Eq s            = (s==)    `eq` ((S.streamList s==) . S.streamList)
    where _types = s :: String
sf_Eq (applyFun -> p) s =
    ((L.filter p s==) . L.filter p) `eq`
    (((S.filter p $ S.streamList s)==) . S.filter p . S.streamList)
t_Eq s            = (s==)    `eq` ((T.pack s==) . T.pack)
tl_Eq s           = (s==)    `eq` ((TL.pack s==) . TL.pack)
s_Ord s           = (compare s) `eq` (compare (S.streamList s) . S.streamList)
    where _types = s :: String
sf_Ord (applyFun -> p) s =
    ((compare $ L.filter p s) . L.filter p) `eq`
    (compare (S.filter p $ S.streamList s) . S.filter p . S.streamList)
t_Ord s           = (compare s) `eq` (compare (T.pack s) . T.pack)
tl_Ord s          = (compare s) `eq` (compare (TL.pack s) . TL.pack)
t_Read            = id       `eq` (T.unpack . read . show)
tl_Read           = id       `eq` (TL.unpack . read . show)
t_Show            = show     `eq` (show . T.pack)
tl_Show           = show     `eq` (show . TL.pack)
t_mappend s       = mappend s`eqP` (unpackS . mappend (T.pack s))
tl_mappend s      = mappend s`eqP` (unpackS . mappend (TL.pack s))
t_mconcat         = (mconcat . unSqrt) `eq` (unpackS . mconcat . L.map T.pack . unSqrt)
tl_mconcat        = (mconcat . unSqrt) `eq` (unpackS . mconcat . L.map TL.pack . unSqrt)
t_mempty          = mempty === (unpackS (mempty :: T.Text))
tl_mempty         = mempty === (unpackS (mempty :: TL.Text))
t_IsString        = fromString  `eqP` (T.unpack . fromString)
tl_IsString       = fromString  `eqP` (TL.unpack . fromString)

testInstances :: TestTree
testInstances =
  testGroup "instances" [
    testProperty "s_Eq" s_Eq,
    testProperty "sf_Eq" sf_Eq,
    testProperty "t_Eq" t_Eq,
    testProperty "tl_Eq" tl_Eq,
    testProperty "s_Ord" s_Ord,
    testProperty "sf_Ord" sf_Ord,
    testProperty "t_Ord" t_Ord,
    testProperty "tl_Ord" tl_Ord,
    testProperty "t_Read" t_Read,
    testProperty "tl_Read" tl_Read,
    testProperty "t_Show" t_Show,
    testProperty "tl_Show" tl_Show,
    testProperty "t_mappend" t_mappend,
    testProperty "tl_mappend" tl_mappend,
    testProperty "t_mconcat" t_mconcat,
    testProperty "tl_mconcat" tl_mconcat,
    testProperty "t_mempty" t_mempty,
    testProperty "tl_mempty" tl_mempty,
    testProperty "t_IsString" t_IsString,
    testProperty "tl_IsString" tl_IsString
  ]
