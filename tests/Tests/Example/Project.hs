module Tests.Example.Project where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Example.Project

prop_zeroRegAlwaysZero :: H.Property
prop_zeroRegAlwaysZero = H.property $ do
  a <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  b <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  m_registers (step initState (0, IImm 0 100)) ! 0 === 0

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
