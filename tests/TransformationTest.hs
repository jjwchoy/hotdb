{-# LANGUAGE OverloadedStrings #-}
module TransformationTest (tests) where

import Control.Monad

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit

import qualified HotDB.Core.Node as N
import qualified HotDB.Core.Operation as O
import qualified HotDB.Core.Transformation as T

import SCInstances ()
import Util (assertNothing)

tests :: TestTree
tests = testGroup "Transformation tests" [propertyTests, unitTests]

c :: N.Node
c = N.IntNode 123

c2 :: N.Node
c2 = N.BoolNode False

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "NoOps do not transform anything" $
     \op -> T.transform op O.NoOp T.Left == Just op &&
             T.transform op O.NoOp T.Right == Just op
  , testProperty "IntIncs do not interfere" $
      \(x,y) -> let o1 = O.IntInc $ fromInteger x
                    o2 = O.IntInc $ fromInteger y in
                  T.transform o1 o2 T.Left == Just o1 &&
                  T.transform o1 o2 T.Right == Just o1
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testRootSetRootSet
  , testTextInsertTextInsert
  , testTextInsertTextDelete
  , testTextDeleteTextInsert
  , testTextDeleteTextDelete
  , testSequenceSetSequenceSet
  , testSequenceSetSequenceInsert
  , testSequenceSetSequenceDelete
  , testSequenceInsertSequenceSet
  , testSequenceInsertSequenceInsert
  , testSequenceInsertSequenceDelete
  , testSequenceDeleteSequenceSet
  , testSequenceDeleteSequenceInsert
  , testSequenceDeleteSequenceDelete
  , testMapSetMapSet
  , testMapSetMapUnset
  , testMapUnsetMapSet
  , testMapUnsetMapUnset
  , testTransformChildRootSet
  , testTransformChildSequenceSet
  , testTransformChildSequenceInsert
  , testTransformChildSequenceDelete
  , testTransformChildMapSet
  , testTransformChildMapUnset
  , testTransformChildNonAffecting
  ]

testRootSetRootSet :: TestTree
testRootSetRootSet = 
  let rs1 = O.RootSet (N.IntNode 123) 1
      rs2 = O.RootSet (N.BoolNode False) 2
      rs3 = O.RootSet (N.BoolNode False) 1 in
  testGroup "RootSet vs RootSet"
  [ testCase "Wins tiebreaker" $ do
      assertEqual "a" (Just rs2) $ T.transform rs2 rs1 T.Left
      assertEqual "b" (Just rs2) $ T.transform rs2 rs1 T.Right
      assertEqual "c" (Just rs1) $ T.transform rs1 rs3 T.Right
  , testCase "Loses tiebreaker" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform rs1 rs2 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform rs1 rs2 T.Right
      assertEqual "c" (Just O.NoOp) $ T.transform rs1 rs3 T.Left
  , testCase "EqualValues" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform rs2 rs3 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform rs2 rs3 T.Right
      assertEqual "c" (Just O.NoOp) $ T.transform rs3 rs2 T.Left
      assertEqual "c" (Just O.NoOp) $ T.transform rs3 rs2 T.Right
  ]

testTextInsertTextInsert :: TestTree
testTextInsertTextInsert = 
  let ti1 = O.TextInsert 0 'x' 0
      ti2 = O.TextInsert 0 'y' 1
      ti3 = O.TextInsert 0 'z' 0
      ti4 = O.TextInsert 1 '_' 10 in
  testGroup "TextInsert vs TextInsert"
  [ testCase "Wins tiebreaker" $ do
      assertEqual "a" (Just ti2) $ T.transform ti2 ti1 T.Left
      assertEqual "b" (Just ti2) $ T.transform ti2 ti1 T.Right
      assertEqual "c" (Just ti1) $ T.transform ti1 ti3 T.Right
  , testCase "Loses tiebreaker" $ do
      assertEqual "a" (Just $ O.TextInsert 1 'x' 0) $ T.transform ti1 ti2 T.Left
      assertEqual "b" (Just $ O.TextInsert 1 'x' 0) $ T.transform ti1 ti2 T.Right
      assertEqual "c" (Just $ O.TextInsert 1 'x' 0) $ T.transform ti1 ti3 T.Left
  , testCase "Not affected" $ do
      assertEqual "a" (Just ti1) $ T.transform ti1 ti4 T.Left
      assertEqual "b" (Just ti1) $ T.transform ti1 ti4 T.Right
  , testCase "Index incremented" $ do
      assertEqual "a" (Just $ O.TextInsert 2 '_' 10) $ T.transform ti4 ti1 T.Left
      assertEqual "b" (Just $ O.TextInsert 2 '_' 10) $ T.transform ti4 ti1 T.Right
  ]

testTextInsertTextDelete :: TestTree
testTextInsertTextDelete =
  let ti1 = O.TextInsert 0 'x' 0
      td1 = O.TextDelete 0
      ti2 = O.TextInsert 1 'y' 0
      td2 = O.TextDelete 1 in
  testGroup "TextInsert vs TextDelete"
  [ testCase "Delete >= Insert" $ do
      assertEqual "a" (Just ti1) $ T.transform ti1 td1 T.Left
      assertEqual "b" (Just ti1) $ T.transform ti1 td1 T.Right
      assertEqual "c" (Just ti1) $ T.transform ti1 td2 T.Left
      assertEqual "d" (Just ti1) $ T.transform ti1 td2 T.Right
  , testCase "Delete < Insert" $ do
      assertEqual "a" (Just $ O.TextInsert 0 'y' 0) $ T.transform ti2 td1 T.Left
      assertEqual "b" (Just $ O.TextInsert 0 'y' 0) $ T.transform ti2 td1 T.Right
  ]

testTextDeleteTextInsert :: TestTree
testTextDeleteTextInsert =
  let ti1 = O.TextInsert 0 'x' 0
      td1 = O.TextDelete 0
      ti2 = O.TextInsert 1 'y' 0
      td2 = O.TextDelete 1 in
  testGroup "TextDelete vs TextInsert"
  [ testCase "Delete < Insert" $ do
      assertEqual "a" (Just td1) $ T.transform td1 ti2 T.Left
      assertEqual "b" (Just td1) $ T.transform td1 ti2 T.Right
  , testCase "Delete >= Insert" $ do
      assertEqual "a" (Just $ O.TextDelete 1) $ T.transform td1 ti1 T.Left
      assertEqual "b" (Just $ O.TextDelete 1) $ T.transform td1 ti1 T.Right
      assertEqual "c" (Just $ O.TextDelete 2) $ T.transform td2 ti1 T.Left
      assertEqual "c" (Just $ O.TextDelete 2) $ T.transform td2 ti1 T.Right
  ]

testTextDeleteTextDelete :: TestTree
testTextDeleteTextDelete =
  let td1 = O.TextDelete 0
      td2 = O.TextDelete 1 in
  testGroup "TextDelete vs TextDelete"
  [ testCase "Before other" $ do
      assertEqual "a" (Just td1) $ T.transform td1 td2 T.Left
      assertEqual "b" (Just td1) $ T.transform td1 td2 T.Right
  , testCase "Equal to other" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform td1 td1 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform td1 td1 T.Right
  , testCase "After other" $ do
      assertEqual "a" (Just td1) $ T.transform td2 td1 T.Left
      assertEqual "b" (Just td1) $ T.transform td2 td1 T.Right
  ]

testSequenceSetSequenceSet :: TestTree
testSequenceSetSequenceSet =
  let ss1 = O.SequenceSet 0 c 0
      ss2 = O.SequenceSet 0 c2 1
      ss3 = O.SequenceSet 1 c 0
      ss4 = O.SequenceSet 0 c2 0 in
  testGroup "SequenceSet vs SequenceSet"
  [ testCase "Wins tiebreaker" $ do
      assertEqual "a" (Just ss2) $ T.transform ss2 ss1 T.Left
      assertEqual "b" (Just ss2) $ T.transform ss2 ss1 T.Right
      assertEqual "c" (Just ss1) $ T.transform ss1 ss4 T.Right
  , testCase "Loses tiebreaker" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform ss1 ss2 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform ss1 ss2 T.Right
      assertEqual "c" (Just O.NoOp) $ T.transform ss1 ss4 T.Left
  , testCase "Non-conflicting" $ do
      assertEqual "a" (Just ss1) $ T.transform ss1 ss3 T.Left
      assertEqual "b" (Just ss1) $ T.transform ss1 ss3 T.Right
      assertEqual "c" (Just ss3) $ T.transform ss3 ss1 T.Left
      assertEqual "d" (Just ss3) $ T.transform ss3 ss1 T.Right
  , testCase "Equal objects" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform ss2 ss4 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform ss2 ss4 T.Right
      assertEqual "c" (Just O.NoOp) $ T.transform ss4 ss2 T.Left
      assertEqual "d" (Just O.NoOp) $ T.transform ss4 ss2 T.Right
  ]

testSequenceSetSequenceInsert :: TestTree
testSequenceSetSequenceInsert =
  let ss1 = O.SequenceSet 0 c 0
      ss2 = O.SequenceSet 1 c 0
      si1 = O.SequenceInsert 0 c 0
      si2 = O.SequenceInsert 1 c 0 in
  testGroup "SequenceSet vs SequenceInsert"
  [ testCase "Non-affecting" $ do
      assertEqual "a" (Just ss1) $ T.transform ss1 si2 T.Left
      assertEqual "b" (Just ss1) $ T.transform ss1 si2 T.Right
  , testCase "Shifted" $ do
      assertEqual "a" (Just ss2) $ T.transform ss1 si1 T.Left
      assertEqual "b" (Just ss2) $ T.transform ss1 si1 T.Right
      assertEqual "c" (Just $ O.SequenceSet 2 c 0) $ T.transform ss2 si1 T.Left
      assertEqual "d" (Just $ O.SequenceSet 2 c 0) $ T.transform ss2 si1 T.Right
  ]

testSequenceSetSequenceDelete :: TestTree
testSequenceSetSequenceDelete =
  let ss1 = O.SequenceSet 0 c 0
      ss2 = O.SequenceSet 1 c 0
      sd1 = O.SequenceDelete 0
      sd2 = O.SequenceDelete 1 in
  testGroup "SequenceSet vs SequenceDelete"
  [ testCase "Non-affecting" $ do
      assertEqual "a" (Just ss1) $ T.transform ss1 sd2 T.Left
      assertEqual "b" (Just ss1) $ T.transform ss1 sd2 T.Right
  , testCase "Same index"$ do
      assertEqual "a" (Just O.NoOp) $ T.transform ss1 sd1 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform ss1 sd1 T.Right
  , testCase "Shifted" $ do
      assertEqual "a" (Just ss1) $ T.transform ss2 sd1 T.Left
      assertEqual "b" (Just ss1) $ T.transform ss2 sd1 T.Right
  ]

testSequenceInsertSequenceSet :: TestTree
testSequenceInsertSequenceSet =
  let si1 = O.SequenceInsert 0 c 0
      si2 = O.SequenceInsert 1 c 0
      ss1 = O.SequenceSet 0 c 0
      ss2 = O.SequenceSet 1 c 0 in
  testGroup "SequenceInsert vs SequenceSet"
  [ testCase "Non-affecting" $ do
      assertEqual "a" (Just si1) $ T.transform si1 ss1 T.Left
      assertEqual "b" (Just si1) $ T.transform si1 ss1 T.Right
      assertEqual "c" (Just si1) $ T.transform si1 ss2 T.Left
      assertEqual "d" (Just si1) $ T.transform si1 ss2 T.Right
      assertEqual "e" (Just si2) $ T.transform si2 ss1 T.Left
      assertEqual "f" (Just si2) $ T.transform si2 ss1 T.Right
  ]

testSequenceInsertSequenceInsert :: TestTree
testSequenceInsertSequenceInsert = 
  let si1 = O.SequenceInsert 0 c 0
      si2 = O.SequenceInsert 0 c 1
      si3 = O.SequenceInsert 0 c 0
      si4 = O.SequenceInsert 1 c 10 in
  testGroup "SequenceInsert vs SequenceInsert"
  [ testCase "Wins tiebreaker" $ do
      assertEqual "a" (Just si2) $ T.transform si2 si1 T.Left
      assertEqual "b" (Just si2) $ T.transform si2 si1 T.Right
      assertEqual "c" (Just si1) $ T.transform si1 si3 T.Right
  , testCase "Loses tiebreaker" $ do
      assertEqual "a" (Just $ O.SequenceInsert 1 c 0) $ T.transform si1 si2 T.Left
      assertEqual "b" (Just $ O.SequenceInsert 1 c 0) $ T.transform si1 si2 T.Right
      assertEqual "c" (Just $ O.SequenceInsert 1 c 0) $ T.transform si1 si3 T.Left
  , testCase "Not affected" $ do
      assertEqual "a" (Just si1) $ T.transform si1 si4 T.Left
      assertEqual "b" (Just si1) $ T.transform si1 si4 T.Right
  , testCase "Index incremented" $ do
      assertEqual "a" (Just $ O.SequenceInsert 2 c 10) $ T.transform si4 si1 T.Left
      assertEqual "b" (Just $ O.SequenceInsert 2 c 10) $ T.transform si4 si1 T.Right
  ]

testSequenceInsertSequenceDelete :: TestTree
testSequenceInsertSequenceDelete =
  let si1 = O.SequenceInsert 0 c 0
      sd1 = O.SequenceDelete 0
      si2 = O.SequenceInsert 1 c 0
      sd2 = O.SequenceDelete 1 in
  testGroup "SequenceInsert vs SequenceDelete"
  [ testCase "Delete >= Insert" $ do
      assertEqual "a" (Just si1) $ T.transform si1 sd1 T.Left
      assertEqual "b" (Just si1) $ T.transform si1 sd1 T.Right
      assertEqual "c" (Just si1) $ T.transform si1 sd2 T.Left
      assertEqual "d" (Just si1) $ T.transform si1 sd2 T.Right
  , testCase "Delete < Insert" $ do
      assertEqual "a" (Just $ O.SequenceInsert 0 c 0) $ T.transform si2 sd1 T.Left
      assertEqual "b" (Just $ O.SequenceInsert 0 c 0) $ T.transform si2 sd1 T.Right
  ]

testSequenceDeleteSequenceSet :: TestTree
testSequenceDeleteSequenceSet =
  let ss1 = O.SequenceSet 0 c 0
      ss2 = O.SequenceSet 1 c 0
      sd1 = O.SequenceDelete 0
      sd2 = O.SequenceDelete 1 in
  testGroup "SequenceDelete vs SequenceSet"
  [ testCase "Non-affecting" $ do
      assertEqual "a" (Just sd1) $ T.transform sd1 ss1 T.Left
      assertEqual "b" (Just sd1) $ T.transform sd1 ss1 T.Right
      assertEqual "c" (Just sd1) $ T.transform sd1 ss2 T.Left
      assertEqual "d" (Just sd1) $ T.transform sd1 ss2 T.Right
      assertEqual "e" (Just sd2) $ T.transform sd2 ss1 T.Left
      assertEqual "f" (Just sd2) $ T.transform sd2 ss1 T.Right
  ]

testSequenceDeleteSequenceInsert :: TestTree
testSequenceDeleteSequenceInsert =
  let si1 = O.SequenceInsert 0 c 0
      sd1 = O.SequenceDelete 0
      si2 = O.SequenceInsert 1 c 0
      sd2 = O.SequenceDelete 1 in
  testGroup "SequenceDelete vs SequenceInsert"
  [ testCase "Delete < Insert" $ do
      assertEqual "a" (Just sd1) $ T.transform sd1 si2 T.Left
      assertEqual "b" (Just sd1) $ T.transform sd1 si2 T.Right
  , testCase "Delete >= Insert" $ do
      assertEqual "a" (Just $ O.SequenceDelete 1) $ T.transform sd1 si1 T.Left
      assertEqual "b" (Just $ O.SequenceDelete 1) $ T.transform sd1 si1 T.Right
      assertEqual "c" (Just $ O.SequenceDelete 2) $ T.transform sd2 si1 T.Left
      assertEqual "c" (Just $ O.SequenceDelete 2) $ T.transform sd2 si1 T.Right
  ]

testSequenceDeleteSequenceDelete :: TestTree
testSequenceDeleteSequenceDelete =
  let sd1 = O.SequenceDelete 0
      sd2 = O.SequenceDelete 1 in
  testGroup "SequenceDelete vs SequenceDelete"
  [ testCase "Before other" $ do
      assertEqual "a" (Just sd1) $ T.transform sd1 sd2 T.Left
      assertEqual "b" (Just sd1) $ T.transform sd1 sd2 T.Right
  , testCase "Equal to other" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform sd1 sd1 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform sd1 sd1 T.Right
  , testCase "After other" $ do
      assertEqual "a" (Just sd1) $ T.transform sd2 sd1 T.Left
      assertEqual "b" (Just sd1) $ T.transform sd2 sd1 T.Right
  ]

testMapSetMapSet :: TestTree
testMapSetMapSet =
  let ms1 = O.MapSet "foo" c 0
      ms2 = O.MapSet "foo" c2 1
      ms3 = O.MapSet "bar" c 0
      ms4 = O.MapSet "foo" c 1 in
  testGroup "MapSet vs MapSet"
  [ testCase "Non-conflicting" $ do
      assertEqual "a" (Just ms1) $ T.transform ms1 ms3 T.Left
      assertEqual "b" (Just ms1) $ T.transform ms1 ms3 T.Right
  , testCase "Wins tiebreaker" $ do
      assertEqual "a" (Just ms2) $ T.transform ms2 ms1 T.Left
      assertEqual "b" (Just ms2) $ T.transform ms2 ms1 T.Right
      assertEqual "c" (Just ms2) $ T.transform ms2 ms4 T.Right
  , testCase "Loses tiebreaker" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform ms1 ms2 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform ms1 ms2 T.Right
      assertEqual "c" (Just O.NoOp) $ T.transform ms2 ms4 T.Left
  , testCase "Equal objects" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform ms1 ms4 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform ms1 ms4 T.Right
      assertEqual "c" (Just O.NoOp) $ T.transform ms4 ms1 T.Left
      assertEqual "d" (Just O.NoOp) $ T.transform ms4 ms1 T.Right
  ]

testMapSetMapUnset :: TestTree
testMapSetMapUnset =
  let ms1 = O.MapSet "foo" c 0
      mu1 = O.MapUnset "foo"
      mu2 = O.MapUnset "bar" in
  testGroup "MapSet vs MapUnset"
  [ testCase "Non-conflicting" $ do
      assertEqual "a" (Just ms1) $ T.transform ms1 mu1 T.Left
      assertEqual "b" (Just ms1) $ T.transform ms1 mu1 T.Right
      assertEqual "c" (Just ms1) $ T.transform ms1 mu2 T.Left
      assertEqual "d" (Just ms1) $ T.transform ms1 mu2 T.Right
  ]

testMapUnsetMapSet :: TestTree
testMapUnsetMapSet =
  let mu1 = O.MapUnset "foo"
      mu2 = O.MapUnset "bar"
      ms1 = O.MapSet "foo" c 0 in
  testGroup "MapUnset vs MapSet"
  [ testCase "Non-conflicting" $ do
      assertEqual "a" (Just mu2) $ T.transform mu2 ms1 T.Left
      assertEqual "b" (Just mu2) $ T.transform mu2 ms1 T.Right
  , testCase "Conflicting" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform mu1 ms1 T.Left
      assertEqual "b" (Just O.NoOp) $ T.transform mu1 ms1 T.Right
  ]

testMapUnsetMapUnset :: TestTree
testMapUnsetMapUnset =
  let mu1 = O.MapUnset "foo"
      mu2 = O.MapUnset "bar" in
  testGroup "MapUnset vs MapUnset"
  [ testCase "Non-conflicting" $ do
      assertEqual "a" (Just mu1) $ T.transform mu1 mu2 T.Left
      assertEqual "b" (Just mu1) $ T.transform mu1 mu2 T.Right
  , testCase "Conflicting" $ do
      assertEqual "a" (Just O.NoOp) $ T.transform mu1 mu1 T.Left
      assertEqual "a" (Just O.NoOp) $ T.transform mu1 mu1 T.Right
  ]

testTransformChildRootSet :: TestTree
testTransformChildRootSet =
  let dop1 = O.DirectedOperation "/" $ O.IntInc 1
      dop2 = O.DirectedOperation "/foo" $ O.IntInc 1
      dop3 = O.DirectedOperation "" $ O.IntInc 1
      rs = O.DirectedOperation "" $ O.RootSet c 0 in
  testCase "Transform Child against RootSet" $ do
    assertEqual "a" (Just O.directedNoOp) $ T.transformDirected dop1 rs T.Left
    assertEqual "b" (Just O.directedNoOp) $ T.transformDirected dop1 rs T.Right
    assertEqual "c" (Just O.directedNoOp) $ T.transformDirected dop2 rs T.Left
    assertEqual "d" (Just O.directedNoOp) $ T.transformDirected dop2 rs T.Right
    assertEqual "e" Nothing $ T.transformDirected dop3 rs T.Left
    assertEqual "f" Nothing $ T.transformDirected dop3 rs T.Right


testTransformChildSequenceSet :: TestTree
testTransformChildSequenceSet =
  let dop1 = O.DirectedOperation "3/foo" $ O.IntInc 1
      dop2 = O.DirectedOperation "foo/bar" $ O.IntInc 1
      dop3 = O.DirectedOperation "" $ O.IntInc 1
      ss1 = O.DirectedOperation "" $ O.SequenceSet 2 c 0
      ss2 = O.DirectedOperation "" $ O.SequenceSet 3 c 0
      ss3 = O.DirectedOperation "" $ O.SequenceSet 4 c 0 in
  testCase "Transform Child against SequenceSet" $ do
    assertEqual "a" (Just dop1) $ T.transformDirected dop1 ss1 T.Left
    assertEqual "b" (Just dop1) $ T.transformDirected dop1 ss1 T.Right
    assertEqual "c" (Just O.directedNoOp) $ T.transformDirected dop1 ss2 T.Left
    assertEqual "d" (Just O.directedNoOp) $ T.transformDirected dop1 ss2 T.Right
    assertEqual "e" (Just dop1) $ T.transformDirected dop1 ss3 T.Left
    assertEqual "f" (Just dop1) $ T.transformDirected dop1 ss3 T.Right
    assertEqual "g" Nothing $ T.transformDirected dop2 ss1 T.Left
    assertEqual "h" Nothing $ T.transformDirected dop2 ss1 T.Right
    assertEqual "i" Nothing $ T.transformDirected dop3 ss1 T.Left
    assertEqual "j" Nothing $ T.transformDirected dop3 ss1 T.Right

testTransformChildSequenceInsert :: TestTree
testTransformChildSequenceInsert =
  let dop1 = O.DirectedOperation "3/foo" $ O.IntInc 1
      dop2 = O.DirectedOperation "foo/bar" $ O.IntInc 1
      dop3 = O.DirectedOperation "" $ O.IntInc 1
      dop4 = O.DirectedOperation "4/foo" $ O.IntInc 1
      ss1 = O.DirectedOperation "" $ O.SequenceInsert 2 c 0
      ss2 = O.DirectedOperation "" $ O.SequenceInsert 3 c 0
      ss3 = O.DirectedOperation "" $ O.SequenceInsert 4 c 0 in
  testCase "Transform Child against SequenceSet" $ do
    assertEqual "a" (Just dop4) $ T.transformDirected dop1 ss1 T.Left
    assertEqual "b" (Just dop4) $ T.transformDirected dop1 ss1 T.Right
    assertEqual "c" (Just dop4) $ T.transformDirected dop1 ss2 T.Left
    assertEqual "d" (Just dop4) $ T.transformDirected dop1 ss2 T.Right
    assertEqual "e" (Just dop1) $ T.transformDirected dop1 ss3 T.Left
    assertEqual "f" (Just dop1) $ T.transformDirected dop1 ss3 T.Right
    assertEqual "g" Nothing $ T.transformDirected dop2 ss1 T.Left
    assertEqual "h" Nothing $ T.transformDirected dop2 ss1 T.Right
    assertEqual "i" Nothing $ T.transformDirected dop3 ss1 T.Left
    assertEqual "j" Nothing $ T.transformDirected dop3 ss1 T.Right

testTransformChildSequenceDelete :: TestTree
testTransformChildSequenceDelete =
  let dop1 = O.DirectedOperation "3/foo" $ O.IntInc 1
      dop2 = O.DirectedOperation "foo/bar" $ O.IntInc 1
      dop3 = O.DirectedOperation "" $ O.IntInc 1
      dop4 = O.DirectedOperation "2/foo" $ O.IntInc 1
      ss1 = O.DirectedOperation "" $ O.SequenceDelete 2
      ss2 = O.DirectedOperation "" $ O.SequenceDelete 3
      ss3 = O.DirectedOperation "" $ O.SequenceDelete 4 in
  testCase "Transform Child against SequenceSet" $ do
    assertEqual "a" (Just dop4) $ T.transformDirected dop1 ss1 T.Left
    assertEqual "b" (Just dop4) $ T.transformDirected dop1 ss1 T.Right
    assertEqual "c" (Just O.directedNoOp) $ T.transformDirected dop1 ss2 T.Left
    assertEqual "d" (Just O.directedNoOp) $ T.transformDirected dop1 ss2 T.Right
    assertEqual "e" (Just dop1) $ T.transformDirected dop1 ss3 T.Left
    assertEqual "f" (Just dop1) $ T.transformDirected dop1 ss3 T.Right
    assertEqual "g" Nothing $ T.transformDirected dop2 ss1 T.Left
    assertEqual "h" Nothing $ T.transformDirected dop2 ss1 T.Right
    assertEqual "i" Nothing $ T.transformDirected dop3 ss1 T.Left
    assertEqual "j" Nothing $ T.transformDirected dop3 ss1 T.Right

testTransformChildMapSet :: TestTree
testTransformChildMapSet =
  let dop1 = O.DirectedOperation "foo/bar" $ O.IntInc 1
      dop2 = O.DirectedOperation "" $ O.IntInc 1
      ss1 = O.DirectedOperation "" $ O.MapSet "foo" c 0
      ss2 = O.DirectedOperation "" $ O.MapSet "bar" c 0 in
  testCase "Transform Child against MapSet" $ do
    assertEqual "a" (Just dop1) $ T.transformDirected dop1 ss2 T.Left
    assertEqual "b" (Just dop1) $ T.transformDirected dop1 ss2 T.Right
    assertEqual "c" (Just O.directedNoOp) $ T.transformDirected dop1 ss1 T.Left
    assertEqual "d" (Just O.directedNoOp) $ T.transformDirected dop1 ss1 T.Right
    assertEqual "e" Nothing $ T.transformDirected dop2 ss1 T.Left
    assertEqual "f" Nothing $ T.transformDirected dop2 ss1 T.Right

testTransformChildMapUnset :: TestTree
testTransformChildMapUnset =
  let dop1 = O.DirectedOperation "foo/bar" $ O.IntInc 1
      dop2 = O.DirectedOperation "" $ O.IntInc 1
      ss1 = O.DirectedOperation "" $ O.MapUnset "foo"
      ss2 = O.DirectedOperation "" $ O.MapUnset "bar" in
  testCase "Transform Child against MapUnset" $ do
    assertEqual "a" (Just dop1) $ T.transformDirected dop1 ss2 T.Left
    assertEqual "b" (Just dop1) $ T.transformDirected dop1 ss2 T.Right
    assertEqual "c" (Just O.directedNoOp) $ T.transformDirected dop1 ss1 T.Left
    assertEqual "d" (Just O.directedNoOp) $ T.transformDirected dop1 ss1 T.Right
    assertEqual "e" Nothing $ T.transformDirected dop2 ss2 T.Left
    assertEqual "f" Nothing $ T.transformDirected dop2 ss2 T.Right

testTransformChildNonAffecting :: TestTree
testTransformChildNonAffecting =
  let dop1 = O.DirectedOperation "foo/bar" $ O.IntInc 1
      dop1' = O.DirectedOperation "foo/0" $ O.IntInc 1
      dop2 = O.DirectedOperation "foo/bar/baz" $ O.MapUnset "bar"
      dop3 = O.DirectedOperation "foo/baz" $ O.MapUnset "baz"
      dop4 = O.DirectedOperation "baz" $ O.MapUnset "baz"
      dop5 = O.DirectedOperation "foo/bar/baz" $ O.MapSet "bar" c 0
      dop6 = O.DirectedOperation "foo/baz" $ O.MapSet "baz" c 0
      dop7 = O.DirectedOperation "baz" $ O.MapSet "baz" c 0
      dop8 = O.DirectedOperation "foo/bar/baz" $ O.SequenceSet 0 c 0
      dop9 = O.DirectedOperation "foo/baz" $ O.SequenceSet 0 c 0
      dop10 = O.DirectedOperation "baz" $ O.SequenceSet 0 c 0
      dop11 = O.DirectedOperation "foo/bar/baz" $ O.SequenceInsert 0 c 0
      dop12 = O.DirectedOperation "foo/baz" $ O.SequenceInsert 0 c 0
      dop13 = O.DirectedOperation "baz" $ O.SequenceInsert 0 c 0
      dop14 = O.DirectedOperation "foo/bar/baz" $ O.SequenceDelete 0
      dop15 = O.DirectedOperation "foo/baz" $ O.SequenceDelete 0
      dop16 = O.DirectedOperation "baz" $ O.SequenceDelete 0
      dop17 = O.DirectedOperation "foo/bar/baz" $ O.RootSet c 0
      dop18 = O.DirectedOperation "foo/baz" $ O.RootSet c 0
      dop19 = O.DirectedOperation "baz" $ O.RootSet c 0
      in
  testCase "Transform Child Non-Affecting" $ do
    assertEqual "a" (Just dop1) $ T.transformDirected dop1 dop2 T.Left
    assertEqual "b" (Just dop1) $ T.transformDirected dop1 dop2 T.Right
    assertEqual "c" (Just dop1) $ T.transformDirected dop1 dop3 T.Left
    assertEqual "d" (Just dop1) $ T.transformDirected dop1 dop3 T.Right
    assertEqual "e" (Just dop1) $ T.transformDirected dop1 dop4 T.Left
    assertEqual "f" (Just dop1) $ T.transformDirected dop1 dop4 T.Right
    assertEqual "g" (Just dop1) $ T.transformDirected dop1 dop5 T.Left
    assertEqual "h" (Just dop1) $ T.transformDirected dop1 dop5 T.Right
    assertEqual "i" (Just dop1) $ T.transformDirected dop1 dop6 T.Left
    assertEqual "j" (Just dop1) $ T.transformDirected dop1 dop6 T.Right
    assertEqual "k" (Just dop1) $ T.transformDirected dop1 dop7 T.Left
    assertEqual "l" (Just dop1) $ T.transformDirected dop1 dop7 T.Right
    assertEqual "m" (Just dop1') $ T.transformDirected dop1' dop8 T.Left
    assertEqual "n" (Just dop1') $ T.transformDirected dop1' dop8 T.Right
    assertEqual "o" (Just dop1') $ T.transformDirected dop1' dop9 T.Left
    assertEqual "p" (Just dop1') $ T.transformDirected dop1' dop9 T.Right
    assertEqual "q" (Just dop1') $ T.transformDirected dop1' dop10 T.Left
    assertEqual "r" (Just dop1') $ T.transformDirected dop1' dop10 T.Right
    assertEqual "s" (Just dop1') $ T.transformDirected dop1' dop11 T.Left
    assertEqual "t" (Just dop1') $ T.transformDirected dop1' dop11 T.Right
    assertEqual "u" (Just dop1') $ T.transformDirected dop1' dop12 T.Left
    assertEqual "v" (Just dop1') $ T.transformDirected dop1' dop12 T.Right
    assertEqual "w" (Just dop1') $ T.transformDirected dop1' dop13 T.Left
    assertEqual "x" (Just dop1') $ T.transformDirected dop1' dop13 T.Right
    assertEqual "y" (Just dop1') $ T.transformDirected dop1' dop14 T.Left
    assertEqual "z" (Just dop1') $ T.transformDirected dop1' dop14 T.Right
    assertEqual "aa" (Just dop1') $ T.transformDirected dop1' dop15 T.Left
    assertEqual "ab" (Just dop1') $ T.transformDirected dop1' dop15 T.Right
    assertEqual "ac" (Just dop1') $ T.transformDirected dop1' dop16 T.Left
    assertEqual "ad" (Just dop1') $ T.transformDirected dop1' dop16 T.Right
    assertEqual "ae" (Just dop1') $ T.transformDirected dop1' dop17 T.Left
    assertEqual "af" (Just dop1') $ T.transformDirected dop1' dop17 T.Right
    assertEqual "ag" (Just dop1') $ T.transformDirected dop1' dop18 T.Left
    assertEqual "ah" (Just dop1') $ T.transformDirected dop1' dop18 T.Right
    assertEqual "ai" (Just dop1') $ T.transformDirected dop1' dop19 T.Left
    assertEqual "aj" (Just dop1') $ T.transformDirected dop1' dop19 T.Right
