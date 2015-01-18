{-# LANGUAGE OverloadedStrings #-}
module OperationTest (tests) where

import Control.Monad

import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Text (Text)
import Data.Word (Word32)

import Test.Tasty
import Test.Tasty.HUnit

import qualified HotDB.Core.Node as N
import qualified HotDB.Core.Operation as O
import qualified HotDB.Core.Path as P

import Util (assertNothing)

tests :: TestTree
tests = testGroup "Operation tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "RootSet" testRootSet
  , testCase "RootSetInvalid" testRootSetInvalid
  , testCase "IntInc" testIntInc
  , testCase "TextInsert" testTextInsert
  , testCase "TextInsertInvalid" testTextInsertInvalid
  , testCase "TextDelete" testTextDelete
  , testCase "TextDeleteInvalid" testTextDeleteInvalid
  , testCase "SequenceSet" testSequenceSet
  , testCase "SequenceSetInvalid" testSequenceSetInvalid
  , testCase "SequenceInsert" testSequenceInsert
  , testCase "SequenceInsertInvalid" testSequenceInsertInvalid
  , testCase "SequenceDelete" testSequenceDelete
  , testCase "SequenceDeleteInvalid" testSequenceDeleteInvalid
  , testCase "MapSet" testMapSet
  , testCase "MapUnset" testMapUnset
  , testCase "MapUnsetInvalid" testMapUnsetInvalid
  , testCase "NestedOps" testNestedOps
  , testCase "NestedOpsInvalid" testNestedOpsInvalid
  ]

c :: N.Node
c = N.IntNode 123

testRootSet :: Assertion
testRootSet = do
  let r = N.RootNode N.EmptyNode
  let o = O.DirectedOperation "" $ O.RootSet c 1
  let e = N.RootNode c
  assertEqual "Expected (RootNode (IntNode 123))" (Just e) $ O.applyPath o r

testRootSetInvalid :: Assertion
testRootSetInvalid = do
  let o = O.DirectedOperation "" $ O.RootSet c 1
  assertNothing "Expected nothing" $ O.applyPath o c

assertIntInc :: Int64 -> Int64 -> Assertion
assertIntInc initial delta = assertEqual "Expected (IntNode (initial + delta))" (Just $ N.IntNode $ initial + delta) $ O.applyPath (O.DirectedOperation "" $ O.IntInc delta) $ N.IntNode initial

testIntInc :: Assertion
testIntInc = do
  assertIntInc 0 123
  assertIntInc 0 (-123)
  assertIntInc (2 ^ 64 - 1) 5

assertTextInsert :: String -> Word32 -> Char -> String -> Assertion
assertTextInsert initial pos c expected = assertEqual "Expected given string" (Just $ N.TextNode expected) $ O.applyPath (O.DirectedOperation "" $ O.TextInsert pos c 1) $ N.TextNode initial

testTextInsert :: Assertion
testTextInsert = do
  assertTextInsert "abc" 0 '_' "_abc"
  assertTextInsert "" 0 '_' "_"
  assertTextInsert "abc" 1 'x' "axbc"
  assertTextInsert "abc" 3 'd' "abcd"

assertTextInsertInvalid :: String -> Word32 -> Char -> Assertion
assertTextInsertInvalid initial p c = assertNothing "Expected nothing" $ O.applyPath (O.DirectedOperation "" $ O.TextInsert p c 1) $ N.TextNode initial

testTextInsertInvalid :: Assertion
testTextInsertInvalid = do
  assertTextInsertInvalid "abc" 4 '_'
  assertTextInsertInvalid "" 1 '_'

assertTextDelete :: String -> Word32 -> String -> Assertion
assertTextDelete initial p expected = assertEqual "Expected provided text" (Just $ N.TextNode expected) $ O.applyPath (O.DirectedOperation "" $ O.TextDelete p) $ N.TextNode initial

testTextDelete :: Assertion
testTextDelete = do
  assertTextDelete "abc" 0 "bc"
  assertTextDelete "abc" 1 "ac"
  assertTextDelete "abc" 2 "ab"
  assertTextDelete "a" 0 ""

assertTextDeleteInvalid :: String -> Word32 -> Assertion
assertTextDeleteInvalid initial p = assertNothing "Expected Nothing" $ O.applyPath (O.DirectedOperation "" $ O.TextDelete p) $ N.TextNode initial

testTextDeleteInvalid :: Assertion
testTextDeleteInvalid = do
  assertTextDeleteInvalid "abc" 3
  assertTextDeleteInvalid "" 0

assertSequenceSet :: [N.Node] -> Word32 -> N.Node -> [N.Node] -> Assertion
assertSequenceSet initial pos child expected = assertEqual "Expected given list" (Just $ N.SequenceNode $ S.fromList expected) $ O.applyPath (O.DirectedOperation "" $ O.SequenceSet pos child 1) $ N.SequenceNode $ S.fromList initial

testSequenceSet :: Assertion
testSequenceSet = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  let x = N.TextNode "hello"
  assertSequenceSet [a, b, c] 0 x [x, b, c]
  assertSequenceSet [a, b, c] 1 x [a, x, c]
  assertSequenceSet [a, b, c] 2 x [a, b, x]
  assertSequenceSet [a] 0 x [x]

assertSequenceSetInvalid :: [N.Node] -> Word32 -> N.Node -> Assertion
assertSequenceSetInvalid initial pos child = assertNothing "Expected Nothing" $ O.applyPath (O.DirectedOperation "" $ O.SequenceSet pos child 1) $ N.SequenceNode $ S.fromList initial

testSequenceSetInvalid :: Assertion
testSequenceSetInvalid = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  let x = N.TextNode "hello"
  assertSequenceSetInvalid [a, b, c] 3 x
  assertSequenceSetInvalid [] 0 x

assertSequenceInsert :: [N.Node] -> Word32 -> N.Node -> [N.Node] -> Assertion
assertSequenceInsert initial pos child expected = assertEqual "Expected given list" (Just $ N.SequenceNode $ S.fromList expected) $ O.applyPath (O.DirectedOperation "" $ O.SequenceInsert pos child 1) $ N.SequenceNode $ S.fromList initial

testSequenceInsert :: Assertion
testSequenceInsert = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  let x = N.TextNode "hello"
  assertSequenceInsert [a, b, c] 0 x [x, a, b, c]
  assertSequenceInsert [a, b, c] 1 x [a, x, b, c]
  assertSequenceInsert [a, b, c] 2 x [a, b, x, c]
  assertSequenceInsert [a, b, c] 3 x [a, b, c, x]
  assertSequenceInsert [] 0 x [x]

assertSequenceInsertInvalid :: [N.Node] -> Word32 -> N.Node -> Assertion
assertSequenceInsertInvalid initial pos child = assertNothing "Expected Nothing" $ O.applyPath (O.DirectedOperation "" $ O.SequenceInsert pos child 1) $ N.SequenceNode $ S.fromList initial

testSequenceInsertInvalid :: Assertion
testSequenceInsertInvalid = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  let x = N.TextNode "hello"
  assertSequenceInsertInvalid [a, b, c] 4 x
  assertSequenceInsertInvalid [a] 2 x
  assertSequenceInsertInvalid [] 1 x

assertSequenceDelete :: [N.Node] -> Word32 -> [N.Node] -> Assertion
assertSequenceDelete initial pos expected = assertEqual "Expected given list" (Just $ N.SequenceNode $ S.fromList expected) $ O.applyPath (O.DirectedOperation "" $ O.SequenceDelete pos) $ N.SequenceNode $ S.fromList initial

testSequenceDelete :: Assertion
testSequenceDelete = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  assertSequenceDelete [a, b, c] 0 [b, c]
  assertSequenceDelete [a, b, c] 1 [a, c]
  assertSequenceDelete [a, b, c] 2 [a, b]
  assertSequenceDelete [a] 0 []

assertSequenceDeleteInvalid :: [N.Node] -> Word32 -> Assertion
assertSequenceDeleteInvalid initial pos = assertNothing "Expected Nothing" $ O.applyPath (O.DirectedOperation "" $ O.SequenceDelete pos) $ N.SequenceNode $ S.fromList initial

testSequenceDeleteInvalid :: Assertion
testSequenceDeleteInvalid = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  assertSequenceDeleteInvalid [a, b, c] 3
  assertSequenceDeleteInvalid [a] 1
  assertSequenceDeleteInvalid [] 0

assertMapSet :: [(Text, N.Node)] -> Text -> N.Node -> [(Text, N.Node)] -> Assertion
assertMapSet initial key child expected = assertEqual "Expected provided map" (Just $ N.MapNode $ M.fromList expected) $ O.applyPath (O.DirectedOperation "" $ O.MapSet key child 1) $ N.MapNode $ M.fromList initial

testMapSet :: Assertion
testMapSet = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  let x = N.TextNode "hello"
  assertMapSet [("foo", a), ("bar", b), ("baz", c)] "qux" x [("foo", a), ("bar", b), ("baz", c), ("qux", x)]
  assertMapSet [("foo", a), ("bar", b), ("baz", c)] "foo" x [("foo", x), ("bar", b), ("baz", c)]
  assertMapSet [] "foo" x [("foo", x)]

assertMapUnset :: [(Text, N.Node)] -> Text -> [(Text, N.Node)] -> Assertion
assertMapUnset initial key expected = assertEqual "Expected provided map" (Just $ N.MapNode $ M.fromList expected) $ O.applyPath (O.DirectedOperation "" $ O.MapUnset key) $ N.MapNode $ M.fromList initial

testMapUnset :: Assertion
testMapUnset = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  assertMapUnset [("foo", a), ("bar", b), ("baz", c)] "foo" [("bar", b), ("baz", c)]
  assertMapUnset [("foo", a), ("bar", b), ("baz", c)] "bar" [("foo", a), ("baz", c)]
  assertMapUnset [("foo", a), ("bar", b), ("baz", c)] "baz" [("foo", a), ("bar", b)]
  assertMapUnset [("foo", a)] "foo" []

assertMapUnsetInvalid :: [(Text, N.Node)] -> Text -> Assertion
assertMapUnsetInvalid initial key = assertNothing "Expected nothing" $ O.applyPath (O.DirectedOperation "" $ O.MapUnset key) $ N.MapNode $ M.fromList initial

testMapUnsetInvalid :: Assertion
testMapUnsetInvalid = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  assertMapUnsetInvalid [("foo", a), ("bar", b), ("baz", c)] "qux"
  assertMapUnsetInvalid [("foo", a)] "qux"
  assertMapUnsetInvalid [] "qux"

assertNestedOp :: N.Node -> P.Path -> O.Operation -> N.Node -> Assertion
assertNestedOp initial path op expected = assertEqual "Expected provided node" (Just expected) $ O.applyPath (O.DirectedOperation path op) $ initial

testNestedOps :: Assertion
testNestedOps = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  let s = N.SequenceNode $ S.fromList [a, b, c]
  let m = N.MapNode $ M.fromList [("foo", s)]
  let s2 = N.SequenceNode $ S.fromList [m]
  assertNestedOp s "0" (O.IntInc 3) (N.SequenceNode $ S.fromList [N.IntNode 126, b, c])
  assertNestedOp m "foo/0" (O.IntInc 3) (N.MapNode $ M.fromList [("foo", N.SequenceNode $ S.fromList [N.IntNode 126, b, c])])
  assertNestedOp s2 "0/foo/0" (O.IntInc 3) (N.SequenceNode $ S.fromList [N.MapNode $ M.fromList [("foo", N.SequenceNode $ S.fromList [N.IntNode 126, b, c])]])

assertNestedOpInvalid :: N.Node -> P.Path -> O.Operation -> Assertion
assertNestedOpInvalid initial path op = assertNothing "Expected Nothing" $ O.applyPath (O.DirectedOperation path op) $ initial

testNestedOpsInvalid :: Assertion
testNestedOpsInvalid = do
  let a = N.IntNode 123
  let b = N.BoolNode True
  let c = N.DoubleNode 123.456
  let s = N.SequenceNode $ S.fromList [a, b, c]
  let m = N.MapNode $ M.fromList [("foo", s)]
  let s2 = N.SequenceNode $ S.fromList [m]
  assertNestedOpInvalid s "3" O.NoOp
  assertNestedOpInvalid s "foo" O.NoOp
  assertNestedOpInvalid s "0" (O.TextDelete 3)
  assertNestedOpInvalid m "bar" O.NoOp
  assertNestedOpInvalid m "foo" (O.IntInc 3)
