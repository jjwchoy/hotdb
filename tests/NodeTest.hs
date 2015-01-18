{-# LANGUAGE OverloadedStrings #-}
module NodeTest (tests) where

import Control.Monad

import qualified Data.Map as M
import qualified Data.Sequence as S

import Test.Tasty
import Test.Tasty.HUnit

import qualified HotDB.Core.Node as N
import Util (assertNothing)

tests :: TestTree
tests = testGroup "Node tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "LookupLeaf" testLookupLeaf
  , testCase "LookupRoot" testLookupRoot
  , testCase "LookupRootInvalid" testLookupRootInvalid
  , testCase "LookupSequence" testLookupSequence
  , testCase "LookupSequenceOutOfRange" testLookupSequenceOutOfRange
  , testCase "LookupSequenceInvalid" testLookupSequenceInvalid
  , testCase "LookupMap" testLookupMap
  , testCase "LookupMapMissing" testLookupMapMissing
  , testCase "LookupNested" testLookupNested
  , testCase "LookupNestedMissing" testLookupNestedMissing
  ]

testLookupLeaf :: Assertion
testLookupLeaf = do
  assertNothing "Expected Nothing" $ N.lookupPath N.EmptyNode "foo"
  assertNothing "Exepected Nothing" $ N.lookupPath (N.BoolNode True) "foo"
  assertNothing "Exepected Nothing" $ N.lookupPath (N.BoolNode False) "foo"
  assertNothing "Exepected Nothing" $ N.lookupPath (N.IntNode 123) "foo"
  assertNothing "Exepected Nothing" $ N.lookupPath (N.DoubleNode 123.456) "foo"
  assertNothing "Exepected Nothing" $ N.lookupPath (N.TextNode "abc") "foo"

c :: N.Node
c = N.IntNode 123

testLookupRoot :: Assertion
testLookupRoot = do
  let rn = N.RootNode c
  assertEqual "Expected (IntNode 123)" (Just c) $ N.lookupPath rn "/"

testLookupRootInvalid :: Assertion
testLookupRootInvalid = do
  let rn = N.RootNode c
  assertNothing "Expected Nothing" $ N.lookupPath rn "foo"

testLookupSequence :: Assertion
testLookupSequence = do
  let l = [N.EmptyNode, c, N.BoolNode True]
  let sn = N.SequenceNode $ S.fromList l
  assertEqual "Expected (IntNode 123)" (Just c) $ N.lookupPath sn "1"

testLookupSequenceOutOfRange :: Assertion
testLookupSequenceOutOfRange = do
  let l = [N.EmptyNode, c, N.BoolNode True]
  let sn = N.SequenceNode $ S.fromList l
  assertNothing "Expected Nothing" $ N.lookupPath sn "3"

testLookupSequenceInvalid :: Assertion
testLookupSequenceInvalid = do
  let l = [N.EmptyNode, c, N.BoolNode True]
  let sn = N.SequenceNode $ S.fromList l
  assertNothing "Expected Nothing" $ N.lookupPath sn "xyz"
  assertNothing "Expected Nothing" $ N.lookupPath sn "0xyz"
  assertNothing "Expected Nothing" $ N.lookupPath sn "xyz0"
  assertNothing "Expected Nothing" $ N.lookupPath sn "0 xyz"
  assertNothing "Expected Nothing" $ N.lookupPath sn "xyz 0"
  assertNothing "Expected Nothing" $ N.lookupPath sn "-1"

testLookupMap :: Assertion
testLookupMap = do
  let l = [("foo", N.EmptyNode), ("bar", c), ("baz", N.BoolNode True)]
  let mn = N.MapNode $ M.fromList l
  assertEqual "Expected (IntNode 123)" (Just c) $ N.lookupPath mn "bar"

testLookupMapMissing :: Assertion
testLookupMapMissing = do
  let mn = N.MapNode M.empty
  assertNothing "Expected Nothing" $ N.lookupPath mn "foo"

-- TODO add nested lookup tests
testLookupNested :: Assertion
testLookupNested = do
  let s1 = N.SequenceNode $ S.fromList [c]
  let m1 = N.MapNode $ M.fromList [("bar", s1)]
  let sn = N.SequenceNode $ S.fromList [m1]
  let mn = N.MapNode $ M.fromList [("foo", sn)]
  let rn = N.RootNode mn
  assertEqual "Expected (IntNode 123)" (Just c) $ N.lookupPath m1 "bar/0"
  assertEqual "Expected (IntNode 123)" (Just c) $ N.lookupPath sn "0/bar/0"
  assertEqual "Expected (IntNode 123)" (Just c) $ N.lookupPath mn "foo/0/bar/0"
  assertEqual "Expected (IntNode 123)" (Just c) $ N.lookupPath rn "/foo/0/bar/0"

testLookupNestedMissing :: Assertion
testLookupNestedMissing = do
  let s1 = N.SequenceNode $ S.fromList [c]
  let m1 = N.MapNode $ M.fromList [("bar", s1)]
  let sn = N.SequenceNode $ S.fromList [m1]
  let mn = N.MapNode $ M.fromList [("foo", sn)]
  let rn = N.RootNode mn
  assertNothing "Expected Nothing" $ N.lookupPath rn "bar"
  assertNothing "Expected Nothing" $ N.lookupPath rn "/bar"
  assertNothing "Expected Nothing" $ N.lookupPath rn "/foo/1"
  assertNothing "Expected Nothing" $ N.lookupPath rn "/foo/0/baz"
  assertNothing "Expected Nothing" $ N.lookupPath rn "/foo/0/bar/1"
  assertNothing "Expected Nothing" $ N.lookupPath rn "/foo/abc/bar/0"
  assertNothing "Expected Nothing" $ N.lookupPath rn "/foo/10/bar/0"
