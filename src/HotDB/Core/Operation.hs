{-# LANGUAGE OverloadedStrings #-}
module HotDB.Core.Operation (
  Operation(..)
, DirectedOperation(..)
, directedNoOp
, Position
, apply
, applyPath
) where

import Data.Sequence ((<|), (><))
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Int (Int64)
import Data.Word (Word32)
import Control.Applicative
import Control.Monad

import HotDB.Core.Node
import qualified HotDB.Core.JsonUtils as J
import qualified HotDB.Core.Path as P

type Tiebreaker = Int64
type Position = Word32

data Operation = NoOp
    | RootSet Node Tiebreaker
    | IntInc Int64
    | TextInsert Position Char Tiebreaker
    | TextDelete Position
    | SequenceSet Position Node Tiebreaker
    | SequenceInsert Position Node Tiebreaker
    | SequenceDelete Position
    | MapSet T.Text Node Tiebreaker
    | MapUnset T.Text
    deriving (Eq, Show)

data DirectedOperation = DirectedOperation P.Path Operation deriving (Show, Eq)

directedNoOp :: DirectedOperation
directedNoOp = DirectedOperation "" NoOp

apply :: Operation -> Node -> Maybe Node

apply (RootSet v _) (RootNode _) = Just (RootNode v)

apply (IntInc d) (IntNode v) = Just (IntNode (v + d))

apply (TextInsert p c _) (TextNode t)
    | p' <= length t = Just $ TextNode $ (take p' t) ++ [c] ++ (drop p' t)
    | otherwise = Nothing
    where p' = fromIntegral p :: Int

apply (TextDelete p) (TextNode t)
    | p' < length t = Just $ TextNode $ (take p' t) ++ (drop (p' + 1) t)
    | otherwise = Nothing
    where p' = fromIntegral p :: Int

apply (SequenceSet i v _) (SequenceNode s)
    | i' < Seq.length s = Just  $ SequenceNode $ Seq.update i' v s
    | otherwise = Nothing
    where i' = fromIntegral i :: Int

apply (SequenceInsert i v _) (SequenceNode s)
    | i' <= Seq.length s = Just $ SequenceNode $ Seq.take i' s >< (v <| Seq.drop i' s)
    | otherwise = Nothing
    where i' = fromIntegral i :: Int

apply (SequenceDelete i) (SequenceNode s)
    | i' < Seq.length s = Just $ SequenceNode $ Seq.take i' s >< Seq.drop (i' + 1) s
    | otherwise = Nothing
    where i' = fromIntegral i :: Int

apply (MapSet k v _) (MapNode m)
    = Just (MapNode (Map.insert k v m))

apply (MapUnset k) (MapNode m)
    | Map.member k m = Just (MapNode (Map.delete k m))
    | otherwise = Nothing

apply NoOp n = Just n
apply _ _ = Nothing

applyPath :: DirectedOperation -> Node -> Maybe Node
applyPath (DirectedOperation p op) node = adjustPath node p $ apply op

instance A.ToJSON Operation where
  toJSON NoOp = A.toJSON A.Null
  toJSON (RootSet v t) = A.toJSON $ J.Discriminated "rs" (v, J.Int64 t)
  toJSON (IntInc d) = A.toJSON $ J.Discriminated "i+" (J.Int64 d)
  toJSON (TextInsert i c t) = A.toJSON $ J.Discriminated "ti" (i, c, J.Int64 t)
  toJSON (TextDelete i) = A.toJSON $ J.Discriminated "td" i
  toJSON (SequenceSet i v t) = A.toJSON $ J.Discriminated "ss" (i, v, J.Int64 t)
  toJSON (SequenceInsert i v t) = A.toJSON $ J.Discriminated "si" (i, v, J.Int64 t)
  toJSON (SequenceDelete i) = A.toJSON $ J.Discriminated "sd" i
  toJSON (MapSet k v t) = A.toJSON $ J.Discriminated "ms" (k, v, J.Int64 t)
  toJSON (MapUnset k) = A.toJSON $ J.Discriminated "mu" k

instance A.FromJSON Operation where
  parseJSON A.Null = pure NoOp
  parseJSON (A.Object o) =
      (\(v, t) -> RootSet v $ J.getInt64 t) <$> J.discriminator "rs" o <|>
      IntInc . J.getInt64 <$> J.discriminator "i+" o <|>
      (\(i, c, t) -> TextInsert i c $ J.getInt64 t) <$> J.discriminator "ti" o <|>
      TextDelete <$> J.discriminator "td" o <|>
      (\(i, v, t) -> SequenceSet i v $ J.getInt64 t) <$> J.discriminator "ss" o <|>
      (\(i, v, t) -> SequenceInsert i v $ J.getInt64 t) <$> J.discriminator "si" o <|>
      SequenceDelete <$> J.discriminator "sd" o <|>
      (\(k, v, t) -> MapSet k v $ J.getInt64 t) <$> J.discriminator "ms" o <|>
      MapUnset <$> J.discriminator "mu" o
  parseJSON _ = mzero

instance A.ToJSON DirectedOperation where
  toJSON (DirectedOperation p op) = A.toJSON (p, op)

instance A.FromJSON DirectedOperation where
  parseJSON (A.Array v) | V.length v == 2 = DirectedOperation <$> (A.parseJSON $ V.head v) <*> (A.parseJSON $ V.last v)
                        | otherwise = fail "Invalid DirectedOperation"
  parseJSON _ = fail "Invalid DirectedOperation"
