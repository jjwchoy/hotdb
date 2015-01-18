{-# LANGUAGE OverloadedStrings, TypeFamilies, Rank2Types, DeriveDataTypeable #-}
module HotDB.Core.Node (
  Node(..)
, Document(..)
, lookupPath
, adjustPath
) where

import Prelude hiding (lookup)

import Data.Bits (shift, xor)
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Scientific as S
import qualified Data.Sequence as Seq
import Data.Typeable
import Data.Word (Word32, Word64)
import qualified Data.Vector as V
import Control.Monad (guard, mzero)
import Control.Applicative (pure)
import Text.Read (readMaybe)
import Data.Traversable hiding (sequence)

import Control.Applicative

import qualified HotDB.Core.JsonUtils as J
import qualified HotDB.Core.Path as P

data Node = EmptyNode
    | RootNode Node
    | BoolNode Bool
    | IntNode Int64
    | DoubleNode Double
    | TextNode String
    | SequenceNode (Seq.Seq Node)
    | MapNode (M.Map T.Text Node)
    deriving (Show, Eq, Typeable)

newtype Document = Document (Node, String) deriving (Show, Eq)

instance A.ToJSON Node where
  toJSON EmptyNode = A.toJSON A.Null
  toJSON (RootNode v) = A.toJSON $ J.Discriminated "r" v
  toJSON (BoolNode v) = A.toJSON v
  toJSON (IntNode v) = A.toJSON $ J.Discriminated "i" (J.Int64 v)
  toJSON (DoubleNode v) = A.toJSON v
  toJSON (TextNode v) = A.toJSON v
  toJSON (SequenceNode v) = A.toJSON $ map A.toJSON $ toList v
  toJSON (MapNode v) = A.toJSON $ J.Discriminated "m" (J.Map v)

instance A.FromJSON Node where
  parseJSON A.Null = pure EmptyNode
  parseJSON (A.Bool v) = pure $ BoolNode v
  parseJSON (A.Number v) = pure $ DoubleNode $ S.toRealFloat v
  parseJSON (A.String v) = pure $ TextNode $ T.unpack v
  parseJSON n@(A.Array v) = SequenceNode . vecToSeq <$> traverse A.parseJSON v
  parseJSON (A.Object v) = RootNode <$> J.discriminator "r" v <|>
                           IntNode . J.getInt64 <$> J.discriminator "i" v <|>
                           MapNode . M.fromList <$> J.discriminator "m" v

instance A.ToJSON Document where
  toJSON (Document d) = A.toJSON d

instance A.FromJSON Document where
  parseJSON (A.Array v) = (\r c -> Document (r, c)) <$> A.parseJSON (v V.! 0) <*> A.parseJSON (v V.! 1)

vecToSeq :: V.Vector a -> Seq.Seq a
vecToSeq = Seq.fromList . V.toList

parseInt :: Integral a => T.Text -> Maybe a
parseInt t = case (TR.decimal t :: Either String (Integer, T.Text)) of
  Right (i, "") -> let i' = fromIntegral i in
    if (fromIntegral i') == i
      then Just i'
      else Nothing
  _ -> Nothing

parseIndex :: T.Text -> Maybe Word32
parseIndex = parseInt

lookup :: Node -> T.Text -> Maybe Node
lookup (RootNode n) "/" = Just n
lookup (SequenceNode s) key =
  case mi of
    Just i -> let i' = fromIntegral i in
                if i' < Seq.length (s)
                  then Just $ Seq.index s i'
                  else Nothing
    _ -> Nothing
  where mi = parseIndex key
lookup (MapNode m) key = M.lookup key m
lookup _ _ = Nothing

lookupPath :: Node -> P.Path -> Maybe Node
lookupPath n p =
  lookupPath' n $ P.getPath p
  where lookupPath' n [] = Just n
        lookupPath' n ("":ps) = lookupPath' n ps
        lookupPath' n (p:ps) = do
          c <- lookup n p
          lookupPath' c ps
                        
adjust :: Node -> T.Text -> (Node -> Maybe Node) -> Maybe Node
adjust (RootNode n) "/" f = RootNode <$> f n
adjust sn@(SequenceNode s) key f =
  case mi of
    Just i -> let i' = fromIntegral i in
                if i' < Seq.length (s)
                  then do
                    n' <- f $ Seq.index s i'
                    Just $ SequenceNode $ Seq.update i' n' s
                  else Nothing
    _ -> Nothing
  where mi = parseIndex key
adjust (MapNode m) key f = do
  n <- M.lookup key m
  n' <- f n
  return $ MapNode $ M.insert key n' m
adjust _ _ _ = Nothing

adjustPath :: Node -> P.Path -> (Node -> Maybe Node) -> Maybe Node
adjustPath n p f =
  adjustPath' n (P.getPath p) f
  where adjustPath' n [] f = f n
        adjustPath' n ("":ps) f = adjustPath' n ps f
        adjustPath' n (p:ps) f = adjust n p (\c -> adjustPath' c ps f)
