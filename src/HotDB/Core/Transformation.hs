module HotDB.Core.Transformation (
  transform
, transformDirected
, Side(..)
) where

import Prelude hiding (Left, Right)
import Control.Monad
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Text as T
import Text.Read (readMaybe)

import HotDB.Core.Operation
import qualified HotDB.Core.Path as P

data Side = Left | Right deriving (Eq, Ord)

type Tiebreaker = Int64

isLeft :: Side -> Bool
isLeft Left = True
isLeft _ = False

isRight :: Side -> Bool
isRight s = not $ isLeft s

winsTiebreaker :: Tiebreaker -> Tiebreaker -> Side -> Bool
winsTiebreaker tb1 tb2 side = (tb1, side) > (tb2, oppositeSide side)
  where oppositeSide Left = Right
        oppositeSide Right = Left

losesTiebreaker :: Tiebreaker -> Tiebreaker -> Side -> Bool
losesTiebreaker tb1 tb2 side = not $ winsTiebreaker tb1 tb2 side

ifCanInc :: Position -> Maybe a -> Maybe a
ifCanInc p m = if (p + 1) == 0
                 then Nothing -- Increment would cause overflow
                 else m

transform :: Operation -> Operation -> Side -> Maybe Operation
transform (RootSet v tb) (RootSet av atb) side
    | tb < atb || (losesTiebreaker tb atb side) || (v == av) = Just NoOp
    | otherwise = Just $ RootSet v tb

transform (IntInc v) (IntInc _) _ = Just $ IntInc v

transform (TextInsert p c tb) (TextInsert ap _ atb) side
    | p < ap || (p == ap && (winsTiebreaker tb atb side)) = Just $ TextInsert p c tb
    | otherwise = ifCanInc p $ Just $ TextInsert (p + 1) c tb

transform (TextInsert p c tb) (TextDelete ap) _
    | ap < p = Just $ TextInsert (p - 1) c tb
    | otherwise = Just $ TextInsert p c tb

transform (TextDelete p) (TextInsert ap _ _) _
    | ap <= p = ifCanInc p $ Just $ TextDelete (p + 1)
    | otherwise = Just $ TextDelete p

transform (TextDelete p) (TextDelete ap) _
    | p == ap = Just NoOp
    | ap < p = Just $ TextDelete (p - 1)
    | otherwise = Just $ TextDelete p

transform (SequenceSet p v tb) (SequenceSet ap av atb) side
    | p == ap && ((losesTiebreaker tb atb side) || (v == av)) = Just NoOp
    | otherwise = Just $ SequenceSet p v tb

transform (SequenceSet p v tb) (SequenceInsert ap _ _) _
    | ap <= p = ifCanInc p $ Just $ SequenceSet (p + 1) v tb
    | otherwise = Just $ SequenceSet p v tb

transform (SequenceSet p v tb) (SequenceDelete ap) _
    | ap == p = Just NoOp
    | ap < p = Just $ SequenceSet (p - 1) v tb
    | otherwise = Just $ SequenceSet p v tb

transform (SequenceInsert p v tb) (SequenceSet _ _ _) _ = Just $ SequenceInsert p v tb

transform (SequenceInsert p v tb) (SequenceInsert ap av atb) side 
    | p < ap || ((p == ap) && (winsTiebreaker tb atb side)) = Just $ SequenceInsert p v tb
    | otherwise = ifCanInc p $ Just $ SequenceInsert (p + 1) v tb

transform (SequenceInsert p v tb) (SequenceDelete ap) _
    | ap < p = Just $ SequenceInsert (p - 1) v tb
    | otherwise = Just $ SequenceInsert p v tb

transform (SequenceDelete p) (SequenceSet _ _ _) _ = Just $ SequenceDelete p

transform (SequenceDelete p) (SequenceInsert ap _ _) _
    | ap <= p = ifCanInc p $ Just $ SequenceDelete (p + 1)
    | otherwise = Just $ SequenceDelete p

transform (SequenceDelete p) (SequenceDelete ap) _
    | p == ap = Just NoOp
    | ap < p = Just $ SequenceDelete (p - 1)
    | otherwise = Just $ SequenceDelete p

transform (MapSet k v tb) (MapSet ak av atb) side
    | k == ak && ((losesTiebreaker tb atb side) || (v == av)) = Just NoOp
    | otherwise = Just $ MapSet k v tb

transform (MapSet k v tb) (MapUnset _) _ = Just $ MapSet k v tb

transform (MapUnset k) (MapSet ak _ _) _
    | k == ak = Just NoOp
    | otherwise = Just $ MapUnset k

transform (MapUnset k) (MapUnset ak) _
    | k == ak = Just NoOp
    | otherwise = Just $ MapUnset k

transform op NoOp _ = Just op

transform _ _ _ = Nothing -- Incompatible transformation

transformDirected :: DirectedOperation -> DirectedOperation -> Side -> Maybe DirectedOperation
transformDirected dop@(DirectedOperation p op) (DirectedOperation ap aop) side =
  case P.removePrefix ap p of
    Nothing -> Just dop
    Just p' -> if P.isEmpty p'
                 then do
                   op' <- transform op aop side
                   return $ DirectedOperation p op'
                 else do
                   DirectedOperation newRelPath op <- transformChild (DirectedOperation p' op) aop
                   return $ DirectedOperation (newRelPath `P.relativeTo` ap) op

transformChild :: DirectedOperation -> Operation -> Maybe DirectedOperation
transformChild _ (RootSet _ _) = Just directedNoOp

transformChild dop@(DirectedOperation p _) (SequenceSet i _ _) = do
  t <- P.head p
  pi <- readMaybe $ T.unpack t
  if pi == i
    then Just directedNoOp
    else Just dop

transformChild dop@(DirectedOperation p op) (SequenceInsert i _ _) = do
  t <- P.head p
  pi <- readMaybe $ T.unpack t
  if i <= pi
    then ifCanInc pi $ do
      p' <- P.replaceHead p (T.pack $ show $ pi + 1)
      Just $ DirectedOperation p' op
    else Just dop

transformChild dop@(DirectedOperation p op) (SequenceDelete i) = do
  t <- P.head p
  pi <- readMaybe $ T.unpack t
  if i == pi
    then Just directedNoOp
    else if i < pi
      then do
        p' <- P.replaceHead p (T.pack $ show $ pi - 1)
        Just $ DirectedOperation p' op
      else Just dop

transformChild dop@(DirectedOperation p _) (MapSet k _ _) = do
  t <- P.head p
  if t == k
    then Just directedNoOp
    else Just dop

transformChild dop@(DirectedOperation p _) (MapUnset k) = do
  t <- P.head p
  if t == k
    then Just directedNoOp
    else Just dop

transformChild _ _ = Nothing
