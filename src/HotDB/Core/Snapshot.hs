{-# LANGUAGE DeriveDataTypeable #-}
module HotDB.Core.Snapshot (
  Snapshot(..)
, initialSnapshot
) where

import Control.Applicative

import Data.Typeable

import qualified Data.Aeson as A

import HotDB.Core.Commit (CommitId, emptyCommitId)
import HotDB.Core.Node (Node(..))

data Snapshot = Snapshot Node CommitId deriving (Typeable, Show, Eq)

instance A.ToJSON Snapshot where
  toJSON (Snapshot n cId) = A.toJSON (n, cId)

instance A.FromJSON Snapshot where
  parseJSON j = (\(n, cId) -> Snapshot n cId) <$> A.parseJSON j

initialSnapshot :: Snapshot
initialSnapshot = Snapshot EmptyNode emptyCommitId
