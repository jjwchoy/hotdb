{-# LANGUAGE OverloadedStrings #-}
module HotDB.Core.Commit (
  Commit(..)
, TaggedCommit
, UserId
, Timestamp
, CommitId
, emptyCommitId
) where

import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V

import qualified Data.Aeson as A

import qualified Crypto.Hash.SHA1 as SHA1

import qualified HotDB.Core.JsonUtils as J
import HotDB.Core.Operation (DirectedOperation)

type UserId = Text
type Timestamp = Int64
type HexHash = Text
type CommitId = Text

data Commit = Commit {
  directedOperation :: DirectedOperation
, userId :: UserId
, timestamp :: Timestamp
, parent :: CommitId
} deriving (Show, Eq)

type TaggedCommit = (CommitId, Commit)

instance A.ToJSON Commit where
  toJSON (Commit dop uId ts p) = A.toJSON (dop, uId, J.Int64 ts, p)

instance A.FromJSON Commit where
  parseJSON j = (\(dop, uId, ts, p) -> Commit dop uId (J.getInt64 ts) p) <$> A.parseJSON j

emptyCommitId :: Text
emptyCommitId = "da39a3ee5e6b4b0d3255bfef95601890afd80709"

serialize :: Commit -> ByteString
serialize commit = LB.toStrict $ serialize' commit

serialize' :: Commit -> LB.ByteString
serialize' commit = A.encode commit

serializeAndSum :: Commit -> (ByteString, Text)
serializeAndSum commit = (serialized, decodeUtf8 $ Base16.encode $ SHA1.hash serialized)
  where serialized = serialize commit

deserialize :: ByteString -> Maybe Commit
deserialize bytes = deserialize' $ LB.fromChunks [bytes]

deserialize' :: LB.ByteString -> Maybe Commit
deserialize' bytes = A.decode bytes
