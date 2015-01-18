module HotDB.Core.JsonUtils (
  Int64(..)
, discriminator
, Discriminated(..)
, Map(..)
) where

import Control.Applicative
import Text.Read (readMaybe)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.Int as I
import qualified Data.Map as M
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson.Types (Parser)
import Data.Traversable

newtype Int64 = Int64 { getInt64 :: I.Int64 } deriving (Show, Eq)

instance A.ToJSON Int64 where
  toJSON (Int64 v) | v >= (-(2^53)) && v <= 2^53 = A.toJSON v
                    | otherwise = A.toJSON $ show v

instance A.FromJSON Int64 where
  parseJSON (A.Number v) = case S.toBoundedInteger v of
                            Just n -> return $ Int64 n
                            otherwise -> fail $ "not an Int64"
  parseJSON (A.String v) = case readMaybe (T.unpack v) of
                            Just n -> return $ Int64 n
                            otherwise -> fail $ "not an Int64"
  parseJSON _ = fail "not an Int64"

newtype Map k v = Map { getMap :: M.Map k v } deriving (Show, Eq)
instance (A.ToJSON k, A.ToJSON v) => A.ToJSON (Map k v) where
  toJSON (Map m) = A.toJSON $ M.toAscList $ M.map A.toJSON m

instance (Ord k, A.FromJSON k, A.FromJSON v) => A.FromJSON (Map k v) where
  parseJSON (A.Array v) = Map . M.fromList . V.toList <$> traverse A.parseJSON v

data Discriminated a = Discriminated String a

instance (A.ToJSON a) => A.ToJSON (Discriminated a) where
  toJSON (Discriminated k v) = A.toJSON $ M.fromList [(k, A.toJSON v)]

discriminator :: A.FromJSON a => String -> A.Object -> Parser a
discriminator k h = case H.lookup (T.pack k) h of
                       Just v -> A.parseJSON v
                       Nothing -> fail ""
