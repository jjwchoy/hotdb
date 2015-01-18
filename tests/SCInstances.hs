{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SCInstances () where

import Control.Applicative
import Control.Monad

import Data.Bits (shift, xor)
import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Word (Word32)

import qualified Test.SmallCheck.Series as SCS

import qualified HotDB.Core.Node as N
import qualified HotDB.Core.Operation as O

instance Monad m => SCS.Serial m Int64 where
  series = mkInt64 <$> SCS.series
         where mkInt64 (msi, lsi) = (shift (fromInteger msi) 32) `xor` fromInteger lsi

instance Monad m => SCS.Serial m Word32 where
  series = (SCS.generate $ \d -> if d >= 0 then pure 0 else empty) <|> nats
    where nats = SCS.generate $ \d -> let ud = fromIntegral d in [1..ud]

instance Monad m => SCS.Serial m T.Text where
  series = T.pack <$> SCS.series

instance Monad m => SCS.Serial m (Seq.Seq N.Node) where
  series = Seq.fromList <$> SCS.series

instance Monad m => SCS.Serial m (M.Map T.Text N.Node) where
  series = M.fromList <$> SCS.series

instance Monad m => SCS.Serial m N.Node where
  series = SCS.decDepth $
             SCS.cons0 N.EmptyNode SCS.\/
             SCS.cons1 N.RootNode SCS.\/
             SCS.cons1 N.BoolNode SCS.\/
             SCS.cons1 N.IntNode SCS.\/
             SCS.cons1 N.DoubleNode SCS.\/
             SCS.cons1 N.TextNode SCS.\/
             SCS.cons1 N.MapNode

instance Monad m => SCS.Serial m O.Operation where
  series = SCS.decDepth $
             SCS.cons0 O.NoOp SCS.\/
             SCS.cons2 O.RootSet SCS.\/
             SCS.cons1 O.IntInc SCS.\/
             SCS.cons3 O.TextInsert SCS.\/
             SCS.cons1 O.TextDelete SCS.\/
             SCS.cons3 O.SequenceSet SCS.\/
             SCS.cons3 O.SequenceInsert SCS.\/
             SCS.cons1 O.SequenceDelete SCS.\/
             SCS.cons3 O.MapSet SCS.\/
             SCS.cons1 O.MapUnset
