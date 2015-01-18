{-# LANGUAGE OverloadedStrings #-}
module HotDB.Core.Path (
  Path
, getPath
, toText
, fromText
, length
, isEmpty
, removePrefix
, relativeTo
, head
, replaceHead
, adjustHead
) where

import Prelude hiding (head, length)
import qualified Prelude

import Control.Applicative
import Control.Monad (mzero)

import Data.String (IsString(..))
import qualified Data.Text as T

import qualified Data.Aeson as A

newtype Path = Path { getPath :: [ T.Text ] } deriving (Show, Eq)

toText :: Path -> T.Text
toText (Path ("/":ps)) = T.concat ["/", toText (Path ps)]
toText (Path ps) = T.intercalate "/" ps

fromText :: T.Text -> Maybe Path
fromText "" = Just $ Path []
fromText t =
  case T.head t of
    '/' -> do
      p <- pathFromText' $ T.tail t
      return $ Path ("/":p)
    _ -> do
      p <- pathFromText' t
      return $ Path p
  where validateComponent t = Just t
        validate ("/":ps) = do
          ps' <- validate ps
          return $ "/" : ps'
        validate ps = sequence $ map validateComponent ps
        pathFromText' t =
          let ps = T.splitOn "/" t in
            validate ps

length :: Path -> Int
length (Path ps) = Prelude.length ps

isEmpty :: Path -> Bool
isEmpty (Path []) = True
isEmpty _ = False

removePrefix :: Path -> Path -> Maybe Path
removePrefix (Path []) (Path ps) = Just $ Path ps
removePrefix _ (Path []) = Nothing
removePrefix (Path (p:ps)) (Path (c:cs)) =
  if p == c
    then removePrefix (Path ps) (Path cs)
    else Nothing

relativeTo :: Path -> Path -> Path
relativeTo p@(Path ("/":_)) _ = p
relativeTo (Path relPath) (Path basePath) = Path $ basePath ++ relPath

head :: Path -> Maybe T.Text
head (Path []) = Nothing
head (Path (p:_)) = Just p

replaceHead :: Path -> T.Text -> Maybe Path
replaceHead (Path []) _ = Nothing
replaceHead (Path (_:ps)) p = Just $ Path (p:ps) -- TODO validate path component

adjustHead :: Path -> (T.Text -> Maybe T.Text) -> Maybe Path
adjustHead (Path []) f = Nothing
adjustHead (Path (p:ps)) f = do
  p' <- f p
  return $ Path (p':ps)

instance A.ToJSON Path where
  toJSON p = A.toJSON $ toText p

instance A.FromJSON Path where
  parseJSON j = do
    v <- A.parseJSON j
    case v of
      Nothing -> mzero
      Just p -> return p
  --parseJSON j = (\v -> maybe mzero return (fromText v)) <*> A.parseJSON j

instance IsString Path where
  fromString s = maybe (error "Invalid path") id $ fromText $ T.pack s
