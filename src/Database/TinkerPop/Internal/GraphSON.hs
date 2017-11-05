-- |
-- Module: Database.TinkerPop.Internal.GraphSON
-- Copyright: (c) 2017 The gremlin-haskell Authors
-- Description: Internal helper for encoding/decoding GraphSON
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Database.TinkerPop.Internal.GraphSON
       ( GraphSON(..)
       ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), Value(..), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)

-- | Wrapper for the typed JSON object introduced in GraphSON 2. If
-- '_atType' is 'Just', it is a typed JSON object with the given type
-- text. If '_atType' is 'Nothing', the bare value of type @a@ is
-- encoded to/decoded from JSON.
data GraphSON a = GraphSON {
  _atType :: Maybe Text
  , _atValue :: a
  } deriving (Show,Eq,Ord)

instance ToJSON a => ToJSON (GraphSON a) where
  toJSON gson = case _atType gson of
    Nothing -> toJSON $ _atValue gson
    Just t -> object [ "@type" .= t,
                       "@value" .= (_atValue gson)
                     ]

instance FromJSON a => FromJSON (GraphSON a) where
  parseJSON v@(Object o) = do
    if length o /= 2
      then parseDirect v
      else do
      mtype <- o .:? "@type"
      mvalue <- o .:? "@value"
      maybe (parseDirect v) return $ typedGraphSON <$> mtype <*> mvalue
  parseJSON v = parseDirect v
    
typedGraphSON :: Text -> a -> GraphSON a
typedGraphSON t v = GraphSON (Just t) v

parseDirect :: FromJSON a => Value -> Parser (GraphSON a)
parseDirect v = GraphSON Nothing <$> parseJSON v
