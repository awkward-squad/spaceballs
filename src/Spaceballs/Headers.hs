module Spaceballs.Headers
  ( Headers (..),
    Header (..),
    makeHeaders,
    getHeader,
    foldHeaders,
  )
where

import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Foldable.WithIndex (ifor_)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.Array (Array, createArray, indexArray, sizeofArray, writeArray)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

-- | HTTP headers.
newtype Headers
  = Headers (Array Header)
  deriving stock (Eq)

-- | An HTTP header.
--
-- HTTP header names are case-insensitive. However, for performance and simplicity, we always represent header names as
-- lowercase.
data Header
  = Header
      {-# UNPACK #-} !Text
      {-# UNPACK #-} !Text
  deriving stock (Eq)

makeHeaders :: [(CaseInsensitive.CI ByteString, ByteString)] -> Headers
makeHeaders =
  headersMapToHeaders . waiHeadersToHeadersMap

waiHeadersToHeadersMap :: [(CaseInsensitive.CI ByteString, ByteString)] -> Map Text Text
waiHeadersToHeadersMap =
  List.foldl' step Map.empty
  where
    step :: Map Text Text -> (CaseInsensitive.CI ByteString, ByteString) -> Map Text Text
    step acc (name, value0) =
      Map.alter f (Text.decodeASCII (CaseInsensitive.foldedCase name)) acc
      where
        f = \case
          Nothing -> Just value1
          Just value2 -> Just (value2 <> ", " <> value1)
        value1 = Text.decodeUtf8 value0

headersMapToHeaders :: Map Text Text -> Headers
headersMapToHeaders m =
  Headers do
    createArray (Map.size m) undefined \array -> do
      ifor_ (Map.toList m) \i (name, value) ->
        writeArray array i (Header name value)

-- | Get a header by (lowercase) name.
--
-- > >>> getHeader "content-type" headers
-- > Just "image/png"
getHeader :: Text -> Headers -> Maybe Text
getHeader name (Headers headers) =
  go 0
  where
    go :: Int -> Maybe Text
    go i
      | i == len = Nothing
      | name == name1 = Just value
      | otherwise = go (i + 1)
      where
        Header name1 value = indexArray headers i

    len :: Int
    len =
      sizeofArray headers

-- | Fold headers (strict, left-associated).
foldHeaders :: (Monoid m) => (Text -> Text -> m) -> Headers -> m
foldHeaders f (Headers headers) =
  go mempty 0
  where
    go !acc !i
      | i == len = acc
      | otherwise = go (acc <> f name value) (i + 1)
      where
        Header name value = indexArray headers i

    len :: Int
    len =
      sizeofArray headers
