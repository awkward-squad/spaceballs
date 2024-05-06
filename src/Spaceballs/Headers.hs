module Spaceballs.Headers
  ( -- * Headers
    Headers (..),
    emptyHeaders,
    getHeader,
    foldMapHeaders,
    foldlHeaders,
    headersToWaiHeaders,

    -- * Headers builder
    HeadersBuilder,
    emptyHeadersBuilder,
    addHeader,
    addWaiHeaders,
    buildHeaders,

    -- * Header
    Header (..),
  )
where

import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Coerce (coerce)
import Data.Foldable.WithIndex (ifor_)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.Array (Array, createArray, emptyArray, indexArray, sizeofArray, writeArray)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

-- | HTTP headers.
newtype Headers
  = Headers (Array Header)
  deriving stock (Eq)

emptyHeaders :: Headers
emptyHeaders =
  Headers emptyArray

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

-- | Fold headers (left associated).
foldMapHeaders :: (Monoid m) => (Text -> Text -> m) -> Headers -> m
foldMapHeaders f =
  foldlHeaders (\acc name value -> acc <> f name value) mempty

-- | Left-fold headers.
foldlHeaders :: (a -> Text -> Text -> a) -> a -> Headers -> a
foldlHeaders f z (Headers headers) =
  go z 0
  where
    go !acc !i
      | i == len = acc
      | otherwise = go (f acc name value) (i + 1)
      where
        Header name value = indexArray headers i

    len :: Int
    len =
      sizeofArray headers

headersToWaiHeaders :: Headers -> [(CaseInsensitive.CI ByteString, ByteString)]
headersToWaiHeaders =
  foldlHeaders step []
  where
    step acc name value =
      (name', value') : acc
      where
        !name' = CaseInsensitive.mk (Text.encodeUtf8 name)
        !value' = Text.encodeUtf8 value

------------------------------------------------------------------------------------------------------------------------
-- Headers builder

newtype HeadersBuilder
  = HeadersBuilder (Map Text Text)

instance Monoid HeadersBuilder where
  mempty =
    emptyHeadersBuilder

instance Semigroup HeadersBuilder where
  HeadersBuilder xs <> HeadersBuilder ys =
    HeadersBuilder (Map.unionWith (\x y -> x <> ", " <> y) xs ys)

emptyHeadersBuilder :: HeadersBuilder
emptyHeadersBuilder =
  HeadersBuilder Map.empty

addHeader :: Text -> Text -> HeadersBuilder -> HeadersBuilder
addHeader =
  coerce addHeader_

addHeader_ :: Text -> Text -> Map Text Text -> Map Text Text
addHeader_ name value =
  Map.alter f name
  where
    f = \case
      Nothing -> Just value
      Just value1 -> Just (value1 <> ", " <> value)

addWaiHeaders :: [(CaseInsensitive.CI ByteString, ByteString)] -> HeadersBuilder
addWaiHeaders =
  List.foldl' step emptyHeadersBuilder
  where
    step :: HeadersBuilder -> (CaseInsensitive.CI ByteString, ByteString) -> HeadersBuilder
    step (HeadersBuilder acc) (name, value0) =
      HeadersBuilder (Map.alter f (Text.decodeASCII (CaseInsensitive.foldedCase name)) acc)
      where
        f = \case
          Nothing -> Just value1
          Just value2 -> Just (value2 <> ", " <> value1)
        value1 = Text.decodeUtf8 value0

buildHeaders :: HeadersBuilder -> Headers
buildHeaders =
  coerce buildHeaders_

buildHeaders_ :: Map Text Text -> Array Header
buildHeaders_ headers =
  createArray (Map.size headers) undefined \array -> do
    ifor_ (Map.toList headers) \i (name, value) ->
      writeArray array i (Header name value)

------------------------------------------------------------------------------------------------------------------------
-- Header

-- | An HTTP header.
--
-- HTTP header names are case-insensitive. However, for performance and simplicity, we always represent header names as
-- lowercase.
data Header
  = Header
      {-# UNPACK #-} !Text
      {-# UNPACK #-} !Text
  deriving stock (Eq)
