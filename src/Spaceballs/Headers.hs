module Spaceballs.Headers
  ( -- * Headers
    Headers (..),
    emptyHeaders,
    getHeader,
    headersToList,
    headersToMap,
    foldMapHeaders,
    foldlHeaders,
    foldrHeaders,
    headersToWaiHeaders,

    -- * Headers builder
    HeadersBuilder,
    header,
    headers,
    waiHeaders,

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
getHeader name (Headers hdrs) =
  go 0
  where
    go :: Int -> Maybe Text
    go i
      | i == len = Nothing
      | name == name1 = Just value
      | otherwise = go (i + 1)
      where
        Header name1 value = indexArray hdrs i

    len :: Int
    len =
      sizeofArray hdrs

-- | Convert headers to a list.
headersToList :: Headers -> [(Text, Text)]
headersToList =
  foldrHeaders (\key value -> ((key, value) :)) []

-- | Convert headers to a map.
headersToMap :: Headers -> Map Text Text
headersToMap =
  foldlHeaders (\acc key value -> Map.insert key value acc) Map.empty

-- | Fold headers (left associated).
foldMapHeaders :: (Monoid m) => (Text -> Text -> m) -> Headers -> m
foldMapHeaders f =
  foldlHeaders (\acc name value -> acc <> f name value) mempty

-- | Left-fold headers.
foldlHeaders :: (a -> Text -> Text -> a) -> a -> Headers -> a
foldlHeaders f z (Headers hdrs) =
  go z 0
  where
    go !acc !i
      | i == len = acc
      | otherwise = go (f acc name value) (i + 1)
      where
        Header name value = indexArray hdrs i

    len :: Int
    len =
      sizeofArray hdrs

-- | Right-fold headers.
foldrHeaders :: (Text -> Text -> a -> a) -> a -> Headers -> a
foldrHeaders f z (Headers hdrs) =
  go z (len - 1)
  where
    go !acc !i
      | i == -1 = acc
      | otherwise = go (f name value acc) (i - 1)
      where
        Header name value = indexArray hdrs i

    len :: Int
    len =
      sizeofArray hdrs

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
  = HeadersBuilder (Map Text Text -> Map Text Text)

instance Monoid HeadersBuilder where
  mempty =
    HeadersBuilder id

instance Semigroup HeadersBuilder where
  HeadersBuilder f <> HeadersBuilder g =
    HeadersBuilder (g . f)

header :: Text -> Text -> HeadersBuilder
header =
  coerce header_

header_ :: Text -> Text -> Map Text Text -> Map Text Text
header_ name value =
  Map.alter f name
  where
    f = \case
      Nothing -> Just value
      Just value1 -> Just (value1 <> ", " <> value)

waiHeaders :: [(CaseInsensitive.CI ByteString, ByteString)] -> HeadersBuilder
waiHeaders =
  List.foldl' step mempty
  where
    step :: HeadersBuilder -> (CaseInsensitive.CI ByteString, ByteString) -> HeadersBuilder
    step acc (name, value) =
      acc <> header (Text.decodeASCII (CaseInsensitive.foldedCase name)) (Text.decodeUtf8 value)

headers :: HeadersBuilder -> Headers
headers =
  coerce headers_

headers_ :: (Map Text Text -> Map Text Text) -> Array Header
headers_ f =
  createArray (Map.size hdrs) undefined \array -> do
    ifor_ (Map.toList hdrs) \i (name, value) ->
      writeArray array i (Header name value)
  where
    hdrs = f Map.empty

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
