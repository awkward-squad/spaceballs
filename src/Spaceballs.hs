module Spaceballs
  ( -- * Application
    application,

    -- * Router
    Router,
    Resource,
    route,
    act,

    -- ** Resources
    delete,
    get,
    patch,
    post,
    put,

    -- ** Children
    capture,
    segment,

    -- * Request
    Request (..),
    Params,

    -- ** Query params
    Param,
    ptext,
    param,
    params,

    -- * Response

    -- ** Building
    Response (..),
    response,
    setHeaders,
    setBody,

    -- ** Sending
    respond,

    -- * Headers
    Headers,
    getHeader,
    foldHeaders,
    foldlHeaders,

    -- ** Headers builder
    HeadersBuilder,
    emptyHeadersBuilder,
    addHeader,
    buildHeaders,
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, throwIO, try)
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as Http
import Network.HTTP.Types.Status qualified as Http
import Network.Wai qualified as Wai
import Spaceballs.Headers (Headers, HeadersBuilder, addHeader, addWaiHeaders, buildHeaders, emptyHeaders, emptyHeadersBuilder, foldHeaders, foldlHeaders, getHeader, headersToWaiHeaders)
import System.Random.Stateful qualified as Random
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (id)

------------------------------------------------------------------------------------------------------------------------
-- Application

-- | Make a WAI application.
application :: (Request -> IO Void) -> Wai.Application
application app request0 respond_ = do
  request <- makeRequest request0
  try (app request) >>= \case
    Left exception
      | Just (Respond response_) <- fromException @Respond exception -> respond_ response_
      | otherwise -> throwIO exception
    Right v -> absurd v

-- Internal exception type that indicates we have a response for the client.
newtype Respond
  = Respond Wai.Response

instance Exception Respond where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

instance Show Respond where
  show _ = "Respond"

------------------------------------------------------------------------------------------------------------------------
-- Router

data Router a = Router
  { resource :: !(Resource a),
    children :: !(Mapping Text (Router a))
  }

-- A lot of ugly code here just to get a `Router` that doesn't have a user-visible `Semigroup` instance... heh.
-- There's no harm in the instance, it just seems cleaner to give the user only one way to do things (namely, to
-- define each router as an element in a list, and not be able to <> them together).
newtype RouterWithSemigroup a
  = RouterWithSemigroup (Router a)

instance Semigroup (RouterWithSemigroup a) where
  (<>) :: RouterWithSemigroup a -> RouterWithSemigroup a -> RouterWithSemigroup a
  RouterWithSemigroup x <> RouterWithSemigroup y =
    RouterWithSemigroup (appendRouters x y)

emptyRouter :: Router a
emptyRouter =
  Router emptyResource EmptyMapping

appendRouters :: forall a. Router a -> Router a -> Router a
appendRouters x y =
  Router
    (x.resource <> y.resource)
    ( coerce
        @(Mapping Text (RouterWithSemigroup a))
        @(Mapping Text (Router a))
        ( coerce @(Mapping Text (Router a)) @(Mapping Text (RouterWithSemigroup a)) x.children
            <> coerce @(Mapping Text (Router a)) @(Mapping Text (RouterWithSemigroup a)) y.children
        )
    )

concatRouters :: forall a. [Router a] -> Router a
concatRouters =
  coerce
    @([RouterWithSemigroup a] -> RouterWithSemigroup a)
    @([Router a] -> Router a)
    concatRoutersWithSemigroup

concatRoutersWithSemigroup :: [RouterWithSemigroup a] -> RouterWithSemigroup a
concatRoutersWithSemigroup = \case
  [] -> RouterWithSemigroup emptyRouter
  x : xs ->
    x <> foldr (<>) (RouterWithSemigroup emptyRouter) xs

-- | Route a request to a resource.
route :: [Router a] -> [Text] -> Maybe (Resource a)
route =
  route1 . concatRouters

route1 :: Router a -> [Text] -> Maybe (Resource a)
route1 router = \case
  [] -> Just router.resource
  segment_ : segments -> do
    router1 <- runMapping router.children segment_
    route1 router1 segments

-- | Act upon a resource.
act :: Resource a -> Http.Method -> Maybe (a -> Text -> Params -> Headers -> ByteString -> IO Void)
act resource method
  | method == Http.methodGet = resource.get
  | method == Http.methodPost = resource.post
  | method == Http.methodPut = resource.put
  | method == Http.methodDelete = resource.delete
  | method == Http.methodPatch = resource.patch
  | otherwise = Nothing

-- | @DELETE@ a resource.
delete :: (a -> Text -> Params -> Headers -> ByteString -> IO Void) -> Router a
delete handler =
  emptyRouter
    { resource = emptyResource {delete = Just handler}
    }

-- | @GET@ a resource.
get :: (a -> Text -> Params -> Headers -> ByteString -> IO Void) -> Router a
get handler =
  emptyRouter
    { resource = emptyResource {get = Just handler}
    }

-- | @PATCH@ a resource.
patch :: (a -> Text -> Params -> Headers -> ByteString -> IO Void) -> Router a
patch handler =
  emptyRouter
    { resource = emptyResource {patch = Just handler}
    }

-- | @POST@ a resource.
post :: (a -> Text -> Params -> Headers -> ByteString -> IO Void) -> Router a
post handler =
  emptyRouter
    { resource = emptyResource {post = Just handler}
    }

-- | @PUT@ a resource.
put :: (a -> Text -> Params -> Headers -> ByteString -> IO Void) -> Router a
put handler =
  emptyRouter
    { resource = emptyResource {put = Just handler}
    }

-- | Capture a path segment.
capture :: (Text -> [Router a]) -> Router a
capture f =
  emptyRouter
    { children = TotalMapping (concatRouters . f)
    }

-- | Match a path segment.
segment :: Text -> [Router a] -> Router a
segment name router =
  emptyRouter
    { children = PartialMapping (Map.singleton name (concatRouters router))
    }

------------------------------------------------------------------------------------------------------------------------
-- Mapping

data Mapping a b
  = EmptyMapping
  | PartialMapping !(Map a b)
  | TotalMapping !(a -> b)
  | TotalMappingWithOverrides !(Map a b) !(a -> b)

instance (Ord a, Semigroup b) => Semigroup (Mapping a b) where
  EmptyMapping <> y = y
  x <> EmptyMapping = x
  PartialMapping x <> PartialMapping y = PartialMapping (Map.unionWith (<>) x y)
  PartialMapping x <> TotalMapping y = TotalMappingWithOverrides x y
  PartialMapping x <> TotalMappingWithOverrides y z = TotalMappingWithOverrides (Map.unionWith (<>) x y) z
  x@(TotalMapping _) <> _ = x
  TotalMappingWithOverrides x y <> TotalMappingWithOverrides z _ = TotalMappingWithOverrides (Map.unionWith (<>) x z) y
  x@(TotalMappingWithOverrides _ _) <> _ = x

runMapping :: (Ord a) => Mapping a b -> a -> Maybe b
runMapping = \case
  EmptyMapping -> const Nothing
  PartialMapping m -> (`Map.lookup` m)
  TotalMapping f -> Just . f
  TotalMappingWithOverrides m f -> Just . \x -> fromMaybe (f x) (Map.lookup x m)

------------------------------------------------------------------------------------------------------------------------
-- Handlers

-- A collection of resource handlers (one per HTTP verb that can be made upon the resource).
data Resource a = Resource
  { delete :: !(Maybe (a -> Text -> Params -> Headers -> ByteString -> IO Void)),
    get :: !(Maybe (a -> Text -> Params -> Headers -> ByteString -> IO Void)),
    patch :: !(Maybe (a -> Text -> Params -> Headers -> ByteString -> IO Void)),
    post :: !(Maybe (a -> Text -> Params -> Headers -> ByteString -> IO Void)),
    put :: !(Maybe (a -> Text -> Params -> Headers -> ByteString -> IO Void))
  }

-- Left-biased
instance Semigroup (Resource a) where
  Resource a1 b1 c1 d1 e1 <> Resource a2 b2 c2 d2 e2 =
    Resource (a1 <|> a2) (b1 <|> b2) (c1 <|> c2) (d1 <|> d2) (e1 <|> e2)

emptyResource :: Resource a
emptyResource =
  Resource Nothing Nothing Nothing Nothing Nothing

------------------------------------------------------------------------------------------------------------------------
-- Request

data Request = Request
  { body :: !ByteString,
    headers :: !Headers,
    id :: !Text,
    method :: !Http.Method,
    params :: !Params,
    path :: ![Text]
  }
  deriving stock (Eq, Generic)

makeRequest :: Wai.Request -> IO Request
makeRequest request = do
  id <- Random.uniformByteStringM 16 Random.globalStdGen
  body <- Wai.consumeRequestBodyStrict request
  pure
    Request
      { body = LazyByteString.toStrict body,
        headers = buildHeaders (addWaiHeaders (Wai.requestHeaders request)),
        id = extractBase64 (Base64.Url.encodeBase64Unpadded id),
        method = Wai.requestMethod request,
        params = makeParams (Wai.queryString request),
        path = Wai.pathInfo request
      }

------------------------------------------------------------------------------------------------------------------------
-- Params

newtype Params
  = Params (Map Text (P Array 'True))
  deriving stock (Eq)

data P :: (Type -> Type) -> Bool -> Type where
  SingleParam :: !Text -> P f a
  ListOfParams :: !(f (P f 'False)) -> P f 'True

deriving stock instance (forall x. (Eq x) => Eq (f x)) => (Eq (P f a))

coercePs :: [P f 'False] -> [P g b]
coercePs = unsafeCoerce
{-# INLINE coercePs #-}

singleParam :: P f 'False -> Text
singleParam = \case
  SingleParam s -> s

makeParams :: [(ByteString, Maybe ByteString)] -> Params
makeParams =
  coerce finalize . go
  where
    go :: [(ByteString, Maybe ByteString)] -> Map Text (P [] 'True)
    go =
      List.foldl' f Map.empty
      where
        f :: Map Text (P [] 'True) -> (ByteString, Maybe ByteString) -> Map Text (P [] 'True)
        f acc (k, mv) =
          Map.alter g (Text.decodeUtf8 k) acc
          where
            g :: Maybe (P [] 'True) -> Maybe (P [] 'True)
            g =
              Just . \case
                Nothing -> v
                Just (SingleParam x) -> ListOfParams [v, SingleParam x]
                Just (ListOfParams xs) -> ListOfParams (v : xs)

            v :: P [] b
            v =
              SingleParam (maybe Text.empty Text.decodeUtf8 mv)

    finalize :: Map Text (P [] 'True) -> Map Text (P Array 'True)
    finalize ps =
      Map.map f ps
      where
        f :: P [] 'True -> P Array 'True
        f = \case
          SingleParam x -> SingleParam x
          ListOfParams xs -> ListOfParams (Array.arrayFromList (coercePs (reverse xs)))

allParams :: P Array a -> Array Text
allParams = \case
  SingleParam s ->
    Array.createArray 1 undefined \array ->
      Array.writeArray array 0 s
  ListOfParams ss -> Array.mapArray' singleParam ss

firstParam :: P Array a -> Text
firstParam = \case
  SingleParam s -> s
  ListOfParams ss -> singleParam (Array.indexArray ss 0)

lookupParam :: Text -> Params -> Maybe (P Array 'True)
lookupParam =
  coerce @(Text -> Map Text (P Array 'True) -> Maybe (P Array 'True)) Map.lookup

------------------------------------------------------------------------------------------------------------------------
-- Query params

-- | A query parameter parser.
newtype Param a
  = Param (Text -> Maybe a)
  deriving stock (Functor)

instance Alternative Param where
  empty = Param \_ -> Nothing
  Param x <|> Param y = Param \s -> x s <|> y s

instance Applicative Param where
  pure x = Param \_ -> Just x
  Param x <*> Param y = Param \s -> x s <*> y s

ptext :: Param Text
ptext =
  Param Just

-- | Get an optional parameter from the request.
--
-- If multiple parameters with the given name exist, this function returns the first.
param :: Request -> Text -> Param a -> IO (Maybe a)
param request name (Param parser) = do
  case lookupParam name request.params of
    Nothing -> pure Nothing
    Just p ->
      case parser (firstParam p) of
        Nothing -> respondBadParameter
        Just result -> pure (Just result)

-- | Get an optional parameter from the request.
--
-- If multiple parameters with the given name exist, this function returns them all.
params :: Request -> Text -> Param a -> IO (Array a)
params request name (Param parser) =
  case lookupParam name request.params of
    Nothing -> pure Array.emptyArray
    Just p ->
      case traverse parser (allParams p) of
        Nothing -> respondBadParameter
        Just result -> pure result

respondBadParameter :: IO void
respondBadParameter =
  respondWai (Wai.responseBuilder Http.status400 [] mempty)

------------------------------------------------------------------------------------------------------------------------
-- Response

data Response = Response
  { body :: !ByteString,
    headers :: !Headers,
    status :: {-# UNPACK #-} !Int
  }

response :: Int -> Response
response status =
  Response
    { body = ByteString.empty,
      headers = emptyHeaders,
      status
    }

setHeaders :: Headers -> Response -> Response
setHeaders headers response_ =
  Response
    { body = response_.body,
      headers = headers,
      status = response_.status
    }

setBody :: ByteString -> Response -> Response
setBody body response_ =
  Response
    { body,
      headers = response_.headers,
      status = response_.status
    }

responseToWai :: Response -> Wai.Response
responseToWai response_ =
  Wai.responseLBS
    (intToStatus response_.status)
    (headersToWaiHeaders response_.headers)
    (LazyByteString.fromStrict response_.body)

-- | Respond to the client.
respond :: Response -> IO void
respond =
  respondWai . responseToWai

respondWai :: Wai.Response -> IO void
respondWai =
  throwIO . Respond

intToStatus :: Int -> Http.Status
intToStatus = \case
  100 -> Http.status100
  101 -> Http.status101
  200 -> Http.status200
  201 -> Http.status201
  202 -> Http.status202
  203 -> Http.status203
  204 -> Http.status204
  205 -> Http.status205
  206 -> Http.status206
  300 -> Http.status300
  301 -> Http.status301
  302 -> Http.status302
  303 -> Http.status303
  304 -> Http.status304
  305 -> Http.status305
  307 -> Http.status307
  308 -> Http.status308
  400 -> Http.status400
  401 -> Http.status401
  402 -> Http.status402
  403 -> Http.status403
  404 -> Http.status404
  405 -> Http.status405
  406 -> Http.status406
  407 -> Http.status407
  408 -> Http.status408
  409 -> Http.status409
  410 -> Http.status410
  411 -> Http.status411
  412 -> Http.status412
  413 -> Http.status413
  414 -> Http.status414
  415 -> Http.status415
  416 -> Http.status416
  417 -> Http.status417
  418 -> Http.status418
  422 -> Http.status422
  426 -> Http.status426
  428 -> Http.status428
  429 -> Http.status429
  431 -> Http.status431
  500 -> Http.status500
  501 -> Http.status501
  502 -> Http.status502
  503 -> Http.status503
  504 -> Http.status504
  505 -> Http.status505
  511 -> Http.status511
  status -> Http.Status status ByteString.empty
