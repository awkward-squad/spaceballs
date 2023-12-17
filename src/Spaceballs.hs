module Spaceballs
  ( -- * Application
    application,

    -- * Router
    Router,

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

    -- ** Headers
    header,

    -- ** Query params
    Param,
    ptext,
    param,
    params,

    -- * Response

    -- ** Building
    Response (..),
    response,
    addHeader,
    setBody,

    -- ** Sending
    respond,
    respondFile,
    respondStream,
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Builder qualified as ByteString (Builder)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
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
import System.Random.Stateful qualified as Random
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (id)

------------------------------------------------------------------------------------------------------------------------
-- Application

-- | Make a WAI application.
application :: [Router] -> (Request -> IO Void) -> Wai.Application
application routers notFoundHandler =
  \request0 respond_ -> do
    request <- makeRequest request0
    let router = concatRouters routers
    let handlers = routerHandlersAtPath router request.path
    case if isEmptyHandlers handlers then YesHandler notFoundHandler else findHandler handlers request.method of
      NoHandler -> respond_ (Wai.responseBuilder Http.status405 [] mempty)
      YesHandler handler ->
        try (handler request) >>= \case
          Left exception
            | Just (Respond response_) <- fromException @Respond exception -> respond_ response_
            | Just (Sent sent) <- fromException @Sent exception -> pure sent
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

-- Internal exception type that indicates we've sent a response to the client.
newtype Sent
  = Sent Wai.ResponseReceived

instance Exception Sent where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

instance Show Sent where
  show _ = "Sent"

------------------------------------------------------------------------------------------------------------------------
-- Router

data Router = Router
  { handlers :: !ResourceHandlers,
    children :: !(Mapping Text Router)
  }

-- A lot of ugly code here just to get a `Router` that doesn't have a user-visible `Semigroup` instance... heh.
-- There's no harm in the instance, it just seems cleaner to give the user only one way to do things (namely, to
-- define each router as an element in a list, and not be able to <> them together).
newtype RouterWithSemigroup
  = RouterWithSemigroup Router

instance Semigroup RouterWithSemigroup where
  (<>) :: RouterWithSemigroup -> RouterWithSemigroup -> RouterWithSemigroup
  (<>) = coerce appendRouters

emptyRouter :: Router
emptyRouter =
  Router emptyResourceHandlers EmptyMapping

appendRouters :: Router -> Router -> Router
appendRouters x y =
  Router
    (x.handlers <> y.handlers)
    ( coerce
        @(Mapping Text RouterWithSemigroup)
        @(Mapping Text Router)
        ( coerce @(Mapping Text Router) @(Mapping Text RouterWithSemigroup) x.children
            <> coerce @(Mapping Text Router) @(Mapping Text RouterWithSemigroup) y.children
        )
    )

concatRouters :: [Router] -> Router
concatRouters =
  coerce
    @([RouterWithSemigroup] -> RouterWithSemigroup)
    @([Router] -> Router)
    concatRoutersWithSemigroup

concatRoutersWithSemigroup :: [RouterWithSemigroup] -> RouterWithSemigroup
concatRoutersWithSemigroup = \case
  [] -> RouterWithSemigroup emptyRouter
  x : xs ->
    x <> foldr (<>) (RouterWithSemigroup emptyRouter) xs

routerHandlersAtPath :: Router -> [Text] -> ResourceHandlers
routerHandlersAtPath router = \case
  [] -> router.handlers
  x : xs ->
    case runMapping router.children x of
      Nothing -> emptyResourceHandlers
      Just router1 -> routerHandlersAtPath router1 xs

-- | @DELETE@ a resource.
delete :: (Request -> IO Void) -> Router
delete handler =
  emptyRouter
    { handlers = emptyResourceHandlers {delete = YesHandler handler}
    }

-- | @GET@ a resource.
get :: (Request -> IO Void) -> Router
get handler =
  emptyRouter
    { handlers = emptyResourceHandlers {get = YesHandler handler}
    }

-- | @PATCH@ a resource.
patch :: (Request -> IO Void) -> Router
patch handler =
  emptyRouter
    { handlers = emptyResourceHandlers {patch = YesHandler handler}
    }

-- | @POST@ a resource.
post :: (Request -> IO Void) -> Router
post handler =
  emptyRouter
    { handlers = emptyResourceHandlers {post = YesHandler handler}
    }

-- | @PUT@ a resource.
put :: (Request -> IO Void) -> Router
put handler =
  emptyRouter
    { handlers = emptyResourceHandlers {put = YesHandler handler}
    }

-- | Capture a path segment.
capture :: (Text -> [Router]) -> Router
capture f =
  emptyRouter
    { children = TotalMapping (concatRouters . f)
    }

-- | Match a path segment.
segment :: Text -> [Router] -> Router
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
data ResourceHandlers = ResourceHandlers
  { delete :: !MaybeHandler,
    get :: !MaybeHandler,
    patch :: !MaybeHandler,
    post :: !MaybeHandler,
    put :: !MaybeHandler
  }

-- Left-biased
instance Semigroup ResourceHandlers where
  ResourceHandlers a1 b1 c1 d1 e1 <> ResourceHandlers a2 b2 c2 d2 e2 =
    ResourceHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

emptyResourceHandlers :: ResourceHandlers
emptyResourceHandlers =
  ResourceHandlers NoHandler NoHandler NoHandler NoHandler NoHandler

isEmptyHandlers :: ResourceHandlers -> Bool
isEmptyHandlers = \case
  ResourceHandlers NoHandler NoHandler NoHandler NoHandler NoHandler -> True
  _ -> False

findHandler :: ResourceHandlers -> Http.Method -> MaybeHandler
findHandler handlers method
  | method == Http.methodGet = handlers.get
  | method == Http.methodPost = handlers.post
  | method == Http.methodPut = handlers.put
  | method == Http.methodDelete = handlers.delete
  | method == Http.methodPatch = handlers.patch
  | otherwise = NoHandler

------------------------------------------------------------------------------------------------------------------------
-- Resource handler

data MaybeHandler
  = NoHandler
  | YesHandler !(Request -> IO Void)

-- Left-biased
instance Semigroup MaybeHandler where
  NoHandler <> x = x
  x <> _ = x

------------------------------------------------------------------------------------------------------------------------
-- Request

data Request = Request
  { body :: !ByteString,
    headers :: !(Map (CaseInsensitive.CI Text) Text),
    id :: !Text,
    method :: !Http.Method,
    params :: !Params,
    path :: ![Text]
  }
  deriving stock (Eq, Generic)

makeRequest :: Wai.Request -> IO Request
makeRequest request = do
  id <- Base64.Url.encodeBase64Unpadded <$> Random.uniformByteStringM 16 Random.globalStdGen
  body <- LazyByteString.toStrict <$> Wai.consumeRequestBodyStrict request
  pure
    Request
      { body,
        headers = makeHeaders (Wai.requestHeaders request),
        id,
        method = Wai.requestMethod request,
        params = makeParams (Wai.queryString request),
        path = Wai.pathInfo request
      }

makeHeaders :: [(CaseInsensitive.CI ByteString, ByteString)] -> Map (CaseInsensitive.CI Text) Text
makeHeaders =
  List.foldl' step Map.empty
  where
    step ::
      Map (CaseInsensitive.CI Text) Text ->
      (CaseInsensitive.CI ByteString, ByteString) ->
      Map (CaseInsensitive.CI Text) Text
    step acc (k, v0) =
      Map.alter f (CaseInsensitive.map Text.decodeUtf8 k) acc
      where
        f = \case
          Nothing -> Just v1
          Just v2 -> Just (v2 <> ", " <> v1)
        v1 = Text.decodeUtf8 v0

------------------------------------------------------------------------------------------------------------------------
-- Headers

header :: Request -> CaseInsensitive.CI Text -> Maybe Text
header request name =
  Map.lookup name request.headers

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
  respondWith (Wai.responseBuilder Http.status400 [] mempty)

------------------------------------------------------------------------------------------------------------------------
-- Response

data Response = Response
  { body :: !ByteString,
    headers :: ![Http.Header],
    status :: {-# UNPACK #-} !Int
  }

response :: Int -> Response
response status =
  Response
    { body = ByteString.empty,
      headers = [],
      status
    }

addHeader :: Text -> Text -> Response -> Response
addHeader key value response_ =
  Response
    { body = response_.body,
      headers = (key', value') : response_.headers,
      status = response_.status
    }
  where
    !key' = CaseInsensitive.mk (Text.encodeUtf8 key)
    !value' = Text.encodeUtf8 value

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
    response_.headers
    (LazyByteString.fromStrict response_.body)

-- | Respond to the client.
respond :: Response -> IO void
respond =
  respondWith . responseToWai

-- | Respond to the client with a file.
respondFile :: [Http.Header] -> FilePath -> IO void
respondFile headers file =
  -- status is irrelevant here because warp ignores it (lolwtf?)
  -- https://github.com/yesodweb/wai/issues/527
  respondWith (Wai.responseFile Http.status200 headers file Nothing)

-- | Respond to the client with a stream of bytes.
respondStream ::
  (Wai.Response -> IO Wai.ResponseReceived) ->
  Int ->
  [Http.Header] ->
  ((ByteString.Builder -> IO ()) -> IO ()) ->
  IO void
respondStream respond_ status headers withSend = do
  sent <- respond_ (Wai.responseStream (intToStatus status) headers \send _flush -> withSend send)
  throwIO (Sent sent)

respondWith :: Wai.Response -> IO void
respondWith resp =
  throwIO (Respond resp)

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
