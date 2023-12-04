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

    -- * Handler monad
    MonadHandler (..),
    Respond,

    -- ** Request
    Request (..),
    Params,
    makeRequest,

    -- *** Query params
    Param,
    ptext,
    param,
    params,

    -- ** Response
    respond,
    respondFile,
    respondStream,
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, throwIO, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Builder qualified as ByteString (Builder)
import Data.ByteString.Lazy (LazyByteString)
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
application ::
  (MonadHandler m) =>
  (forall a. m a -> IO a) ->
  [Router m] ->
  IO Wai.ResponseReceived
application runHandler routers = do
  let router = concatRouters routers
  request <- runHandler askRequest
  resp <- runHandler askRespond
  let handlers = routerHandlersAtPath router request.path
  if isEmptyHandlers handlers
    then resp (Wai.responseBuilder Http.status404 [] mempty)
    else case findHandler handlers request.method of
      NoHandler -> resp (Wai.responseBuilder Http.status405 [] mempty)
      Handler handler ->
        try (runHandler handler) >>= \case
          Left (Done sent) -> pure sent
          Right v -> absurd v

-- Internal exception type that indicates we've responded to the client.
-- TODO make this an async exception so it's less likely to be caught and ignored
newtype Done
  = Done Wai.ResponseReceived
  deriving anyclass (Exception)

instance Show Done where
  show (Done _) = "Done"

------------------------------------------------------------------------------------------------------------------------
-- Router

data Router m = Router
  { handlers :: !(Handlers m),
    children :: !(Mapping Text (Router m))
  }

-- A lot of ugly code here just to get a `Router` that doesn't have a user-visible `Semigroup` instance... heh.
-- There's no harm in the instance, it just seems cleaner to give the user only one way to do things (namely, to
-- define each router as an element in a list, and not be able to <> them together).
newtype RouterWithSemigroup m
  = RouterWithSemigroup (Router m)

instance Semigroup (RouterWithSemigroup m) where
  RouterWithSemigroup x <> RouterWithSemigroup y = RouterWithSemigroup (appendRouters x y)

emptyRouter :: Router m
emptyRouter =
  Router emptyHandlers EmptyMapping

appendRouters :: forall m. Router m -> Router m -> Router m
appendRouters x y =
  Router
    (x.handlers <> y.handlers)
    ( coerce
        @(Mapping Text (RouterWithSemigroup m))
        @(Mapping Text (Router m))
        ( coerce @(Mapping Text (Router m)) @(Mapping Text (RouterWithSemigroup m)) x.children
            <> coerce @(Mapping Text (Router m)) @(Mapping Text (RouterWithSemigroup m)) y.children
        )
    )

concatRouters :: forall m. [Router m] -> Router m
concatRouters =
  coerce
    @([RouterWithSemigroup m] -> RouterWithSemigroup m)
    @([Router m] -> Router m)
    concatRoutersWithSemigroup

concatRoutersWithSemigroup :: forall m. [RouterWithSemigroup m] -> RouterWithSemigroup m
concatRoutersWithSemigroup = \case
  [] -> RouterWithSemigroup emptyRouter
  x : xs ->
    x <> foldr (<>) (RouterWithSemigroup emptyRouter) xs

routerHandlersAtPath :: Router m -> [Text] -> Handlers m
routerHandlersAtPath router = \case
  [] -> router.handlers
  x : xs ->
    case runMapping router.children x of
      Nothing -> emptyHandlers
      Just router1 -> routerHandlersAtPath router1 xs

-- | @DELETE@ a resource.
delete :: m Void -> Router m
delete handler =
  emptyRouter
    { handlers = emptyHandlers {delete = Handler handler}
    }

-- | @GET@ a resource.
get :: m Void -> Router m
get handler =
  emptyRouter
    { handlers = emptyHandlers {get = Handler handler}
    }

-- | @PATCH@ a resource.
patch :: m Void -> Router m
patch handler =
  emptyRouter
    { handlers = emptyHandlers {patch = Handler handler}
    }

-- | @POST@ a resource.
post :: m Void -> Router m
post handler =
  emptyRouter
    { handlers = emptyHandlers {post = Handler handler}
    }

-- | @PUT@ a resource.
put :: m Void -> Router m
put handler =
  emptyRouter
    { handlers = emptyHandlers {put = Handler handler}
    }

-- | Capture a path segment.
capture :: (Text -> [Router m]) -> Router m
capture f =
  emptyRouter
    { children = TotalMapping (concatRouters . f)
    }

-- | Match a path segment.
segment :: Text -> [Router m] -> Router m
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
data Handlers m = Handlers
  { delete :: !(Handler m),
    get :: !(Handler m),
    patch :: !(Handler m),
    post :: !(Handler m),
    put :: !(Handler m)
  }

-- Left-biased
instance Semigroup (Handlers m) where
  Handlers a1 b1 c1 d1 e1 <> Handlers a2 b2 c2 d2 e2 =
    Handlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

emptyHandlers :: Handlers m
emptyHandlers =
  Handlers NoHandler NoHandler NoHandler NoHandler NoHandler

isEmptyHandlers :: Handlers m -> Bool
isEmptyHandlers = \case
  Handlers NoHandler NoHandler NoHandler NoHandler NoHandler -> True
  _ -> False

findHandler :: Handlers m -> Http.Method -> Handler m
findHandler handlers method
  | method == Http.methodGet = handlers.get
  | method == Http.methodPost = handlers.post
  | method == Http.methodPut = handlers.put
  | method == Http.methodDelete = handlers.delete
  | method == Http.methodPatch = handlers.patch
  | otherwise = NoHandler

------------------------------------------------------------------------------------------------------------------------
-- Resource handler

data Handler m
  = NoHandler
  | Handler !(m Void)

-- Left-biased
instance Semigroup (Handler m) where
  NoHandler <> x = x
  x <> _ = x

------------------------------------------------------------------------------------------------------------------------
-- Handler monad

class (MonadIO m) => MonadHandler m where
  askRequest :: m Request
  askRespond :: m Respond

type Respond =
  Wai.Response -> IO Wai.ResponseReceived

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
param :: (MonadHandler m) => Text -> Param a -> m (Maybe a)
param name (Param parser) = do
  request <- askRequest
  case lookupParam name request.params of
    Nothing -> pure Nothing
    Just p ->
      case parser (firstParam p) of
        Nothing -> respondBadParameter
        Just result -> pure (Just result)

-- | Get an optional parameter from the request.
--
-- If multiple parameters with the given name exist, this function returns them all.
params :: (MonadHandler m) => Text -> Param a -> m (Array a)
params name (Param parser) = do
  request <- askRequest
  case lookupParam name request.params of
    Nothing -> pure Array.emptyArray
    Just p ->
      case traverse parser (allParams p) of
        Nothing -> respondBadParameter
        Just result -> pure result

respondBadParameter :: (MonadHandler m) => m void
respondBadParameter =
  respondWith (Wai.responseBuilder Http.status400 [] mempty)

------------------------------------------------------------------------------------------------------------------------
-- Response

-- | Respond to the client.
respond :: (MonadHandler m) => Int -> [Http.Header] -> LazyByteString -> m void
respond status headers body =
  respondWith (Wai.responseLBS (intToStatus status) headers body)

-- | Respond to the client with a file.
respondFile :: (MonadHandler m) => [Http.Header] -> FilePath -> m void
respondFile headers file =
  -- status is irrelevant here because warp ignores it (lolwtf?)
  -- https://github.com/yesodweb/wai/issues/527
  respondWith (Wai.responseFile Http.status200 headers file Nothing)

-- | Respond to the client with a stream of bytes.
respondStream :: (MonadHandler m) => Int -> [Http.Header] -> ((ByteString.Builder -> IO ()) -> IO ()) -> m void
respondStream status headers withSend =
  respondWith (Wai.responseStream (intToStatus status) headers \send _flush -> withSend send)

respondWith :: (MonadHandler m) => Wai.Response -> m void
respondWith response = do
  resp <- askRespond
  liftIO do
    sent <- resp response
    throwIO (Done sent)

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
