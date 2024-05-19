module Spaceballs
  ( -- * Application
    application,

    -- * Router
    Router,
    router,
    on404,
    on405,

    -- * Handler
    Handler,
    delete,
    get,
    patch,
    post,
    put,

    -- ** Route capture
    capture,
    segment,

    -- * Request
    Request (..),
    Params,

    -- ** Query params
    param,
    params,
    paramsToMap,

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
    headersToList,
    headersToMap,
    foldMapHeaders,
    foldlHeaders,
    foldrHeaders,

    -- ** Headers builder
    HeadersBuilder,
    header,
    headers,
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
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
import Spaceballs.Headers
  ( Headers,
    HeadersBuilder,
    emptyHeaders,
    foldMapHeaders,
    foldlHeaders,
    foldrHeaders,
    getHeader,
    header,
    headers,
    headersToList,
    headersToMap,
    headersToWaiHeaders,
    waiHeaders,
  )
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (id)

------------------------------------------------------------------------------------------------------------------------
-- Application

-- | Make a WAI application.
application :: Router a -> a -> Wai.Application
application router_ env request0 respond_ = do
  request <- makeRequest request0
  try (applyRouter router_ env request) >>= \case
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
  show _ = "«internal exception»"

------------------------------------------------------------------------------------------------------------------------
-- Router

data Router a = Router
  { routerRoutes :: [Handler a],
    routerOn404 :: a -> Request -> IO Void,
    routerOn405 :: a -> Request -> IO Void
  }

-- | Make a router from a collection of routes.
router :: [Handler a] -> Router a
router routes =
  Router
    { routerRoutes = routes,
      routerOn404 = \_ _ -> respond (response 404),
      routerOn405 = \_ _ -> respond (response 405)
    }

-- | Set the @404 Not Found@ handler.
on404 :: (a -> Request -> IO Void) -> Router a -> Router a
on404 handler router_ =
  router_ {routerOn404 = handler}

-- | Set the @405 Method Not Allowed@ handler.
on405 :: (a -> Request -> IO Void) -> Router a -> Router a
on405 handler router_ =
  router_ {routerOn405 = handler}

-- | Route a request to a resource.
applyRouter :: Router a -> a -> Request -> IO Void
applyRouter router2 = \env request ->
  case routePath request.path of
    Nothing -> router2.routerOn404 env request
    Just resource ->
      case applyPathHandlers resource request.method of
        Nothing -> router2.routerOn405 env request
        Just handler -> handler env request
  where
    routePath =
      applyHandler (concatHandlers router2.routerRoutes)

------------------------------------------------------------------------------------------------------------------------
-- Handler

-- | A handler.
data Handler a = Handler
  { here :: !(PathHandlers a),
    there :: !(Mapping Text (Handler a))
  }

-- A lot of ugly code here just to get a `Handler` that doesn't have a user-visible `Semigroup` instance... heh.
-- There's no harm in the instance, it just seems cleaner to give the user only one way to do things (namely, to
-- define each handler as an element in a list, and not be able to <> them together).
newtype Handler' a
  = Handler' (Handler a)

instance Semigroup (Handler' a) where
  (<>) :: Handler' a -> Handler' a -> Handler' a
  Handler' x <> Handler' y =
    Handler' (appendHandlers x y)

emptyHandler :: Handler a
emptyHandler =
  Handler emptyPathHandlers EmptyMapping

emptyHandler' :: Handler' a
emptyHandler' =
  Handler' emptyHandler

appendHandlers :: forall a. Handler a -> Handler a -> Handler a
appendHandlers x y =
  Handler
    { here = x.here <> y.here,
      there = untick (tick x.there <> tick y.there)
    }
  where
    tick = coerce @(Mapping Text (Handler a)) @(Mapping Text (Handler' a))
    untick = coerce @(Mapping Text (Handler' a)) @(Mapping Text (Handler a))
    {-# INLINE tick #-}
    {-# INLINE untick #-}

concatHandlers :: forall a. [Handler a] -> Handler a
concatHandlers =
  coerce @([Handler' a] -> Handler' a) @([Handler a] -> Handler a) concatHandlers'

concatHandlers' :: [Handler' a] -> Handler' a
concatHandlers' = \case
  [] -> emptyHandler'
  x : xs -> x <> foldr (<>) emptyHandler' xs

applyHandler :: Handler a -> [Text] -> Maybe (PathHandlers a)
applyHandler router_ = \case
  [] -> Just router_.here
  segment_ : segments -> do
    router1 <- runMapping router_.there segment_
    applyHandler router1 segments

-- | Handle a @DELETE@ request.
delete :: (a -> Request -> IO Void) -> Handler a
delete handler =
  emptyHandler
    { here = emptyPathHandlers {delete = Just handler}
    }

-- | Handle a @GET@ request.
get :: (a -> Request -> IO Void) -> Handler a
get handler =
  emptyHandler
    { here = emptyPathHandlers {get = Just handler}
    }

-- | Handle a @PATCH@ request.
patch :: (a -> Request -> IO Void) -> Handler a
patch handler =
  emptyHandler
    { here = emptyPathHandlers {patch = Just handler}
    }

-- | Handle a @POST@ request.
post :: (a -> Request -> IO Void) -> Handler a
post handler =
  emptyHandler
    { here = emptyPathHandlers {post = Just handler}
    }

-- | Handle a @PUT@ request.
put :: (a -> Request -> IO Void) -> Handler a
put handler =
  emptyHandler
    { here = emptyPathHandlers {put = Just handler}
    }

-- | Capture a path segment.
capture :: (Text -> [Handler a]) -> Handler a
capture f =
  emptyHandler
    { there = TotalMapping (concatHandlers . f)
    }

-- | Match a path segment.
segment :: Text -> [Handler a] -> Handler a
segment name router_ =
  emptyHandler
    { there = PartialMapping (Map.singleton name (concatHandlers router_))
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
-- Path handlers

-- A collection of resource handlers (one per HTTP verb that can be made upon the resource).
data PathHandlers a = PathHandlers
  { delete :: !(Maybe (a -> Request -> IO Void)),
    get :: !(Maybe (a -> Request -> IO Void)),
    patch :: !(Maybe (a -> Request -> IO Void)),
    post :: !(Maybe (a -> Request -> IO Void)),
    put :: !(Maybe (a -> Request -> IO Void))
  }

-- Left-biased
instance Semigroup (PathHandlers a) where
  PathHandlers a1 b1 c1 d1 e1 <> PathHandlers a2 b2 c2 d2 e2 =
    PathHandlers (a1 <|> a2) (b1 <|> b2) (c1 <|> c2) (d1 <|> d2) (e1 <|> e2)

emptyPathHandlers :: PathHandlers a
emptyPathHandlers =
  PathHandlers Nothing Nothing Nothing Nothing Nothing

applyPathHandlers :: PathHandlers a -> Http.Method -> Maybe (a -> Request -> IO Void)
applyPathHandlers handlers method
  | method == Http.methodGet = handlers.get
  | method == Http.methodPost = handlers.post
  | method == Http.methodPut = handlers.put
  | method == Http.methodDelete = handlers.delete
  | method == Http.methodPatch = handlers.patch
  | otherwise = Nothing

------------------------------------------------------------------------------------------------------------------------
-- Request

data Request = Request
  { body :: !ByteString,
    headers :: !Headers,
    method :: !Http.Method,
    params :: !Params,
    path :: ![Text]
  }
  deriving stock (Eq, Generic)

makeRequest :: Wai.Request -> IO Request
makeRequest request = do
  body <- Wai.consumeRequestBodyStrict request
  pure
    Request
      { body = LazyByteString.toStrict body,
        headers = headers (waiHeaders (Wai.requestHeaders request)),
        method = Wai.requestMethod request,
        params = makeParams (Wai.queryString request),
        path = Wai.pathInfo request
      }

------------------------------------------------------------------------------------------------------------------------
-- Params

newtype Params
  = Params (Map Text (P Array 'SingleOrList))
  deriving stock (Eq)

data ParamType
  = DefinitelySingle
  | SingleOrList

data P :: (Type -> Type) -> ParamType -> Type where
  SingleParam :: !Text -> P f a
  ListOfParams :: !(f (P f 'DefinitelySingle)) -> P f 'SingleOrList

deriving stock instance (forall x. (Eq x) => Eq (f x)) => (Eq (P f a))

coercePs :: [P f 'DefinitelySingle] -> [P g b]
coercePs = unsafeCoerce
{-# INLINE coercePs #-}

singleParam :: P f 'DefinitelySingle -> Text
singleParam = \case
  SingleParam s -> s

makeParams :: [(ByteString, Maybe ByteString)] -> Params
makeParams =
  coerce finalize . go
  where
    go :: [(ByteString, Maybe ByteString)] -> Map Text (P [] 'SingleOrList)
    go =
      List.foldl' f Map.empty
      where
        f :: Map Text (P [] 'SingleOrList) -> (ByteString, Maybe ByteString) -> Map Text (P [] 'SingleOrList)
        f acc (k, mv) =
          Map.alter g (Text.decodeUtf8 k) acc
          where
            g :: Maybe (P [] 'SingleOrList) -> Maybe (P [] 'SingleOrList)
            g =
              Just . \case
                Nothing -> v
                Just (SingleParam x) -> ListOfParams [v, SingleParam x]
                Just (ListOfParams xs) -> ListOfParams (v : xs)

            v :: P [] b
            v =
              SingleParam (maybe Text.empty Text.decodeUtf8 mv)

    finalize :: Map Text (P [] 'SingleOrList) -> Map Text (P Array 'SingleOrList)
    finalize ps =
      Map.map f ps
      where
        f :: P [] 'SingleOrList -> P Array 'SingleOrList
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

lookupParam :: Text -> Params -> Maybe (P Array 'SingleOrList)
lookupParam =
  coerce @(Text -> Map Text (P Array 'SingleOrList) -> Maybe (P Array 'SingleOrList)) Map.lookup

------------------------------------------------------------------------------------------------------------------------
-- Query params

-- | Get an optional parameter.
--
-- If multiple parameters with the given name exist, this function returns the first.
param :: Params -> Text -> Maybe Text
param ps name = do
  firstParam <$> lookupParam name ps

-- | Get an optional parameter.
--
-- If multiple parameters with the given name exist, this function returns them all.
params :: Params -> Text -> Array Text
params ps name =
  maybe (mempty @(Array Text)) allParams (lookupParam name ps)

paramsToMap :: Params -> Map Text (Array Text)
paramsToMap =
  coerce (Map.map allParams)

------------------------------------------------------------------------------------------------------------------------
-- Response

data Response = Response
  { body :: !ByteString,
    headers :: !Headers,
    status :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Generic)

response :: Int -> Response
response status =
  Response
    { body = ByteString.empty,
      headers = emptyHeaders,
      status
    }

setHeaders :: Headers -> Response -> Response
setHeaders hdrs response_ =
  Response
    { body = response_.body,
      headers = hdrs,
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
intToStatus status =
  IntMap.findWithDefault (Http.Status status ByteString.empty) status intToStatusMap

intToStatusMap :: IntMap Http.Status
intToStatusMap =
  IntMap.fromList
    [ (100, Http.status100),
      (101, Http.status101),
      (200, Http.status200),
      (201, Http.status201),
      (202, Http.status202),
      (203, Http.status203),
      (204, Http.status204),
      (205, Http.status205),
      (206, Http.status206),
      (300, Http.status300),
      (301, Http.status301),
      (302, Http.status302),
      (303, Http.status303),
      (304, Http.status304),
      (305, Http.status305),
      (307, Http.status307),
      (308, Http.status308),
      (400, Http.status400),
      (401, Http.status401),
      (402, Http.status402),
      (403, Http.status403),
      (404, Http.status404),
      (405, Http.status405),
      (406, Http.status406),
      (407, Http.status407),
      (408, Http.status408),
      (409, Http.status409),
      (410, Http.status410),
      (411, Http.status411),
      (412, Http.status412),
      (413, Http.status413),
      (414, Http.status414),
      (415, Http.status415),
      (416, Http.status416),
      (417, Http.status417),
      (418, Http.status418),
      (422, Http.status422),
      (426, Http.status426),
      (428, Http.status428),
      (429, Http.status429),
      (431, Http.status431),
      (500, Http.status500),
      (501, Http.status501),
      (502, Http.status502),
      (503, Http.status503),
      (504, Http.status504),
      (505, Http.status505),
      (511, Http.status511)
    ]
