module Spaceballs
  ( -- * Application
    application,

    -- * Router monad
    Router,
    text,
    theText,

    -- ** Resource
    Resource,
    delete,
    get,
    patch,
    post,
    put,

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
    FileResponse (..),
    respondFile,
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Random.Stateful qualified as Random
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (id)

------------------------------------------------------------------------------------------------------------------------
-- Application

-- | Make a WAI application.
application ::
  MonadHandler m =>
  (forall a. m a -> IO a) ->
  Router (Resource m) ->
  IO Wai.ResponseReceived
application runHandler router = do
  request <- runHandler askRequest
  resp <- runHandler askRespond
  case runRouter request.path (router <* end) of
    Just (f, []) ->
      case runResource f request.method of
        Nothing -> resp (Wai.responseBuilder Http.status405 [] mempty)
        Just action ->
          try (runHandler action) >>= \case
            Left (Done sent) -> pure sent
            Right v -> absurd v
    _ -> resp (Wai.responseBuilder Http.status400 [] mempty)

-- Internal exception type that indicates we've responded to the client.
-- TODO make this an async exception so it's less likely to be caught and ignored
newtype Done
  = Done Wai.ResponseReceived
  deriving anyclass (Exception)

instance Show Done where
  show (Done _) = "Done"

------------------------------------------------------------------------------------------------------------------------
-- Router monad

-- | The router monad.
newtype Router a = Router ([Text] -> Maybe (a, [Text]))
  deriving (Alternative, Applicative, Functor, Monad) via StateT [Text] Maybe

runRouter :: [Text] -> Router a -> Maybe (a, [Text])
runRouter x (Router f) =
  f x

-- | Assert that current path segment is equal to the given text, and advance to the next path segment.
theText :: Text -> Router ()
theText s =
  Router \case
    t : ts | s == t -> Just ((), ts)
    _ -> Nothing

-- | Get the current path segment, and advance to the next path segment.
text :: Router Text
text =
  Router \case
    [] -> Nothing
    s : ss -> Just (s, ss)

end :: Router ()
end =
  Router \case
    [] -> Just ((), [])
    _ -> Nothing

------------------------------------------------------------------------------------------------------------------------
-- Resource

-- | A resource.
data Resource m = Resource
  { resourceDelete :: !(Maybe (m Void)),
    resourceGet :: !(Maybe (m Void)),
    resourcePatch :: !(Maybe (m Void)),
    resourcePost :: !(Maybe (m Void)),
    resourcePut :: !(Maybe (m Void))
  }

instance Semigroup (Resource m) where
  Resource a1 b1 c1 d1 e1 <> Resource a2 b2 c2 d2 e2 =
    Resource (a1 <|> a2) (b1 <|> b2) (c1 <|> c2) (d1 <|> d2) (e1 <|> e2)

runResource :: Resource m -> Http.Method -> Maybe (m Void)
runResource Resource {resourceDelete, resourceGet, resourcePatch, resourcePost, resourcePut} method
  | method == Http.methodGet = resourceGet
  | method == Http.methodPost = resourcePost
  | method == Http.methodPut = resourcePut
  | method == Http.methodDelete = resourceDelete
  | method == Http.methodPatch = resourcePatch
  | otherwise = Nothing

-- | @DELETE@ a resource.
delete :: m Void -> Resource m
delete handler =
  Resource
    { resourceDelete = Just handler,
      resourceGet = Nothing,
      resourcePatch = Nothing,
      resourcePost = Nothing,
      resourcePut = Nothing
    }

-- | @GET@ a resource.
get :: m Void -> Resource m
get handler =
  Resource
    { resourceDelete = Nothing,
      resourceGet = Just handler,
      resourcePatch = Nothing,
      resourcePost = Nothing,
      resourcePut = Nothing
    }

-- | @PATCH@ a resource.
patch :: m Void -> Resource m
patch handler =
  Resource
    { resourceDelete = Nothing,
      resourceGet = Nothing,
      resourcePatch = Just handler,
      resourcePost = Nothing,
      resourcePut = Nothing
    }

-- | @POST@ a resource.
post :: m Void -> Resource m
post handler =
  Resource
    { resourceDelete = Nothing,
      resourceGet = Nothing,
      resourcePatch = Nothing,
      resourcePost = Just handler,
      resourcePut = Nothing
    }

-- | @PUT@ a resource.
put :: m Void -> Resource m
put handler =
  Resource
    { resourceDelete = Nothing,
      resourceGet = Nothing,
      resourcePatch = Nothing,
      resourcePost = Nothing,
      resourcePut = Just handler
    }

------------------------------------------------------------------------------------------------------------------------
-- Handler monad

class MonadIO m => MonadHandler m where
  askRequest :: m Request
  askRespond :: m Respond

type Respond =
  Wai.Response -> IO Wai.ResponseReceived

------------------------------------------------------------------------------------------------------------------------
-- Request

data Request = Request
  { body :: !ByteString,
    headers :: ![(CaseInsensitive.CI Text, Text)],
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
        headers =
          map (\(k, v) -> (CaseInsensitive.map Text.decodeUtf8 k, Text.decodeUtf8 v)) (Wai.requestHeaders request),
        id,
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

deriving stock instance (forall x. Eq x => Eq (f x)) => (Eq (P f a))

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
param :: MonadHandler m => Text -> Param a -> m (Maybe a)
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
params :: MonadHandler m => Text -> Param a -> m (Array a)
params name (Param parser) = do
  request <- askRequest
  case lookupParam name request.params of
    Nothing -> pure Array.emptyArray
    Just p ->
      case traverse parser (allParams p) of
        Nothing -> respondBadParameter
        Just result -> pure result

respondBadParameter :: MonadHandler m => m void
respondBadParameter =
  respond (Wai.responseBuilder Http.status400 [] mempty)

------------------------------------------------------------------------------------------------------------------------
-- Response

-- | Respond to the client.
respond :: MonadHandler m => Wai.Response -> m void
respond response = do
  resp <- askRespond
  liftIO do
    sent <- resp response
    throwIO (Done sent)

data FileResponse = FileResponse
  { status :: !Http.Status,
    headers :: ![Http.Header],
    file :: !FilePath
  }

-- | Respond to the client with a file.
respondFile :: MonadHandler m => FileResponse -> m void
respondFile FileResponse {file, headers, status} =
  respond (Wai.responseFile status headers file Nothing)
