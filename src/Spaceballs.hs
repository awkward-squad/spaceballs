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
    post,

    -- * Handler monad
    MonadHandler (..),
    Respond,

    -- ** Request
    Request (..),
    makeRequest,

    -- *** Query params
    Param,
    ptext,
    param,

    -- ** Response
    respond,
    FileResponse (..),
    respondFile,
  )
where

import Control.Applicative (Alternative)
import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Void (Void, absurd)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Random.Stateful qualified as Random
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
newtype Done
  = Done Wai.ResponseReceived
  deriving anyclass (Exception)

instance Show Done where
  show (Done _) = "Done"

------------------------------------------------------------------------------------------------------------------------
-- Router monad

-- | The router: statefully parse a request path one path segment at a time.
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

newtype Resource m
  = Resource (Http.Method -> Maybe (m Void))

instance Semigroup (Resource m) where
  Resource r1 <> Resource r2 =
    Resource \method ->
      case r1 method of
        Nothing -> r2 method
        Just action -> Just action

runResource :: Resource m -> Http.Method -> Maybe (m Void)
runResource =
  coerce

delete :: m Void -> Resource m
delete action =
  Resource \case
    DELETE -> Just action
    _ -> Nothing

get :: m Void -> Resource m
get action =
  Resource \case
    GET -> Just action
    _ -> Nothing

post :: m Void -> Resource m
post action =
  Resource \case
    POST -> Just action
    _ -> Nothing

pattern DELETE :: Http.Method
pattern DELETE <- ((== Http.methodDelete) -> True)

pattern GET :: Http.Method
pattern GET <- ((== Http.methodGet) -> True)

pattern POST :: Http.Method
pattern POST <- ((== Http.methodPost) -> True)

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
  { body :: ByteString,
    headers :: [(CaseInsensitive.CI Text, Text)],
    id :: Text,
    method :: Http.Method,
    params :: [(Text, Maybe Text)],
    path :: [Text]
  }

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
        params = map (\(k, v) -> (Text.decodeUtf8 k, Text.decodeUtf8 <$> v)) (Wai.queryString request),
        path = Wai.pathInfo request
      }

------------------------------------------------------------------------------------------------------------------------
-- Query params

newtype Param a
  = Param (Text -> Maybe a)
  deriving stock (Functor)

ptext :: Param Text
ptext =
  Param Just

param :: MonadHandler m => Text -> Param a -> m (Maybe a)
param name (Param parser) = do
  request <- askRequest
  case List.lookup name request.params of
    Nothing -> pure Nothing
    Just value ->
      case value >>= parser of
        Nothing -> respond (Wai.responseBuilder Http.status400 [] mempty)
        Just result -> pure (Just result)

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
