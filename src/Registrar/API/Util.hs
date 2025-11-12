{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.API.Util where

import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (MonadTrans (..))
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as AS
import Data.ByteString.UTF8 qualified as UTF8
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Network.HTTP.Media.RenderHeader qualified as HTTPMedia
import Network.HTTP.Types qualified as HTTP
import Registrar.Prelude (FromJSON, Generic, ToJSON, Type)
import Registrar.Types (PoolSql)
import Servant
import Servant.API.UVerb.Union
import Servant.Server

----------------------- Error formatting ------------------

errorFormatters :: (PoolSql) => Context '[ErrorFormatters]
errorFormatters = (customFormatters :. EmptyContext)

customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = bodyParserErrorFormatter'
    }

bodyParserErrorFormatter' :: ErrorFormatter
bodyParserErrorFormatter' _ _ errMsg =
  ServerError
    { errHTTPCode = HTTP.statusCode HTTP.status400
    , errReasonPhrase = UTF8.toString $ HTTP.statusMessage HTTP.status400
    , errBody =
        AS.encode $
          AS.object
            [ "code" AS..= AS.Number 400
            , "message" AS..= errMsg -- FIXME: need implement error scope for example: { ... "scope": "ReqBody"}
            ]
    , errHeaders = [(HTTP.hContentType, HTTPMedia.renderHeader (Servant.contentType (Proxy @Servant.JSON)))]
    }

----------------------- Throwing api errors ------------------
type UVerbT :: [Type] -> (Type -> Type) -> Type -> Type
newtype UVerbT xs m a = UVerbT {unUVerbT :: ExceptT (Union xs) m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (MonadError e m) => MonadError e (UVerbT xs m) where
  throwError = lift . throwError
  catchError (UVerbT act) h =
    UVerbT $
      ExceptT $
        runExceptT act `catchError` (runExceptT . unUVerbT . h)

{- | This combinator runs 'UVerbT'. It applies 'respond' internally, so the handler
may use the usual 'return'.
-}
runUVerbT :: (Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'UVerbT' computation returning one of the response types.
throwUVerb :: (Monad m, HasStatus x, IsMember x xs) => x -> UVerbT xs m a
throwUVerb = UVerbT . ExceptT . fmap Left . respond

-------------------------- Prelude for http responses --------------------------

type ResponseError :: Type -> Type
data ResponseError a = MKResponseError {error :: a}
  deriving (Eq, Show, Generic)

type BadRequest :: Type
data BadRequest = MkBadRequest {message :: Text}
  deriving (Eq, Show, Generic)

deriving anyclass instance ToJSON BadRequest
deriving anyclass instance FromJSON BadRequest
deriving anyclass instance ToSchema BadRequest
instance HasStatus BadRequest where
  type StatusOf BadRequest = 400

-- | This combinator runs 'throwUVerb' and respond BadRequest with 400 status code
throwBadRequest = undefined -- TODO: Need implement

-- | This combinator runs 'return' with response and status code 201
respond201 :: (Monad m) => a -> m (WithStatus 201 a)
respond201 a = do return $ WithStatus @201 a
