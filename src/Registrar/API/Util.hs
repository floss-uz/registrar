module Registrar.API.Util where

import Data.Aeson qualified as AS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Text (Text)
import Network.HTTP.Media.RenderHeader qualified as HTTPMedia
import Network.HTTP.Types qualified as HTTP
import Registrar.Types (PoolSql)
import Servant
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
