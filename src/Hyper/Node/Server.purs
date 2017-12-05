module Hyper.Node.Server
       ( HttpRequest
       , HttpResponse
       , NodeResponse
       , writeString
       , write
       , module Hyper.Node.Server.Options
       , runServer
       , runServer'
       ) where

import Prelude

import Control.IxMonad (ipure, (:*>), (:>>=))
import Control.Monad.Aff (Aff, launchAff, launchAff_, makeAff, nonCanceler, runAff_)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Node.Server.Options (Options)
import Hyper.Node.Server.Options as Hyper.Node.Server.Options
import Hyper.Request (class ReadableBody, class Request, class StreamableBody, RequestData, parseUrl, readBody)
import Hyper.Response (class ResponseWritable, class Response, ResponseEnded, StatusLineOpen)
import Hyper.Status (Status(..))
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP)
import Node.HTTP as HTTP
import Node.Stream (Stream, Writable)
import Node.Stream as Stream


data HttpRequest
  = HttpRequest HTTP.Request RequestData


instance requestHttpRequest :: Monad m => Request HttpRequest m where
  getRequestData = do
    getConn :>>=
    case _ of
      { request: HttpRequest _ d } -> ipure d


-- A limited version of Writable () e, with which you can only write, not end,
-- the Stream.
newtype NodeResponse m e
  = NodeResponse (Writable () e -> m Unit)
derive instance newtypeNodeResponse ∷ Newtype (NodeResponse m e) _

writeString :: forall m e. MonadAff e m => Encoding -> String -> NodeResponse m e
writeString enc str = NodeResponse $ \w -> liftAff (makeAff (writeAsAff w))
  where
    writeAsAff w k = do
      Stream.writeString w enc str (k (pure unit)) >>=
      if _
        then k (pure unit)
        else k (throwError (error "Failed to write string to response"))
      pure nonCanceler

write :: forall m e. MonadAff e m => Buffer -> NodeResponse m e
write buffer = NodeResponse $ \w ->
  liftAff (makeAff (\k -> Stream.write w buffer (k (pure unit))
                          *> pure nonCanceler))

instance stringNodeResponse :: (MonadAff e m) => ResponseWritable (NodeResponse m e) m String where
  toResponse = ipure <<< writeString UTF8

instance stringAndEncodingNodeResponse :: (MonadAff e m) => ResponseWritable (NodeResponse m e) m (Tuple String Encoding) where
  toResponse (Tuple body encoding) =
    ipure (writeString encoding body)

instance bufferNodeResponse :: (MonadAff e m)
                                  => ResponseWritable (NodeResponse m e) m Buffer where
  toResponse buf =
    ipure (write buf)

-- Helper function that reads a Stream into a Buffer, and throws error
-- in `Aff` when failed.
readBodyAsBuffer
  :: forall e.
     HttpRequest
  -> Aff (http :: HTTP, avar :: AVAR, buffer :: BUFFER | e) Buffer
readBodyAsBuffer (HttpRequest request _) = do
  let stream = HTTP.requestAsStream request
  bodyResult <- makeEmptyVar
  chunks <- makeVar []
  fillResult <- liftEff $
    catchException (pure <<< Left) (Right <$> fillBody stream chunks bodyResult)
  -- Await the body, or an error.
  body <- takeVar bodyResult
  -- Return the body, if neither `fillResult` nor `body` is a `Left`.
  either throwError pure (fillResult *> body)
  where
    fillBody stream chunks bodyResult = do
      -- Append all chunks to the body buffer.
      Stream.onData stream \chunk ->
        let modification = do
              v <- takeVar chunks
              putVar (v <> [chunk]) chunks
        in void (launchAff modification)
      -- Complete with `Left` on error.
      Stream.onError stream $
        launchAff_ <<< flip putVar bodyResult <<< Left
      -- Complete with `Right` on successful "end" event.
      Stream.onEnd stream $ void $ launchAff $
        takeVar chunks
        >>= concat'
        >>= (pure <<< Right)
        >>= flip putVar bodyResult
    concat' = liftEff <<< Buffer.concat

instance readableBodyHttpRequestString :: (Monad m, MonadAff (http :: HTTP, avar :: AVAR, buffer :: BUFFER | e) m)
                                       => ReadableBody HttpRequest m String where
  readBody =
    readBody :>>= (liftEff <<< Buffer.toString UTF8)

instance readableBodyHttpRequestBuffer :: (Monad m, MonadAff (http :: HTTP, avar :: AVAR, buffer :: BUFFER | e) m)
                                       => ReadableBody HttpRequest m Buffer where
  readBody =
    _.request <$> getConn :>>=
    case _ of
      r -> liftAff (readBodyAsBuffer r)

instance streamableBodyHttpRequestReadable :: MonadAff (http :: HTTP | e) m
                                           => StreamableBody
                                              HttpRequest
                                              m
                                              (Stream (read :: Stream.Read) (http :: HTTP, exception :: EXCEPTION | e)) where
  streamBody =
    _.request <$> getConn :>>=
    case _ of
      HttpRequest request _ -> ipure (HTTP.requestAsStream request)

-- TODO: Make a newtype
data HttpResponse state = HttpResponse HTTP.Response

getWriter ∷ ∀ req res c m rw.
            Monad m ⇒
            Middleware
            m
            (Conn req { writer ∷ rw | res } c)
            (Conn req { writer ∷ rw | res } c)
            rw
getWriter = _.response.writer <$> getConn

setStatus ∷ ∀ req res c m e.
            MonadEff (http ∷ HTTP | e) m
          ⇒ Status
          → HTTP.Response
          → Middleware m (Conn req res c) (Conn req res c) Unit
setStatus (Status { code, reasonPhrase }) r = liftEff do
  HTTP.setStatusCode r code
  HTTP.setStatusMessage r reasonPhrase

writeHeader' ∷ ∀ req res c m e.
               MonadEff (http ∷ HTTP | e) m
             ⇒ (Tuple String String)
             → HTTP.Response
             → Middleware m (Conn req res c) (Conn req res c) Unit
writeHeader' (Tuple name value) r =
  liftEff $ HTTP.setHeader r name value

writeResponse ∷ ∀ req res c m e.
                MonadAff (http ∷ HTTP | e) m
             ⇒ HTTP.Response
             → NodeResponse m (http :: HTTP | e)
             → Middleware m (Conn req res c) (Conn req res c) Unit
writeResponse r (NodeResponse f) =
  lift' (f (HTTP.responseAsStream r))

endResponse ∷ ∀ req res c m e.
              MonadEff (http ∷ HTTP | e) m
            ⇒ HTTP.Response
            → Middleware m (Conn req res c) (Conn req res c) Unit
endResponse r =
  liftEff (Stream.end (HTTP.responseAsStream r) (pure unit))

instance responseWriterHttpResponse :: MonadAff (http ∷ HTTP | e) m
                                    => Response HttpResponse m (NodeResponse m (http :: HTTP | e)) where
  writeStatus status =
    getConn :>>= \{ response: HttpResponse r } ->
      setStatus status r
      :*> modifyConn (_ { response = HttpResponse r })

  writeHeader header =
    getConn :>>= \{ response: HttpResponse r } ->
      writeHeader' header r
      :*> modifyConn (_ { response = HttpResponse r })

  closeHeaders =
    getConn :>>= \{ response: HttpResponse r } ->
      modifyConn (_ { response = HttpResponse r })

  send f =
    getConn :>>= \{ response: HttpResponse r } ->
      writeResponse r f
      :*> modifyConn (_ { response = HttpResponse r })

  end =
    getConn :>>= \{ response: HttpResponse r } ->
      endResponse r
      :*> modifyConn (_ { response = HttpResponse r })


mkHttpRequest :: HTTP.Request -> HttpRequest
mkHttpRequest request =
  HttpRequest request requestData
  where
    headers = HTTP.requestHeaders request
    requestData =
      { url: HTTP.requestURL request
      , parsedUrl: defer \_ -> parseUrl (HTTP.requestURL request)
      , headers: headers
      , method: Method.fromString (HTTP.requestMethod request)
      , contentLength: StrMap.lookup "content-length" headers
                      >>= Int.fromString
      }


runServer'
  :: forall m e c c'
   . Functor m
  => Options e
  -> c
  -> (forall a. m a -> Aff (http :: HTTP | e) a)
  -> Middleware
     m
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Eff (http :: HTTP | e) Unit
runServer' options components runM middleware = do
  server <- HTTP.createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: unwrap options.hostname
                      , backlog: Nothing
                      }
  HTTP.listen server listenOptions (options.onListening options.hostname options.port)
  where
    onRequest ∷ HTTP.Request → HTTP.Response → Eff (http :: HTTP | e) Unit
    onRequest request response =
      let conn = { request: mkHttpRequest request
                 , response: HttpResponse response
                 , components: components
                 }
          callback =
            case _ of
              Left err -> options.onRequestError err
              Right _ -> pure unit
      in conn
         # evalMiddleware middleware
         # runM
         # runAff_ callback

runServer
  :: forall e c c'.
     Options e
  -> c
  -> Middleware
     (Aff (http :: HTTP | e))
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Eff (http :: HTTP | e) Unit
runServer options components middleware =
  runServer' options components id middleware
