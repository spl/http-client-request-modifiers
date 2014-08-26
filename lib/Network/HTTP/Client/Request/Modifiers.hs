{-|
Module      : Network.HTTP.Client.Request.Modifiers
Description : Convenient monadic HTTP request modifiers
Copyright   : (c) 2014 Sean Leather
License     : BSD3
Maintainer  : sean.leather@gmail.com
Stability   : experimental

Each of the functions in this module is a monadic request modifier, using the
'ReqMod' type. Most of them do not have any side effects; however, the
consistent use of 'Monad' allows for easy chaining with bind ('>>=') or Kleisli
composition ('>=>').

== Example

The example that inspired this package is modifying the 'Request' from
'parseUrl':

@
  'parseUrl' \"http:\/\/httpbin.org\/post\"
    >>= 'setMethod' 'POST'
    >>= 'setBodyLBS' "hello"
@

Suppose I want to reuse the URL post request but not the body. I can define a
function for just that part:

@
  let httpbinPost :: 'MonadThrow' m => m 'Request'
      httpbinPost = 'parseUrl' \"http:\/\/httpbin.org\/post\" >>= 'setMethod' 'POST'
@

Alternative formulations of the above, without using request modifiers, are:

@
  'parseUrl' \"http:\/\/httpbin.org\/post\"
    >>= \req -> return $ req
      { 'method' = 'renderStdMethod' 'POST'
      , 'requestBody' = 'RequestBodyLBS' "hello"
      }
@

and

@
  let httpbinPost :: 'MonadThrow' m => m 'Request'
      httpbinPost = do req <- 'parseUrl' \"http:\/\/httpbin.org\/post\"
                       return $ req { 'method' = 'renderStdMethod' 'POST' }
@

== Benefits

The main benefits of monadic request modifiers are:

  * composability,
  * conciseness, and
  * allowing an arbitrary combination of 'Monad's.

== Naming Scheme

The naming scheme used for functions in this module is:

  * @set@ - Set a value, overriding any existing value.
  * @add@ - Append a value to the end of a list and do not override any existing
    values.
  * @BS@ - Use a strict 'BS.ByteString' as a parameter.
  * @LBS@ - Use a lazy 'BS.ByteString' as a parameter.
-}
module Network.HTTP.Client.Request.Modifiers (
  -- * Request Modifier Type
    ReqMod
  -- * URI/URL
  , setUri
  , setUriRelative
  -- * Query String
  , setQueryBS
  , setQuery
  , addQuery
  , addQueryPair
  -- * Method
  , setMethodBS
  , setMethod
  -- * Headers
  , setHeaders
  , setHeader
  , addHeaders
  , addHeader
  , setContentTypeHeader
  , setAcceptHeader
  -- * Body
  , setBody
  , setBodyBS
  , setBodyLBS
  , setUrlEncodedBody
  -- * Convenient Combinations
  , setSimpleRequestBS
  , setSimpleRequestLBS
  ) where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import qualified Network.HTTP.Client.Internal as I
import Network.HTTP.Media.MediaType
import Network.HTTP.Types
import Network.HTTP.Types.QueryLike
import Network.URI (URI)

--------------------------------------------------------------------------------

-- | Request modifier, abbreviated
--
-- Since 0.1
type ReqMod m = Request -> m Request

--------------------------------------------------------------------------------

-- | Validate and set the request URI.
--
-- Since 0.1
setUri :: MonadThrow m => URI -> ReqMod m
setUri = flip I.setUri

-- | Extend the request URI with a relative URI.
--
-- Since 0.1
setUriRelative :: MonadThrow m => URI -> ReqMod m
setUriRelative = flip I.setUriRelative

--------------------------------------------------------------------------------

-- | Set the query string with a strict 'BS.ByteString'.
--
-- Since 0.1
setQueryBS :: Monad m => BS.ByteString -> ReqMod m
setQueryBS q req = return $ req { queryString = q }

-- | Set the query string with a rendered 'Query'.
--
-- Since 0.1
setQuery :: (Monad m, QueryLike q) => q -> ReqMod m
setQuery = setQueryBS . renderQuery True . toQuery

-- | Add a rendered 'Query' to the end of the query string.
--
-- Since 0.1
addQuery :: (Monad m, QueryLike q) => q -> ReqMod m
addQuery q req = setQuery (parseQuery (queryString req) ++ toQuery q) req

-- | Add a single query key/value pair to the end of the query string.
--
-- Since 0.1
addQueryPair :: (Monad m, QueryKeyLike k, QueryValueLike v) => k -> v -> ReqMod m
addQueryPair k v = addQuery [(toQueryKey k, toQueryValue v)]

--------------------------------------------------------------------------------

-- | Set the method with a strict 'BS.ByteString'.
--
-- See "Network.HTTP.Types.Method" for the methods, e.g. 'methodGet' or
-- 'methodPost'.
--
-- Since 0.1
setMethodBS :: Monad m => Method -> ReqMod m
setMethodBS m req = return $ req { method = m }

-- | Set the method with a standard method, e.g. 'GET' or 'POST'.
--
-- Since 0.1
setMethod :: Monad m => StdMethod -> ReqMod m
setMethod = setMethodBS . renderStdMethod

--------------------------------------------------------------------------------

-- | Set the request headers.
--
-- Since 0.1
setHeaders :: Monad m => RequestHeaders -> ReqMod m
setHeaders hs req = return $ req { requestHeaders = hs }

-- | Set the request header by name, removing any other headers with the same
-- name.
--
-- Since 0.1
setHeader :: Monad m => HeaderName -> BS.ByteString -> ReqMod m
setHeader n v req =
  setHeaders (filter ((/= n) . fst) (requestHeaders req) ++ [(n, v)]) req

-- | Add headers to the request.
--
-- Since 0.1
addHeaders :: Monad m => RequestHeaders -> ReqMod m
addHeaders hs req = setHeaders (requestHeaders req ++ hs) req

-- | Add a single header.
--
-- Since 0.1
addHeader :: Monad m => HeaderName -> BS.ByteString -> ReqMod m
addHeader n v = addHeaders [(n, v)]

-- | Set the @Content-Type@ header with a 'MediaType'.
--
-- Since 0.1
setContentTypeHeader :: Monad m => MediaType -> ReqMod m
setContentTypeHeader = setHeader hContentType . toByteString

-- | Set the @Accept@ header with a 'MediaType'.
--
-- Since 0.1
setAcceptHeader :: Monad m => MediaType -> ReqMod m
setAcceptHeader = setHeader hAccept . toByteString

--------------------------------------------------------------------------------

-- | Set the request body.
--
-- Since 0.1
setBody :: Monad m => RequestBody -> Request -> m Request
setBody b req = return $ req { requestBody = b }

-- | Set the request body with a strict 'BS.ByteString'.
--
-- Since 0.1
setBodyBS :: Monad m => BS.ByteString -> Request -> m Request
setBodyBS = setBody . RequestBodyBS

-- | Set the request body with a lazy 'LBS.ByteString'.
--
-- Since 0.1
setBodyLBS :: Monad m => LBS.ByteString -> Request -> m Request
setBodyLBS = setBody . RequestBodyLBS

-- | Set the request body with URL-encoded key/value pairs, the method to
-- @POST@, and the @Content-Type@ to @application/x-www-form-urlencoded@.
--
-- Since 0.1
setUrlEncodedBody :: Monad m => [(BS.ByteString, BS.ByteString)] -> ReqMod m
setUrlEncodedBody b = return . urlEncodedBody b

--------------------------------------------------------------------------------

-- | Set the method, @Content-Type@, and strict 'BS.ByteString' body.
--
-- Since 0.1
setSimpleRequestBS :: Monad m => StdMethod -> MediaType -> BS.ByteString -> ReqMod m
setSimpleRequestBS m c b = setMethod m >=> setContentTypeHeader c >=> setBodyBS b

-- | Set the method, @Content-Type@, and lazy 'LBS.ByteString' body.
--
-- Since 0.1
setSimpleRequestLBS :: Monad m => StdMethod -> MediaType -> LBS.ByteString -> ReqMod m
setSimpleRequestLBS m c b = setMethod m >=> setContentTypeHeader c >=> setBodyLBS b
