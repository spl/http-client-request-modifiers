module Main (main) where

--------------------------------------------------------------------------------

import Control.Arrow
import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Network.HTTP.Client
import Network.HTTP.Media
import Network.HTTP.Types
import Network.URI (URI, parseURI, parseRelativeReference)
import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP.Client.Request.Modifiers

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "tests"
  [ setUri_tests
  , setUriRelative_tests
  , setQueryBS_tests
  , setQuery_tests
  , addQuery_tests
  , addQueryPair_tests
  , setMethodBS_tests
  , setMethod_tests
  , setHeaders_tests
  , setHeader_tests
  , addHeader_tests
  , setContentTypeHeader_tests
  , setAcceptHeader_tests
  , setBodyBS_tests
  , setBodyLBS_tests
  , setUrlEncodedBody_tests
  ]

--------------------------------------------------------------------------------

match :: Monad m => (Request -> Bool) -> Request -> m ()
match cond req
  | cond req = return ()
  | otherwise = fail $ "Failed to match request: " ++ show req

--------------------------------------------------------------------------------

exampleUri :: URI
Just exampleUri = parseURI "http://host:1234/path?query=string"

setUri_tests :: TestTree
setUri_tests = testGroup "setUri"
  [ testCase "URI matches expected values" $ return def
    >>= setUri exampleUri
    >>= match (\req ->
      not (secure req) && host req == "host" && port req == 1234
      && path req == "/path" && queryString req == "?query=string")
  ]

exampleRelativeUri :: URI
Just exampleRelativeUri = parseRelativeReference "newpath"

setUriRelative_tests :: TestTree
setUriRelative_tests = testGroup "setUriRelative"
  [ testCase "URI matches expected values, new path" $ return def
    >>= setUri exampleUri
    >>= setUriRelative exampleRelativeUri
    >>= match (\req ->
      not (secure req) && host req == "host" && port req == 1234
      && path req == "/newpath" && queryString req == "")
  ]

--------------------------------------------------------------------------------

setQueryBS_tests :: TestTree
setQueryBS_tests = testGroup "setQueryBS"
  [ testCase "Query string matches expected value" $ return def
    >>= setQueryBS "blah"
    >>= match (\req -> queryString req == "blah")
  ]

setQuery_tests :: TestTree
setQuery_tests = testGroup "setQuery"
  [ testCase "Query string matches expected value" $ return def
    >>= setQuery [("a" :: ByteString, Just "b" :: Maybe ByteString)]
    >>= match (\req -> queryString req == "?a=b")
  ]

addQuery_tests :: TestTree
addQuery_tests = testGroup "addQuery"
  [ testCase "New query follows old" $ return def
    >>= setQueryBS "?a=b"
    >>= addQuery [("c" :: ByteString, Nothing :: Maybe ByteString)]
    >>= match (\req -> queryString req == "?a=b&c")
  , testCase "New query is only query" $ return def
    >>= addQuery [("a" :: ByteString, "b" :: ByteString)]
    >>= match (\req -> queryString req == "?a=b")
  ]

addQueryPair_tests :: TestTree
addQueryPair_tests = testGroup "addQueryPair"
  [ testCase "Query string matches expected value" $ return def
    >>= setQueryBS "?a=b"
    >>= addQueryPair ("c" :: ByteString) ("d" :: ByteString)
    >>= match (\req -> queryString req == "?a=b&c=d")
  ]

--------------------------------------------------------------------------------

setMethodBS_tests :: TestTree
setMethodBS_tests = testGroup "setMethodBS"
  [ testCase "Method matches expected value" $ return def
    >>= setMethodBS "blah"
    >>= match (\req -> method req == "blah")
  ]

setMethod_tests :: TestTree
setMethod_tests = testGroup "setMethod"
  [ testCase "Method matches expected value" $ return def
    >>= setMethod HEAD
    >>= match (\req -> method req == "HEAD")
  ]

--------------------------------------------------------------------------------

exampleHeaders :: RequestHeaders
exampleHeaders = [(hAcceptLanguage, "hello"), (hContentEncoding, "goodbye")]

setHeaders_tests :: TestTree
setHeaders_tests = testGroup "setHeaders"
  [ testCase "Headers match expected values" $ return def
    >>= setHeaders exampleHeaders
    >>= match (\req -> requestHeaders req == exampleHeaders)
  ]

setHeader_tests :: TestTree
setHeader_tests = testGroup "setHeader"
  [ testCase "Header matches expected value" $ return def
    >>= setHeader "myheader" "myheadervalue"
    >>= match (\req -> requestHeaders req == [("myheader", "myheadervalue")])
  , testCase "New header replaces old" $ return def
    >>= setHeaders exampleHeaders
    >>= setHeader hAcceptLanguage "somethingElse"
    >>= match (\req -> requestHeaders req ==
      [(hContentEncoding, "goodbye"), (hAcceptLanguage, "somethingElse")])
  ]

addHeader_tests :: TestTree
addHeader_tests = testGroup "addHeader"
  [ testCase "New header matches expected value" $ return def
    >>= addHeader "myheader" "myheadervalue"
    >>= match (\req -> requestHeaders req == [("myheader", "myheadervalue")])
  , testCase "New header follows old" $ return def
    >>= setHeaders exampleHeaders
    >>= addHeader "myheader" "myheadervalue"
    >>= match (\req -> requestHeaders req == exampleHeaders
      ++ [("myheader", "myheadervalue")])
  ]

setXHeader_tests
  :: String
  -> HeaderName
  -> (forall m. Monad m => MediaType -> ReqMod m)
  -> TestTree
setXHeader_tests name hdrName f = testGroup ("set" ++ name ++ "Header")
  [ testCase "Header matches expected value" $ return def
    >>= f ("application" // "json")
    >>= match (\req -> requestHeaders req == [(hdrName, "application/json")])
  , testCase "New header replaces old" $ return def
    >>= f ("application" // "json")
    >>= f ("text" // "html")
    >>= match (\req -> requestHeaders req == [(hdrName, "text/html")])
  ]

setContentTypeHeader_tests :: TestTree
setContentTypeHeader_tests =
  setXHeader_tests "ContentType" hContentType setContentTypeHeader

setAcceptHeader_tests :: TestTree
setAcceptHeader_tests = setXHeader_tests "Accept" hAccept setAcceptHeader

--------------------------------------------------------------------------------

-- There are no tests for setBody because setBodyBS and setBodyLBS both use
-- setBody.

setBodyBS_tests :: TestTree
setBodyBS_tests = testGroup "setBodyBS"
  [ testCase "Body matches expected value" $ return def
    >>= setBodyBS "mybody"
    >>= match (requestBody >>> \case
      RequestBodyBS body | body == "mybody" -> True
      _ -> False)
  ]

setBodyLBS_tests :: TestTree
setBodyLBS_tests = testGroup "setBodyLBS"
  [ testCase "Body matches expected value" $ return def
    >>= setBodyLBS "mybody"
    >>= match (requestBody >>> \case
      RequestBodyLBS body | body == "mybody" -> True
      _ -> False)
  ]

setUrlEncodedBody_tests :: TestTree
setUrlEncodedBody_tests = testGroup "setUrlEncodedBody"
  [ testCase "Request matches expected value" $ return def
    >>= setUrlEncodedBody [("a", "b"), ("c", " d")]
    >>= match (\req -> case requestBody req of
      RequestBodyLBS body | body == "a=b&c=%20d" ->
        requestHeaders req == [(hContentType, "application/x-www-form-urlencoded")]
        && method req == "POST"
      _ -> False)
  ]
