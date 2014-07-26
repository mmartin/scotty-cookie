{-|

Usage example: simple hit counter

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}

import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Text.Read
import qualified Data.Text.Lazy as TL

import Web.Scotty
import Web.Scotty.Cookie

main :: IO ()
main = scotty 3000 $
    get "/" $ do
        hits <- liftM (fromMaybe "0") $ getCookie "hits"

        let hits' = case decimal hits of
                        Right n -> TL.pack . show . (+1) $ (fst n :: Integer)
                        Left _  -> "1"

        setSimpleCookie "hits" $ TL.toStrict hits'

        html $ mconcat [ "\<html\>\<body\>"
                       , hits'
                       , "\<\/body\>\<\/html\>"
                       ]
@

-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Scotty.Cookie
    ( makeSimpleCookie
    , setCookie
    , setSimpleCookie
    , getCookie
    , getCookies
    , deleteCookie
    ) where

import Control.Monad ( liftM )

import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as BSL

import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import Blaze.ByteString.Builder ( toLazyByteString )

import Web.Scotty
import Web.Cookie


makeSimpleCookie :: TS.Text -- ^ name
                 -> TS.Text -- ^ value
                 -> SetCookie
makeSimpleCookie n v = def { setCookieName  = TS.encodeUtf8 n
                           , setCookieValue = TS.encodeUtf8 v
                           }


setCookie :: SetCookie -> ActionM ()
setCookie c = setHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)


-- | 'makeSimpleCookie' and 'setCookie' combined.
setSimpleCookie :: TS.Text -- ^ name
                -> TS.Text -- ^ value
                -> ActionM ()
setSimpleCookie n v = setCookie $ makeSimpleCookie n v


getCookie :: TS.Text -- ^ name
          -> ActionM (Maybe TS.Text)
getCookie c = liftM (Map.lookup c) getCookies


-- | Returns all cookies
getCookies :: ActionM (Map.Map TS.Text TS.Text)
getCookies = liftM (Map.fromList . maybe [] parse) $ header "Cookie"
    where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8


deleteCookie :: TS.Text -- ^ name
             -> ActionM ()
deleteCookie c = setCookie $ (makeSimpleCookie c "") { setCookieExpires = Just $ posixSecondsToUTCTime 0 }

