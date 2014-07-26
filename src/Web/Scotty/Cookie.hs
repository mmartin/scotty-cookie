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


makeSimpleCookie :: TS.Text -> TS.Text -> SetCookie
makeSimpleCookie n v = def { setCookieName  = TS.encodeUtf8 n
                           , setCookieValue = TS.encodeUtf8 v
                           }

setCookie :: SetCookie -> ActionM ()
setCookie c = setHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)

setSimpleCookie :: TS.Text -> TS.Text -> ActionM ()
setSimpleCookie n v = setCookie $ makeSimpleCookie n v

getCookie :: TS.Text -> ActionM (Maybe TS.Text)
getCookie c = liftM (Map.lookup c) getCookies

getCookies :: ActionM (Map.Map TS.Text TS.Text)
getCookies = liftM (Map.fromList . maybe [] parse) $ header "Cookie"
    where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8

deleteCookie :: TS.Text -> ActionM ()
deleteCookie c = setCookie $ (makeSimpleCookie c "") { setCookieExpires = Just $ posixSecondsToUTCTime 0 }

