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
    get \"/\" $ do
        hits <- liftM (fromMaybe \"0\") $ getCookie \"hits\"
        let hits' = case decimal hits of
                        Right n -> TL.pack . show . (+1) $ (fst n :: Integer)
                        Left _  -> \"1\"
        setSimpleCookie \"hits\" $ TL.toStrict hits'
        html $ mconcat [ \"\<html\>\<body\>\"
                       , hits'
                       , \"\<\/body\>\<\/html\>\"
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

import Web.Scotty.Trans
import Web.Cookie


makeSimpleCookie :: TS.Text -- ^ name
                 -> TS.Text -- ^ value
                 -> SetCookie
makeSimpleCookie n v = def { setCookieName  = TS.encodeUtf8 n
                           , setCookieValue = TS.encodeUtf8 v
                           }


setCookie :: (Monad m, ScottyError e)
          => SetCookie
          -> ActionT e m ()
setCookie c = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)


-- | 'makeSimpleCookie' and 'setCookie' combined.
setSimpleCookie :: (Monad m, ScottyError e)
                => TS.Text -- ^ name
                -> TS.Text -- ^ value
                -> ActionT e m ()
setSimpleCookie n v = setCookie $ makeSimpleCookie n v


getCookie :: (Monad m, ScottyError e)
          => TS.Text -- ^ name
          -> ActionT e m (Maybe TS.Text)
getCookie c = liftM (Map.lookup c) getCookies


-- | Returns all cookies
getCookies :: (Monad m, ScottyError e)
           => ActionT e m (Map.Map TS.Text TS.Text)
getCookies = liftM (Map.fromList . maybe [] parse) $ header "Cookie"
    where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8


deleteCookie :: (Monad m, ScottyError e)
             => TS.Text -- ^ name
             -> ActionT e m ()
deleteCookie c = setCookie $ (makeSimpleCookie c "") { setCookieExpires = Just $ posixSecondsToUTCTime 0 }

