Usage example: simple hit counter

```haskell
{-# LANGUAGE OverloadedStrings #-}

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

        html $ mconcat [ "<html><body>"
                       , hits'
                       , "</body></html>"
                       ]
```


