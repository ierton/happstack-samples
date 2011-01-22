-- Author: Sergey Mironov

{-# OPTIONS_GHC -F -pgmF trhsx #-}

import Happstack.Server
import HSP 
import Data.Char
import HSP.ServerPartT
import Control.Monad
import System.Log.Logger

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    simpleHTTP nullConf { port = 3333 }  handlerMap

handlerMap :: ServerPartT IO Response
handlerMap = msum [ tryCookie, noCookie ] >>= renderPage

cookie = "cookie2"

tryCookie =  lookCookie cookie >>= return . Just . show

noCookie = addCookie Session (mkCookie cookie "haba-haba") >> return Nothing

renderPage :: Maybe String -> ServerPartT IO Response
renderPage var = do
    xhello <- unXMLGenT (hello $ show var)
    makeResponse xhello

makeResponse = ok . setHeader "Content-Type" "text/html" . 
  toResponse . renderAsHTML


hello :: String -> XMLGenT (ServerPartT IO) XML
hello noun =
    <html>
     <head>
      <title>Hello, <% noun %></title>
     </head>
     <body>
      <p>Hello, <% map toUpper noun %></p>
     </body>
    </html>


