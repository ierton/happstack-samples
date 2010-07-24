-- Author: Sergey Mironov
-- See also http://blog.ierton.ru

{-# OPTIONS_GHC -F -pgmF trhsx #-}

import Happstack.Server
import HSP 
import Data.Char
import Data.Maybe
import HSP.ServerPartT
import HSP.Google.Analytics
import System.Environment

usage = "usage: GoogleAnalytics UACCID"

check (Just s) = return s
check (Nothing) = fail usage

main :: IO ()
main = do
    uacct <- getArgs >>= return . listToMaybe >>= check 
    let cfg = Conf { validator = Nothing, port = 3333 }
    simpleHTTP cfg (handlerMap $ UACCT uacct)

handlerMap :: UACCT -> ServerPartT IO Response
handlerMap uacct = do
    xhello <- unXMLGenT (addAnalytics uacct $ hello "World")
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
