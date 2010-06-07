-- Author: Sergey Mironov
-- See also http://blog.ierton.ru

{-# OPTIONS_GHC -F -pgmF trhsx #-}

import Happstack.Server
import HSP 
import Data.Char
import HSP.ServerPartT

main :: IO ()
main = do
  let cfg = Conf { validator = Nothing, port = 3333 }
  simpleHTTP cfg handlerMap

handlerMap :: ServerPartT IO Response
handlerMap = do
  xhello <- unXMLGenT (hello "World")
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
