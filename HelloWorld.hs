-- Author: Sergey Mironov
-- See also http://blog.ierton.ru

{-# OPTIONS_GHC -F -pgmF trhsx #-}

import Happstack.Server
import HSP 
import Data.Char
import HSP.ServerPartT

main :: IO ()
main = simpleHTTP (nullConf { port = 3333})  handlerMap

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
