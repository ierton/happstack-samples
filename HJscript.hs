-- Author: Sergey Mironov
-- See also http://blog.ierton.ru

{-# OPTIONS_GHC -F -pgmF trhsx #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

import Happstack.Server
import HSP 
import Data.Char
import HSP.ServerPartT
import HJScript
import HJScript.DOM

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


instance EmbedAsChild (ServerPartT IO) (HJScript ()) where
  asChild script = asChild . snd $ evalHJScript script

instance EmbedAsChild (ServerPartT IO) (Block t) where
  asChild b = asChild $
    <script type="text/javascript">
      <% show b %>
    </script>

js = window # alert (string "an freaking alert box.")

hello :: String -> XMLGenT (ServerPartT IO) XML
hello noun =
    <html>
        <head>
            <title>Hello, <% noun %></title>
            <% js %>
        </head>
        <body>
            <p>Hello, <% map toUpper noun %></p>
        </body>
    </html>


