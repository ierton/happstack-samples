
{-# OPTIONS_GHC -F -pgmF trhsx #-}

import Happstack.Server
import HSP 
import Data.Char
import HSP.ServerPartT
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Maybe
import System.Log.Logger

-- Link to JQuery source
jQuery = "http://ajax.googleapis.com/ajax/libs/jquery/1.3/jquery.min.js"

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    let cfg = Conf { validator = Nothing, port = 3333 }
    simpleHTTP cfg handlerMap

handlerMap :: ServerPartT IO Response
handlerMap = msum [ 
    dir "_ajax" $ renderAjax, 
    renderHello ] 

renderHello :: ServerPartT IO Response
renderHello = do
    xhello <- unXMLGenT hello
    makeResponse . renderAsHTML $ xhello

renderAjax :: ServerPartT IO Response
renderAjax = do
    makeResponse $ "bla-bla-bal"

makeResponse = ok . setHeader "Content-Type" "text/html" .  toResponse

hello :: XMLGenT (ServerPartT IO) XML
hello =
    <html lang="en">
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
        <script src=(jQuery) type="text/javascript"></script>
        <script type="text/javascript">
            $(document).ready( function() {
                $("#generate").click(function() {
                   $("#quote").load("_ajax");
                });
            });
        </script> 
    </head>
    <body>
        <input type="submit" id="generate" value="Generate!"/>
        <div id="quote"></div>
    </body>
    </html>

