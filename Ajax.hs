
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
    makeHtmlResponse xhello

renderAjax :: ServerPartT IO Response
renderAjax = makeAjaxResponse "bla-bla-bal"

makeHtmlResponse = ok . setHeader "Content-Type" "text/html" .  toResponse . renderAsHTML
makeAjaxResponse = ok . setHeader "Content-Type" "text/html" .  toResponse

errMsg :: String
errMsg = "As you can see, the link no longer tooks you to jquery.com"

urlTarget = "http://ajax.googleapis.com/ajax/libs/jquery/1.3/jquery.min.js"

hello :: XMLGenT (ServerPartT IO) XML
hello =
    <html lang="en">
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
        <script src=(urlTarget) type="text/javascript"></script>
        <script type="text/javascript">
            $(document).ready( function() {
                $("a").click(function(event) {
                    alert("<% errMsg %>");
                    event.preventDefault();
                    $("a").addClass("test");
                });
                $("#generate").click(function() {
                   $("#quote").load("_ajax/tellme.php");
                });
            });
        </script> 
        <style type="text/css">
            a.test { font-weight: bold; }
        </style>
    </head>
    <body>
        <a href="http://jquery.com/">Click me</a>
        <input type="submit" id="generate" value="Generate!"/>
        <div id="quote"></div>
    </body>
    </html>

