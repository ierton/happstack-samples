
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
    xhello <- unXMLGenT hello
    makeResponse xhello


makeResponse = ok . setHeader "Content-Type" "text/html" .  toResponse . renderAsHTML


errMsg :: String
errMsg = "As you can see, the link no longer took you to jquery.com"

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
            });
        </script> 
        <style type="text/css">
            a.test { font-weight: bold; }
        </style>
    </head>
    <body>
        <a href="http://jquery.com/">Click me</a>
    </body>
    </html>

