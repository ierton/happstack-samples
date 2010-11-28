
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
    dir "_ajax" $ msum [ 
        dir "cell" $ ajaxCell , 
        dir "test" $ ajaxTest 
        ],
    renderHello ] 

renderHello :: ServerPartT IO Response
renderHello = do
    xhello <- unXMLGenT hello
    makeResponse . renderAsHTML $ xhello

ajaxTest :: ServerPartT IO Response
ajaxTest = do
    makeResponse $ "bla-bla-bal"

ajaxCell :: ServerPartT IO Response
ajaxCell = do
    makeResponse $ "#ffaaaa"

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
                    $("#quote").load("_ajax/test/get");
                });

                $("td.mycell").css( "background-color", "#aaaaff" );

                $("td.mycell").click(function() {
                    var _this = this;
                    $.get("_ajax/cell/get", function(x) {
                        _this.style.backgroundColor = x;
                        });
                    });
                });
        </script> 
    </head>
    <body>
        <input type="submit" id="generate" value="Generate!"/>
        <div id="quote"></div>

        <% table 40 40 %>
    </body>
    </html>

cell_id x y = (show x) ++ "-" ++ (show y)

table :: Int -> Int -> XMLGenT (ServerPartT IO) XML
table rows cols = <table border="0" cellspacing="0"> <% forM [0..rows-1] row %> </table>
    where 
        row y = 
            <tr> <% forM [0..cols-1] $ \x -> cell x y %> 
            </tr>
        cell x y  = 
            <td id=(cell_id x y) class="mycell" width="10" height="10"> 
            </td>

