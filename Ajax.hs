
{-# OPTIONS_GHC -F -pgmF trhsx #-}

{-# OPTIONS_GHC
 -XTemplateHaskell
 -XFlexibleInstances
 -XMultiParamTypeClasses
 -XFlexibleContexts
 -XUndecidableInstances #-}

import Happstack.Server
import HSP 
import Data.Char
import HSP.ServerPartT
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Maybe
import System.Log.Logger
import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Control.Concurrent
import System.Posix.Unistd

data AjaxPacket = AjaxPacket { 
    x :: Int,
    y :: Int, 
    color :: String 
    } deriving(Show)

$(derive[''AjaxPacket])

samplePacket :: AjaxPacket 
samplePacket = AjaxPacket 3 3 "#ffaaaa"

-- Link to JQuery source
jQuery = "_static/jquery-1.3.min.js"

data Pipe a = Pipe {
    fromUI :: Chan a,
    toUI :: Chan a
    }

newPipe = do
    a <- newChan
    b <- newChan
    return $ Pipe a b

mainProgram p = looper p 0
    where
        looper p c = do
            let ap = AjaxPacket c c "#ffaaaa" 
            writeChan (toUI p) ap
            sleep 1
            looper p (c+1)

main :: IO ()
main = do
    p <- newPipe
    forkIO (mainProgram p)
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    let cfg = Conf { validator = Nothing, port = 3333, logAccess = Nothing }
    simpleHTTP cfg (handlerMap p)

handlerMap :: Pipe AjaxPacket -> ServerPartT IO Response
handlerMap p = msum [ 
    dir "_static" $ uriRest staticFiles,
    dir "_ajax" $ msum [ 
        dir "cell" $ ajaxCell,
        dir "json" $ ajaxJSON p,
        dir "test" $ ajaxTest 
        ],
    renderHello ]

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

staticFiles :: String -> ServerPartT IO Response
staticFiles p = do
    serveDirectory DisableBrowsing [] "."

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

ajaxJSON :: Pipe AjaxPacket -> ServerPartT IO Response
ajaxJSON p = do
    c <- liftIO $ readChan (toUI p)
    makeResponse $ toJsonString c

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

                function process(elem, x) {
                    elem.style.backgroundColor = x;
                }

                function request(elem) {
                    $.get("_ajax/cell/get", function(x) { process(elem, x); } );
                }

                function processJSON(x) {
                    var id = "#" + x.x + "_" + x.y
                    $(id).css("backgroundColor", x.color);
                }

                function requestJSON() {
                    $.getJSON("_ajax/json/get", processJSON );
                }

                $("td.mycell").click( function() { requestJSON(); } );

                });
        </script> 
    </head>
    <body>
        <input type="submit" id="generate" value="Generate!"/>
        <div id="quote"></div>

        <% table 40 40 %>
    </body>
    </html>

cell_id x y = (show x) ++ "_" ++ (show y)

table :: Int -> Int -> XMLGenT (ServerPartT IO) XML
table rows cols = 
    <table border="0" cellspacing="0"> 
        <% forM [0..rows-1] row %> 
    </table>
    where 
        row y = 
            <tr> <% forM [0..cols-1] $ \x -> cell x y %> 
            </tr>
        cell x y  = 
            <td id=(cell_id x y) class="mycell" width="10" height="10"> 
            </td>

