{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, EmptyDataDecls #-}
{-# OPTIONS_GHC -F -pgmF trhsx #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Web.Routes
import Web.Routes.TH
import Web.Routes.Regular
import Web.Routes.XMLGenT
import Web.Routes.Happstack

import Control.Monad.Trans

import Happstack.Server
import HSP 
import HSP.ServerPartT
import HSX.XMLGenerator hiding(XML)


data BlogURL
  = BlogHome
  | BlogPost String
    deriving (Eq, Ord, Read, Show)
             
$(derivePathInfo ''BlogURL)

data SiteURL
  = MyHome
  | MyBlog BlogURL
    deriving (Eq, Ord, Read, Show)
             
$(derivePathInfo ''SiteURL)


main :: IO ()
main = simpleHTTP nullConf {port = 3333}  handlerMap


handlerMap :: ServerPartT IO Response
handlerMap = do
    r <- implSite_ "/" "" site
    case r  of
        Right a -> return a
        Left err -> ok $ toResponse $ err


site :: Site SiteURL (ServerPartT IO Response)
site = setDefault MyHome $ Site {
    handleSite = \f u -> runRouteT (mySite u) f, 
    formatPathSegments = \url -> (toPathSegments url, []),
    parsePathSegments = parseSegments fromPathSegments
    }

mySite :: SiteURL -> RouteT SiteURL (ServerPartT IO) Response
mySite MyHome = 
    unXMLGenT (about >>= page) >>= makeResponse

mySite (MyBlog BlogHome) = 
    unXMLGenT (blog "cats" >>= page) >>= makeResponse

mySite (MyBlog (BlogPost x)) = 
    unXMLGenT (blog x >>= page) >>= makeResponse

makeResponse = ok . setHeader "Content-Type" "text/html" . toResponse . renderAsHTML

-- Patch sent to happstack/hsx team.
instance (EmbedAsAttr m (Attr String c), TypeCastM m1 m) => EmbedAsAttr m (Attr String (XMLGenT m1 c)) where
    asAttr (n := (XMLGenT m1a)) = do
            a <- XMLGenT $ typeCastM m1a
            asAttr (n:=a)

page :: XML -> XMLGenT (RouteT SiteURL (ServerPartT IO)) XML
page content =
    let b1 = toBlog "hello-world" 
        b2 = toBlog "cats" in
    <html>
        <head>
            <title>Holy Homepage</title>
        </head>
        <body>
            <p>Hello there</p>
            This is simple stub blog page. Its content is: <br/>
            <b><% content %></b>
            Here is links example:
            <table border="1">
                <tr>
                    <th> Link       </th>
                    <th> Link URL   </th>
                </tr>
                <tr>
                    <td> <a href=(b1)>link</a> </td>
                    <td> <% b1 %> </td>
                </tr>
                <tr>
                    <td> <a href=(b2)>link</a> </td>
                    <td> <% b2 %> </td>
                </tr>
            </table>
        </body>
    </html>

toBlog :: String -> XMLGenT (RouteT SiteURL (ServerPartT IO)) String
toBlog postname = do
    x <- showURL (MyBlog $ BlogPost postname)
    return x

about :: XMLGenT (RouteT SiteURL (ServerPartT IO)) XML
about = <div>This is start page. When you worry call me, i make you happy.</div>

blog :: String -> XMLGenT (RouteT SiteURL (ServerPartT IO)) XML
blog what = <div>This is kinda blog post about <% what %></div>

