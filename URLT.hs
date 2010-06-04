-- Note: URLT module is deprecated. Use web-routes instead.
-- Sergey Mironov

{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, EmptyDataDecls #-}
{-# OPTIONS_GHC -F -pgmF trhsx #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import URLT.PathInfo
import URLT.Happstack
import URLT.TH
import URLT.HandleT
import URLT.XMLGenT
import URLT.Monad
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
main = do
    let cfg = Conf { validator = Nothing, port = 3333 }
    simpleHTTP cfg handlerMap


handlerMap :: ServerPartT IO Response
handlerMap = do
    implSite "" "" site


site :: Site SiteURL String (ServerPartT IO Response)
site = Site {
    handleLink = \f u -> runURLT (mySite u) f, 
    defaultPage = MyHome , 
    formatLink = toPathInfo , 
    parseLink = fromPathInfo 
    }

mySite :: SiteURL -> URLT SiteURL (ServerPartT IO) Response
mySite MyHome = unXMLGenT (about >>= page) >>= makeResponse
mySite (MyBlog BlogHome) = unXMLGenT (blog "cats" >>= page) >>= makeResponse
mySite (MyBlog (BlogPost x)) = unXMLGenT (blog x >>= page) >>= makeResponse

makeResponse = ok . setHeader "Content-Type" "text/html" . toResponse . renderAsHTML

-- ISSUE_1: Should one include this in HSX?
-- One should. Willbe done. Remove me when it happens.
instance (EmbedAsAttr m (Attr String c), TypeCastM m1 m) => EmbedAsAttr m (Attr String (XMLGenT m1 c)) where
    asAttr (n := (XMLGenT m1a)) = do
            a <- XMLGenT $ typeCastM m1a
            asAttr (n:=a)

page :: XML -> XMLGenT (URLT SiteURL (ServerPartT IO)) XML
page content =
    let b1 = toBlog "hello-world" 
        b2 = toBlog "cats" in
    <html>
     <head>
      <title>Holy Homepage</title>
     </head>
     <body>
      <p>Hello there</p>
      <% content %>
      <hr/>
      End of content <br/>
      Next link should bring you on: <% b1 %> <br/>
      <a href=(b1)>A link</a> <br/>
      And this - on <%b2%> <br/>
      <a href=(b2)>Another link</a> <br/>
     </body>
    </html>

toBlog :: String -> XMLGenT (URLT SiteURL (ServerPartT IO)) String
toBlog postname = do
    x <- showURL (MyBlog $ BlogPost postname)
    return x

about :: XMLGenT (URLT SiteURL (ServerPartT IO)) XML
about = <div>When you worry call me, i make you happy.</div>

blog :: String -> XMLGenT (URLT SiteURL (ServerPartT IO)) XML
blog what = <div>This is kinda blog post about <% what %></div>

-- Just an example of another monad. Never called.
page' :: XMLGenT (ServerPartT IO) XML
page' = 
    <html>
     <head>
      <title>Holy Homepage</title>
     </head>
     <body>
      <p>Hello there</p>
      <% nn %>
      <a href=(nn)>Blog</a>
     </body>
    </html>

nn :: XMLGenT (ServerPartT IO) String
nn = undefined
