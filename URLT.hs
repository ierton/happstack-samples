{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, EmptyDataDecls #-}
{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Main where

import Control.Applicative(Applicative((<*>),pure), (<$>), (*>))
import Control.Applicative.Error
import Control.Monad.Consumer(next)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time (UTCTime, getCurrentTime)
import Generics.Regular
import URLT.Base
import URLT.PathInfo
import URLT.Happstack
import URLT.TH
import URLT.HandleT
import URLT.XMLGenT
import URLT.Monad
import URLT.Regular
import Text.XHtml.Transitional
import System.FilePath((</>))

import Happstack.Server
import HSP 
import Data.Char
import HSP.ServerPartT

-- NOTE: in these examples the homepage is:
-- http://localhost:3000/MyHome

-- The URL / route types

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

-- the SiteURL route handler Application
-- In this version, the function for converting the URL type to its
-- String representation is passed in as the argument mkAbs.
page :: XMLGenT (URLT SiteURL (ServerPartT IO)) XML
page =
    <html>
     <head>
      <title>Hello</title>
     </head>
     <body>
      <p>Hello</p>
     </body>
    </html>

mySite :: SiteURL -> URLT SiteURL (ServerPartT IO) Response
mySite url = do
    xml <- unXMLGenT page
    makeResponse xml

makeResponse = ok . setHeader "Content-Type" "text/html" . toResponse . renderAsHTML

--     main <- showURL $ MyHome
--     ok $ setHtml $ toResponse $ renderHtml $ toHtmlFromList [
--             header `hwith` [
--                 meta ! [ 
--                     strAttr "http-equiv" "Content-Type", 
--                     strAttr "content" "text/html", 
--                     strAttr "http-equiv" "text/html; charset=UTF-8" 
--                     ],
--                 thetitle << "MyHome"
--             ] ,
--             body `hwith` [
--                 anchor ! [ href $ main ] << "Home sweet home"
--             ]
--         ]



site :: Site SiteURL String (ServerPartT IO Response)
site = Site {
    handleLink = \f u -> runURLT (mySite u) f, 
    defaultPage = MyHome , 
    formatLink = toPathInfo , 
    parseLink = fromPathInfo 
    }


