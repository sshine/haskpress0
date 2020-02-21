{-# LANGUAGE TemplateHaskell #-}
module Main where

import           RIO
import           RIO.FilePath ((</>), (<.>), isExtensionOf)
import qualified RIO.Map as Map
import           RIO.Map (Map)
import qualified RIO.Text as Text
import           RIO.Text (Text)
import           RIO.Text.Lazy (toStrict)

import Data.Text.IO (putStrLn)

import Web.Spock
import Web.Spock.Config

import Text.Heterocephalus (compileHtmlFile)
import Text.Blaze (Markup)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import System.Directory (listDirectory)
import System.FSNotify
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Web.HaskPress.BlogPost

data AppSession = EmptySession
data AppState = AppState
  { appStatePosts :: IORef (Map FilePath BlogPost)
  }

type HaskPressM a = ActionT (WebStateM () AppSession AppState) a

postsDir :: FilePath
postsDir = "posts"

main :: IO ()
main = do
  posts <- readBlogPosts
  postsRef <- newIORef posts
  _monitorTid <- forkIO . void . blogPostsMonitor $ postsRef
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState postsRef)
  runSpock 3000 (spock spockCfg app)

app :: SpockM () AppSession AppState ()
app = do
  get root rootPage
  get ("posts" <//> var) viewBlogPostPage

rootPage :: HaskPressM a
rootPage = do
  posts <- getBlogPosts
  html . toStrict . renderHtml . rootTemplate $ posts

rootTemplate :: Map FilePath BlogPost -> Markup
rootTemplate blogPosts =
  $(compileHtmlFile "templates/index.html.tmpl")

viewBlogPostPage :: Text -> HaskPressM a
viewBlogPostPage (Text.unpack -> name) = do
  blogPost <- lookupBlogPost (postsDir </> name <.> "md")
  case blogPost of
    Nothing -> text $ "Post not found!"
    Just content -> html (blogPostContentRendered content)

getBlogPosts :: HaskPressM (Map FilePath BlogPost)
getBlogPosts = do
  AppState postsRef <- getState
  liftIO (readIORef postsRef)

lookupBlogPost :: FilePath -> HaskPressM (Maybe BlogPost)
lookupBlogPost name = Map.lookup name <$> getBlogPosts

readBlogPosts :: MonadIO m => m (Map FilePath BlogPost)
readBlogPosts = do
  filePaths <- liftIO (listDirectory' postsDir)
  let markdownFilePaths = filter (".md" `isExtensionOf`) filePaths
  Map.fromList <$> traverse readBlogPost' markdownFilePaths
  where
    readBlogPost' :: MonadIO m => FilePath -> m (FilePath, BlogPost)
    readBlogPost' filePath = (filePath,) <$> readBlogPost filePath

blogPostsMonitor :: IORef (Map FilePath BlogPost) -> IO StopListening
blogPostsMonitor postsRef =
  withManager $ \mgr -> do
    watchDir mgr postsDir (const True) updatePosts
    forever $ threadDelay 1000000
  where
    updatePosts :: Event -> IO ()
    updatePosts _event = do
      posts <- readBlogPosts
      liftIO (atomicWriteIORef postsRef posts)
      putStrLn "Reloaded posts..."

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' dirPath = do
  files' <- listDirectory dirPath
  pure (map (dirPath </>) files')
