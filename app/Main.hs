module Main where

import           RIO
import           RIO.FilePath ((</>), isExtensionOf)
import qualified RIO.Map as Map
import           RIO.Map (Map)
import qualified RIO.Text as Text
import           RIO.Text (Text)
import           RIO.Time (UTCTime)

import Data.Text.IO (putStrLn)

import CMarkGFM (commonmarkToHtml)

import Web.Spock
import Web.Spock.Config

import System.Directory (listDirectory)
import System.FSNotify
import Control.Monad (forever)
import Control.Concurrent (forkIO)

type BlogTitle = Text
data BlogPost = BlogPost
  { blogPostDate            :: Maybe UTCTime
  , blogPostTitle           :: BlogTitle
  , blogPostContent         :: Text
  , blogPostContentRendered :: Text
  }
  deriving (Eq, Ord, Show)

data AppSession = EmptySession
data AppState = AppState
  { appStatePosts :: IORef (Map BlogTitle BlogPost)
  }

postsDir :: FilePath
postsDir = "posts"

main :: IO ()
main = do
  posts <- readBlogPosts
  postsRef <- newIORef posts
  _monitorTid <- forkIO (void $ blogPostsMonitor postsRef)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState postsRef)
  runSpock 3000 (spock spockCfg app)

app :: SpockM () AppSession AppState ()
app = do
  get root $ do
    AppState postsRef <- getState
    posts <- liftIO (readIORef postsRef)
    html $ mconcat
      [ "<h1>Blog posts</h1>"
      , "<ul>"
      , tshowBlogPosts (Map.elems posts)
      , "</ul>"
      ]

  get ("posts" <//> var) $ \name -> do
    AppState postsRef <- getState
    posts <- liftIO (readIORef postsRef)
    case Map.lookup name posts of
      Nothing -> text "Post not found!"
      Just post -> html (blogPostContentRendered post)

tshowBlogPosts :: [BlogPost] -> Text
tshowBlogPosts = foldMap tshowBlogPost
  where
    tshowBlogPost :: BlogPost -> Text
    tshowBlogPost (BlogPost d t c r) =
      "<li><a href=\"/" <> t <> "\">" <> t <> "</a></li>"

readBlogPost :: MonadIO m => FilePath -> m BlogPost
readBlogPost filePath = do
  c <- readFileUtf8 filePath
  let t = Text.pack filePath
  let r = commonmarkToHtml [] [] c
  pure (BlogPost Nothing t c r)

readBlogPosts :: MonadIO m => m (Map BlogTitle BlogPost)
readBlogPosts = do
  filePaths <- liftIO (listDirectory postsDir)
  let markdownFilePaths = filter (".md" `isExtensionOf`) filePaths
  posts <- traverse (readBlogPost . (postsDir </>)) markdownFilePaths
  let fakeTitles = map Text.pack filePaths
  return (Map.fromList (zip fakeTitles posts))

blogPostsMonitor :: IORef (Map BlogTitle BlogPost) -> IO StopListening
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
