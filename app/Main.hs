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
  get root postsOverview
  get ("posts" <//> var) postView

postView :: Text -> HaskPressM a
postView (Text.unpack -> name) = do
  blogPost <- lookupBlogPost (postsDir </> name)
  case blogPost of
    Nothing -> text $ "Post not found!"
    Just content -> html (blogPostContentRendered content)

postsOverview :: HaskPressM a
postsOverview = do
  posts <- getBlogPosts
  html $ mconcat
    [ "<h1>Blog posts</h1>"
    , htmlBlogPosts posts
    ]

htmlBlogPosts :: Map FilePath BlogPost -> Text
htmlBlogPosts =
  ("<ul>" <>) . (<> "</ul>") . foldMap f . Map.toList
  where
    f :: (FilePath, BlogPost) -> Text
    f (filePath, blogPost @ BlogPost{..}) = mconcat
      [ "<li><a href=\"/", Text.pack filePath, "\">"
      , blogPostTitle
      , "</a> "
      , showBlogPostDate blogPostDate
      , "</li>"
      ]

getBlogPosts :: HaskPressM (Map FilePath BlogPost)
getBlogPosts = do
  AppState postsRef <- getState
  liftIO (readIORef postsRef)

lookupBlogPost :: FilePath -> HaskPressM (Maybe BlogPost)
lookupBlogPost name = Map.lookup name <$> getBlogPosts

readBlogPost :: MonadIO m => FilePath -> m BlogPost
readBlogPost filePath = do
  blogPostContent <- readFileUtf8 filePath
  let blogPostDate = readDateFromFilePath filePath
      blogPostTitle = readTitleFromContent blogPostContent
      blogPostContentRendered = commonmarkToHtml [] [] blogPostContent
  pure BlogPost{..}

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
