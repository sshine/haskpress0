module Main where

import           RIO
import           RIO.FilePath ((</>))
import qualified RIO.Map as Map
import           RIO.Map (Map)
import qualified RIO.Text as Text
import           RIO.Text (Text)
import           RIO.Time (UTCTime)

import Web.Spock
import Web.Spock.Config

import System.Directory (listDirectory)

type BlogTitle = Text
data BlogPost = BlogPost
  { blogPostDate    :: Maybe UTCTime
  , blogPostTitle   :: BlogTitle
  , blogPostContent :: Text
  }
  deriving (Eq, Ord, Show)

data AppSession = EmptySession
data AppState = AppState
  { appStatePosts :: Map Text BlogPost
  }

postsDir :: FilePath
postsDir = "posts"

main :: IO ()
main = do
  posts <- readBlogPosts
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState posts)
  runSpock 3000 (spock spockCfg app)

app :: SpockM () AppSession AppState ()
app = do
  get root $ do
    AppState posts <- getState
    html $ mconcat
      [ "<h1>Blog posts</h1>"
      , "<ul>"
      , tshowBlogPosts (Map.elems posts)
      , "</ul>"
      ]

  get ("posts" <//> var) $ \name -> do
    AppState posts <- getState
    case Map.lookup name posts of
      Nothing -> text "Post not found!"
      Just post -> text (tshow post)

tshowBlogPosts :: [BlogPost] -> Text
tshowBlogPosts = foldMap tshowBlogPost
  where
    tshowBlogPost :: BlogPost -> Text
    tshowBlogPost (BlogPost d t c) =
      "<li><a href=\"/" <> t <> "\">" <> t <> "</a></li>"

readBlogPost :: MonadIO m => FilePath -> m BlogPost
readBlogPost filePath = do
  content <- readFileUtf8 filePath
  let title = Text.pack filePath
  pure (BlogPost Nothing title content)

readBlogPosts :: MonadIO m => m (Map BlogTitle BlogPost)
readBlogPosts = do
  filePaths <- liftIO (listDirectory postsDir)
  posts <- traverse (readBlogPost . (postsDir </>)) filePaths
  let fakeTitles = map Text.pack filePaths
  return (Map.fromList (zip fakeTitles posts))
