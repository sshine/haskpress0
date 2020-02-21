
module Web.HaskPress.BlogPost where

import           RIO
import           RIO.List
import qualified RIO.Text as Text
import           RIO.Text (Text)
import           RIO.Time (UTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)
import           RIO.FilePath (takeBaseName)

import CMarkGFM (commonmarkToHtml)

import qualified Text.Blaze as Blaze
import           Text.Blaze (ToMarkup)

type BlogTitle = Text
newtype BlogDate = BlogDate { unBlogDate :: Maybe UTCTime }
  deriving (Eq, Ord, Show)

data BlogPost = BlogPost
  { blogPostSlug            :: Text
  , blogPostDate            :: BlogDate
  , blogPostTitle           :: BlogTitle
  , blogPostContent         :: Text
  , blogPostContentRendered :: Text
  } deriving (Eq, Ord, Show)

instance ToMarkup BlogDate where
  toMarkup (BlogDate Nothing) = Blaze.text ""
  toMarkup (BlogDate (Just date)) =
    Blaze.text . Text.pack . formatTime defaultTimeLocale "%Y-%m-%d" $ date

readBlogPost :: MonadIO m => FilePath -> m BlogPost
readBlogPost filePath = do
  blogPostContent <- readFileUtf8 filePath
  let blogPostSlug = Text.pack (takeBaseName filePath)
      blogPostDate = readDateFromSlug blogPostSlug
      blogPostTitle = readTitleFromContent blogPostContent
      blogPostContentRendered = commonmarkToHtml [] [] blogPostContent
  pure BlogPost{..}

readDateFromSlug :: Text -> BlogDate
readDateFromSlug =
  BlogDate .
  parseTimeM allowSpace defaultTimeLocale "%Y-%m-%d" .
  Text.unpack . Text.intercalate "-" . take 3 . Text.split (== '-')
  where
    allowSpace = True

showBlogPostDate :: Maybe UTCTime -> Text
showBlogPostDate Nothing = ""
showBlogPostDate (Just date) = Text.pack $
  formatTime defaultTimeLocale "%Y-%m-%d" date

readTitleFromContent :: Text -> Text
readTitleFromContent =
    fromMaybe "Unknown title"
  . fmap (Text.dropWhile isHeader)
  . headMaybe
  . Text.lines
  where
    isHeader c = c == ' ' || c == '#'
