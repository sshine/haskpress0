
module Web.HaskPress.BlogPost where

import           RIO
import           RIO.List
import qualified RIO.Map as Map
import           RIO.Map (Map)
import qualified RIO.Text as Text
import           RIO.Text (Text)
import           RIO.Time (UTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)

type BlogTitle = Text
data BlogPost = BlogPost
  { blogPostDate            :: Maybe UTCTime
  , blogPostTitle           :: BlogTitle
  , blogPostContent         :: Text
  , blogPostContentRendered :: Text
  } deriving (Eq, Ord, Show)

readDateFromFilePath :: FilePath -> Maybe UTCTime
readDateFromFilePath =
  parseTimeM allowSpace defaultTimeLocale "%Y-%m-%d" .
  concat . intersperse "-" . take 3 . splitOn "-"
  where allowSpace = True

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

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _sep [] = []
splitOn seps xs = case span (`notElem` seps) xs of
  (ys, []) -> [ys]
  (ys, rest) -> ys : splitOn seps (drop 1 rest)
