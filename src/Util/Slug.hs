module Util.Slug
  ( Slug
  , slug
  ) where

import RIO hiding (many)
import qualified RIO.Text as Text

import Data.Char (isAsciiLower)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Slug =
  Slug Text
  deriving (Show, ToField, FromField)

slugChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
slugChar = satisfy isAsciiLower <?> "slug letter"

slugParser :: Parsec Void Text Slug
slugParser = do
  s <- many (slugChar <|> digitChar <|> char '-')
  return (Slug (Text.pack s))

slug :: Text -> Maybe Slug
slug = parseMaybe slugParser
