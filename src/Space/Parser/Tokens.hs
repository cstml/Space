module Space.Parser.Token where

import Data.String
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser a = forall e s m. (MonadParsec e s m, Token s ~ Char) => m a

text :: IsString a => Parser a
text = do
  x <- char '*'
  return (fromString [x])
