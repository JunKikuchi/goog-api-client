module Path.Parser where

import           RIO                     hiding ( many
                                                , some
                                                )
import qualified RIO.Text                      as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Path.Types

type Parser = Parsec Void Text

path :: Parser Path
path = do
  s  <- segment
  ss <- many $ do
    slash
    segment
  eof
  pure (Path (s : ss))

segment :: Parser Segment
segment = many template

template :: Parser Template
template = expression <|> literal

expression :: Parser Template
expression = do
  void $ char '{'
  (op, str) <- do
    op  <- operator
    str <- T.pack <$> many (satisfy (/= '}'))
    return (op, str)
  void $ char '}'
  pure (Expression op str)

operator :: Parser (Maybe Operator)
operator =
  choice [Just Reserved <$ char '+', Just Fragment <$ char '#', pure Nothing]

literal :: Parser Template
literal = (Literal . T.pack) <$> some (noneOf ['/', '{'])

slash :: Parser ()
slash = void $ char '/'
