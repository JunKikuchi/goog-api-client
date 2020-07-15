module Path.Parser where

import           RIO                     hiding ( many
                                                , mod
                                                , some
                                                )
import qualified RIO.Text                      as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
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
  (op, str, mod) <- do
    op  <- operator
    str <- T.pack <$> many (noneOf ['}', '*', ':'])
    mod <- modifier
    return (op, str, mod)
  void $ char '}'
  pure (Expression op mod str)

operator :: Parser (Maybe Operator)
operator = choice
  [ Just Reserved <$ char '+'
  , Just Fragment <$ char '#'
  , Just PathSegment <$ char '/'
  , pure Nothing
  ]

modifier :: Parser (Maybe Modifier)
modifier = choice [Just Explode <$ char '*', prefixModifier, pure Nothing]

prefixModifier :: Parser (Maybe Modifier)
prefixModifier = do
  void $ char ':'
  num <- decimal
  pure (Just (Prefix num))

literal :: Parser Template
literal = (Literal . T.pack) <$> some (noneOf ['/', '{'])

slash :: Parser ()
slash = void $ char '/'
