module Parser where

import Relude hiding (many)
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Types

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef Text () Identity
style =
  Lang.emptyDef
    { Tok.reservedNames = [],
      Tok.commentStart = "#|",
      Tok.commentEnd = "|#",
      Tok.caseSensitive = True,
      Tok.commentLine = ";",
      Tok.opStart = mzero,
      Tok.opLetter = mzero,
      Tok.identStart = letter <|> symbol,
      Tok.identLetter = letter <|> digit <|> symbol
    }

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

identifier :: Parser Text
identifier = toText <$> Tok.identifier lexer

number :: Parser Value
number = Number <$> Tok.integer lexer

true :: Parser Bool
true = do
  lexeme $ string "#t"
  pure True

false :: Parser Bool
false = do
  lexeme $ string "#f"
  pure False

boolean :: Parser Value
boolean = Boolean <$> (try true <|> false)

stringLiteral :: Parser Value
stringLiteral = String . toText <$> Tok.stringLiteral lexer

quoted :: Parser Value
quoted = do
  lexeme (char '\'')
  x <- value
  pure (List [Atom "quote", x])

quasiquoted :: Parser Value
quasiquoted = do
  lexeme (char '`')
  x <- value
  pure (List [Atom "quasiquote", x])

unquoted :: Parser Value
unquoted = do
  lexeme (char ',')
  x <- value
  pure (List [Atom "unquote", x])

atom :: Parser Value
atom = Atom <$> identifier

dottedList :: Parser Value
dottedList = parens $ do
  xs <- value `sepBy` whiteSpace
  lexeme (string ".")
  last <- value
  case last of
    -- if ends in pair, combine pair heads
    DottedList ls l -> pure (DottedList (xs ++ ls) l)
    -- if ends in list, then the pair is a list
    List ls -> pure (List (xs ++ ls))
    -- otherwise just pure pair
    _ -> pure (DottedList xs last)

list :: Parser Value
list = parens $ do
  values <- value `sepBy` whiteSpace
  pure (List values)

value :: Parser Value
value =
  atom
    <|> number
    <|> boolean
    <|> stringLiteral
    <|> quoted
    <|> quasiquoted
    <|> unquoted
    <|> try dottedList
    <|> list

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  result <- lexeme p
  eof
  pure result

topLevel :: Parser [Value]
topLevel = value `sepBy` whiteSpace

readValue :: Text -> Either ParseError Value
readValue = parse (contents value) "<stdin>"

readFileValues :: FilePath -> IO (Either ParseError [Value])
readFileValues file = do
  txt <- readFileText file
  pure (parse (contents topLevel) file txt)