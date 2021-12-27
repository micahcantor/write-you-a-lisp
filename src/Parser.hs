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
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

identifier :: Parser Text
identifier = toText <$> Tok.identifier lexer

quote :: Parser Text
quote = toText <$> string "\'"

number :: Parser Value
number = Number <$> Tok.integer lexer

true :: Parser Bool
true = do
  lexeme $ string "#t"
  return True

false :: Parser Bool
false = do
  lexeme $ string "#f"
  return False

boolean :: Parser Value
boolean = Boolean <$> (try true <|> false)

stringLiteral :: Parser Value
stringLiteral = String . toText <$> Tok.stringLiteral lexer

atom :: Parser Value
atom = do
  atom <- identifier
  if atom == "."
    then parserZero
    else return (Atom atom)

pair :: Parser Value
pair = parens $ do
  car <- value
  lexeme $ string "."
  cdr <- value
  return (Pair car cdr)

list :: Parser Value
list = parens $ do
  values <- value `sepBy` whiteSpace
  return (List values)

value :: Parser Value
value =
  atom
    <|> number
    <|> boolean
    <|> stringLiteral
    <|> try pair
    <|> list

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  result <- lexeme p
  eof
  return result

topLevel :: Parser [Value]
topLevel = value `sepBy` whiteSpace

readValue :: Text -> Either ParseError Value
readValue = parse (contents value) "<stdin>"

readFileValues :: FilePath -> IO (Either ParseError [Value])
readFileValues file = do
  txt <- readFileText file
  return (parse (contents topLevel) file txt)