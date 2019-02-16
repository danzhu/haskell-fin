module Compiler.Parser
  ( ParserError
  , parseAst
  ) where

import           Compiler.Ast
import           Compiler.Data
import           Control.Applicative (empty, liftA2, many, (<|>))
import           Control.Monad (void)
import           Control.Monad.Combinators.Expr (Operator (InfixL),
                                                 makeExprParser)
import           Data.Void (Void)
import           Text.Megaparsec (ParseErrorBundle, Parsec, between, eof,
                                  getOffset, noneOf, parse, takeWhile1P)
import           Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space spc lineComment empty
  where spc = void $ takeWhile1P Nothing (== ' ')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{" <* scn) (symbol "}")


variable :: Parser Var
variable = Var <$> lexeme var
  where var = liftA2 (:) idStart $ many idCont
        idStart = lowerChar
        idCont = alphaNumChar <|> char '_'

srcPos :: Parser SrcPos
srcPos = SrcPos <$> getOffset

node :: Parser a -> Parser (Node a)
node = liftA2 mkNode srcPos

literal :: Parser Lit
literal = pInt <|> pStr
  where pInt = LInt . fromIntegral <$> integer
        pEsc = char '\\' *> (char 'n' *> pure '\n')
        pStr = char '"' *> (LStr <$> many (noneOf "\\\"" <|> pEsc)) <* char '"'

pattern :: Parser Pat
pattern = node (pLit <|> pVar <|> pIgn) <|> parens pattern
  where pLit = PLit <$> literal
        pVar = PVar <$> variable
        pIgn = PIgn <$ symbol "_"

arm :: Parser Arm
arm = Arm <$> (pattern <|> pIgn) <*> (symbol "|" *> expression)
  where pIgn = node $ pure PIgn

atom :: Parser Expr
atom = node (pLit <|> pVar <|> pAbs) <|> parens expression <|> braces block
  where pLit = ELit <$> literal
        pVar = EVar <$> variable
        pAbs = EAbs <$> (symbol "|" *> (braces (many $ arm <* scn) <|> (:[]) <$> arm))

expression :: Parser Expr
expression = makeExprParser atom table
  where table = [ [ InfixL pApp ]
                , [ InfixL pDot ] ]
        pApp = do
          pos <- srcPos
          pure $ \f a -> mkNode pos $ EApp f a
        pDot = do
          pos <- srcPos <* symbol "."
          pure $ \a f -> mkNode pos $ EApp f a

block :: Parser Expr
block = pLet <|> pDef <|> pExpr
  where pNext = scn *> block
        pLet = do
          pos <- symbol "let" *> srcPos
          name <- variable
          val <- symbol "=" *> expression
          mkNode pos . ELet name val <$> pNext
        pDef = do
          pos <- symbol "def" *> srcPos
          name <- variable
          params <- many pattern
          body <- symbol "=" *> expression
          let fold pat@Node{ nPos = p } e = mkNode p $ EAbs [Arm pat e]
              val = foldr fold body params
          mkNode pos . EDef name val <$> pNext
        pExpr = do
          pos <- srcPos
          expr <- expression <* scn
          mkNode pos . ESeq expr <$> block <|> pure expr


parseAst :: String -> String -> Either ParserError Ast
parseAst = parse $ Ast <$> block <* scn <* eof
