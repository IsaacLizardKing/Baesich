{-# LANGUAGE GADTSyntax #-}

module Baesich where
import qualified Data.Map as M 
import qualified Text.Parsec.Error
import           Parsing2  hiding ((<$), (<$>), (<*>), (*>), (<*))
import           Prelude



{-






-}



charOrder :: [Char]
charOrder = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"


data Bumber where 
    Gen   :: Integer -> Bumber
    Weird :: String -> Bumber
    deriving (Show)

data Arith where
    BaLit :: Bumber -> Arith -> Arith
    Bipe  :: Arith -> Arith -> Arith
    Pipe  :: Arith -> Arith -> Arith
    Pope  :: Op -> Arith -> Arith -> Arith -> Arith
    Lit   :: Bumber -> Arith
    Bin   :: Op -> Arith -> Arith -> Arith
    Var   :: String -> Arith 
    Let   :: String -> Arith -> Arith -> Arith
    If    :: Arith -> Arith -> Arith -> Arith
    deriving (Show)

data Op where
    Naive :: NOp
    Smart :: SOp
    deriving (Show, Eq)

data NOp where
    LeftBaes  :: SOp -> NOp
    RightBaes :: SOp -> NOp
    BothBaes  :: SOp -> NOp
    deriving (Show)

data SOp where
    Plus  :: SOp
    Minus :: SOp
    Times :: SOp
    Div   :: SOp
    LThan :: SOp
    GThan :: SOp
    DEq   :: SOp
    NEq   :: SOp
    ELThn :: SOp
    EGThn :: SOp
    Amp   :: SOp
    Bar   :: SOp
    deriving (Show, Eq)



lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef
  { reservedNames = (["let", "in", "True", "False", "if", "then", "else"])}

parens  :: Parser a -> Parser a
parens     = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved   = getReserved lexer

dubOrInt :: Parser (Either Integer Double)
dubOrInt = getNaturalOrFloat lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

integer :: Parser Integer
integer = getInteger lexer

identifier :: Parser String
identifier = getIdentifier lexer

parseBaLit :: Parser Arith
parseBaLit = BaLit <$> (symbol "b" *> parseArith) <*> parens parseArith

parseIf :: Parser Arith
parseIf = (If <$> (reserved "if" *> parseArith) <*> (reserved "then" *> parseArith) <*> (reserved "else" *> parseArith))

--parsePrefix :: Parser Arith
--parsePrefix = (Pre Neg <$> ((reservedOp "-") *> (parseArith <|> parens parseArith)))

--parseFuncs  :: Parser Arith
--parseFuncs  = (Pre Sin <$ (reserved "sin") <*> parens parseArith) 
  --        <|> (Pre Cos <$ (reserved "cos") <*> parens parseArith)
    --      <|> (Pre Tan <$ (reserved "tan") <*> parens parseArith)
      --    <|> (Pre Abs <$ (reserved "abs") <*> parens parseArith)
        --  <|> (Pre Sqt <$ (reserved "sqrt") <*> parens parseArith)


parseBaes :: Parser Baes
parseBaes = ((char 'b') *> integer)

parseBaLit :: Parser Arith
parseBaLit = BaLit <$> parseBaes <*> parens parseArith


parseArithAtom :: Parser Arith
parseArithAtom = parseLet
    <|> Var <$> identifier
    <|> parseIf
    <|> parens parseArith

parseLet :: Parser Arith
parseLet = Let
    <$> (reserved   "let" *> identifier)
    <*> (reservedOp "="   *> parseArith)
    <*> (reserved   "in"  *> parseArith)

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table = [ 
              [ Infix (Bin Times <$ reservedOp "*") AssocLeft
              , Infix (Bin Div   <$ reservedOp "/") AssocLeft
              ]
            , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
              , Infix (Bin Minus <$ reservedOp "-") AssocLeft
              ]
            , [ Infix (Bin GThan <$ reservedOp ">") AssocNone
            ,   Infix (Bin LThan <$ reservedOp "<") AssocNone
            ,   Infix (Bin DEq   <$ reservedOp "==") AssocNone
            ,   Infix (Bin ELThn <$ reservedOp "<=") AssocNone
            ,   Infix (Bin EGThn <$ reservedOp ">=") AssocNone
              ]
            ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof




baesich :: Parser Integer
baesich = whiteSpace *> integer <* eof

baesichEval :: String -> Integer
baesichEval s = case parse baesich s of 
    Left s  -> 0
    Right a -> a


--interpBaesich :: 