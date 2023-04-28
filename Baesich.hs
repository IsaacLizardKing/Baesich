{-# LANGUAGE GADTSyntax #-}

module Baesich where
import qualified Data.Map as M 
import qualified Text.Parsec.Error
import           Parsing2  hiding ((<$), (<$>), (<*>), (*>), (<*))
import           Prelude



{-

<bumber> ::= 
    | <int>
    | <string>

<baes> ::=
    | 'b' <bumber> 
    | 'b' <expr>

<expr> ::=
  | <bumber>
  | <baes> <expr>
  | <expr> '|>' <baes>
  | <expr> <op> <expr>
  | <var>
  | 'let' <var> '=' <expr> 'in' <expr>
  | 'if' <expr> 'then' <expr> 'else' <expr>

<op> ::= <opfr> | <nop>

<opfr>  ::= '+' | '-' | '*' | '/' | '<' | '==' | '>' | '<=' | '>=' | '&&' | '||'

<nop> ::= '<' <opfr> | '<' <opfr> '>' | <opfr> '>'

...
Nultiplication

    b11 13
<*> b12 24
       120
        34
   b23 154 
    
    b23 (12) (34)
    b10 (25) (73)
      b23 120
    + b23  34
    _________
      b23 154


    b3 20
<*> b2  1
    b5 21

    b5  3
<*> b4  2
    b9 32

Naddition

    b11 1  9
<+> b12 2  3
    b23 3  C
    b10 81

    b11 1  9
<+  b12 2  3
    b11 3  "C" -> 4  1 (for garrett because he has only thought about this for 2 days)
    b10 45

    b11 1  9
+>  b12 2  3
    b12 4  0
    b10 48


Nubtraction

    b5    22
<-> b3    11
    b2    11   b10(3)

    b3    22
<-> b5    11
    b(-2) 11   b10(-1)

    b22   1A
<-> b2    10
    b20   0A

    b2   1
<-> b2   0
    b0   1 ====> return 0

    b20  1A
<-> b16  10
    b4   "A" -> 22

Nivision   (DIVISION BY 0 STILL APPLIES AS UNDEFINED)

    b5 4
</> b2 1
    b3 "4" -> 11

    b7 04
</> b5 14
    b2 "01" -> 1

    b3   2
</> b3   1
    b0   2 ====> return 0

    b27   1C
</> b33   2F
    b(-6) 
          (0.5) * -6
            -(3)
            (3/4) * 1
            (3/4)

            "-(2 + 1/4)"  -> 14.23
                             -3.53


                b10 -8 == b(-6) 24
                b10 -7 == b(-6) 25
                b10 -6 == b(-6) 10
                b10 -5 == b(-6) 11
                b10 -4 == b(-6) 12

                b(-10) 77 == b10 7 + (-70) = -63
                b(-10) 78 == b10 8 + (-70) = -62
                b(-10) 79 == b10 9 + (-70) = -61
                b(-10) 80 == b10 0 + (-80) = -80
                b(-10) 81 == b10 1 + (-80) = -79
                b(-10) 82 == b10 2 + (-80) = -78
                b(-10) 83 == b10 3 + (-80) = -77

                b(-10) -80 == b10 -0 + 80 = 80
                b(-10) -79 == b10 -9 + 70 = 61

                b(-10) -120 == b10 -0 + 20 + -100 = -80
                
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