{-# LANGUAGE GADTSyntax #-}

module Baesich where
import qualified Data.Map as M 
import qualified Text.Parsec.Error
import           Parsing2  hiding ((<$), (<$>), (<*>), (*>), (<*))
import           Prelude
import           Data.List
import           Text.ParserCombinators.Parsec.Prim (many)
import           GHC.List

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
    ---------
      b23 154


    b3 20
<*> b2  1
    b5 21

    b5  3
<*> b4  2
    b9 32

Naddition

     b11 1 | 9
 <+> b12 2 | 3
 --------------
     b23 3 | C
     b23 3C    
  OR b10 81



     b11 1 | 9   
 +>  b12 2 | 3   
 --------------
     b12 4 | 0   
     b12 40      
  OR b10 48      


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
...



Nop rules:

    Rules of Naivete:
        digits are treated as pure values, and are operated on as such, without regard for the bases they originate from
        the pure values are then read as values of the resulting base, naively, that is, the pure value of each digit
        is accepted as digits of the resulting base.

            for addition:
                pure values of digits are added without regard for base, and then are interpreted as values of the new base
            
            for multiplication:
                digits are appended to each other, and the bases are added, the result is then added normally, i.e.
                        b1 d1 d2
                    <*> b2 d3 d4
                (b1+b2) d1 d3  0
              + (b1+b2)    d2 d4
                (b1+b2) d1 (d3 + d2) d4

            for nubtraction:
                same as addition, but bases are subtracted from one another. 
                NOTE: The value of any number converted to b0 is 0

            for division:
                same as nubtraction, but pure values are divided and then interpreted in the resulting base.
            


            Baeversion
    (|) : convert left expression to right base normally through a generic base
    (|>): perform preceding operation normally, convert to following base naively

    Not these.
   (<|) : perform preceding operation naively, convert to following base normally
   (<|>): perform preceding operation naivley, convert to following base naively

            Naddition
    (+) : add numbers normally, use common base OR if no common base, greatest base or generic base
   (<+) : add numbers naively, interpret in left base
    (+>): add numbers naively, interpret in right base
   (<+>): add numbers naively, interpret in the sum of the two bases

        +----------------+----------------+----------------+----------------+
        |      ( + )     |      (<+ )     |      ( +>)     |      (<+>)     |
        +----------------+----------------+----------------+----------------+
        |     b11 19     |     b11 1 | 9  |     b11 1 | 9  |     b11 1 | 9  |
        |  +  b12 23     | <+  b12 2 | 3  | +>  b12 2 | 3  | <+> b12 2 | 3  |
        +----------------+----------------+----------------+----------------+
        |     b6  32     |     b11 3 | C  |     b12 4 | 0  |     b23 3 | C  |
        |     b6  43     |  -> b11 4 | 1  |     b12 40     |     b23 3C     |
        |     b6  75     |  OR b6  73     |  OR b6  120    |  OR b6  213    |
        +----------------+----------------+----------------+----------------+ 
    


            Nubtraction
    (-) : sub numbers normally, use common base OR if no common base, greatest base or generic base
   (<-) : sub numbers naively, interpret in left base
    (->): sub numbers naively, interpret in right base
   (<->): sub numbers naively, interpret in the difference of the two bases (signed)

            Nultiplication
    (*) : mul numbers normally, use common base OR if no common base, greatest base or generic base
   (<*) : mul numbers naively, interpret in left base
    (*>): mul numbers naively, interpret in right base
   (<*>): mul numbers naively, interpret in the sum of the two bases

            Nivision
    (/) : div numbers normally, use common base OR if no common base, greatest base or generic base
   (</) : div numbers naively, interpret in left base
    (/>): div numbers naively, interpret in right base
   (</>): div numbers naively, interpret in the difference of the two bases (signed)

-}

type Baes = [Integer]

data Arith where
    Bart  :: Baes -> Arith
    BaLit :: Baes -> Arith -> Arith
    Lit   :: Bumber -> Arith
    Bin   :: Op -> Arith -> Arith -> Arith
    Var   :: String -> Arith 
    Let   :: String -> Arith -> Arith -> Arith
    If    :: Arith -> Arith -> Arith -> Arith
 -- Bipe  :: Arith -> Arith -> Arith
 -- Pope  :: Op -> Arith -> Arith -> Arith -> Arith
    deriving (Show)

data Op where
    Naive :: NOp -> Op
    Smart :: SOp -> Op
    deriving (Show)

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
 -- LThan :: SOp
 -- GThan :: SOp
 -- DEq   :: SOp
 -- NEq   :: SOp
 -- ELThn :: SOp
 -- EGThn :: SOp
 -- Amp   :: SOp
    Pipe  :: SOp
    deriving (Show, Eq)

data Baerror where
    UnknownChar :: Char -> String -> Baerror
    deriving (Show)

data Bumber where
    Bum :: [Integer] -> Baes -> Integer -> Bumber
    Bam :: [Integer] -> Integer -> Bumber
    deriving (Show)



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

--parseSpecialDigits


parseDigit :: Parser Integer
parseDigit = 0 <$ char '0'
         <|> 1 <$ char '1'
         <|> 2 <$ char '2'
         <|> 3 <$ char '3'
         <|> 4 <$ char '4'
         <|> 5 <$ char '5'
         <|> 6 <$ char '6'
         <|> 7 <$ char '7'
         <|> 8 <$ char '8'
         <|> 9 <$ char '9'
         <|> 10 <$ char 'A'
         <|> 11 <$ char 'B'
         <|> 12 <$ char 'C'
         <|> 13 <$ char 'D'
         <|> 14 <$ char 'E'
         <|> 15 <$ char 'F'
         <|> 16 <$ char 'G'
         <|> 17 <$ char 'H'
         <|> 18 <$ char 'I'
         <|> 19 <$ char 'J'
         <|> 20 <$ char 'K'
         <|> 21 <$ char 'L'
         <|> 22 <$ char 'M'
         <|> 23 <$ char 'N'
         <|> 24 <$ char 'O'
         <|> 25 <$ char 'P'
         <|> 26 <$ char 'Q'
         <|> 27 <$ char 'R'
         <|> 28 <$ char 'S'
         <|> 29 <$ char 'T'
         <|> 30 <$ char 'U'
         <|> 31 <$ char 'V'
         <|> 32 <$ char 'W'
         <|> 33 <$ char 'X'
         <|> 34 <$ char 'Y'
         <|> 35 <$ char 'Z'
    

parseDigits :: Parser [Integer]
parseDigits = many $ parseDigit 

-- parseDigit :: Parser Integer
-- parseDigit = choice $ map (try . char) $ ['0'..'9'] ++ ['A'..'Z']

--parsePrefix :: Parser Arith
--parsePrefix = (Pre Neg <$> ((reservedOp "-") *> (parseArith <|> parens parseArith)))

--parseFuncs  :: Parser Arith
--parseFuncs  = (Pre Sin <$ (reserved "sin") <*> parens parseArith) 
  --        <|> (Pre Cos <$ (reserved "cos") <*> parens parseArith)
    --      <|> (Pre Tan <$ (reserved "tan") <*> parens parseArith)
      --    <|> (Pre Abs <$ (reserved "abs") <*> parens parseArith)
        --  <|> (Pre Sqt <$ (reserved "sqrt") <*> parens parseArith)

parseBaes :: Parser Baes
parseBaes = do
  char 'b'
  base <- parseDigits
  return $ base

parseBaLit :: Parser Arith
parseBaLit = (BaLit <$> parseBaes <*> parens parseArith)
         <|> (Bart  <$> parseBaes)

parseBam :: Parser Arith
parseBam = do
    digs <- parseDigits
    --sgid <- reverse <$> digs
    return $ Lit (Bam digs 0)

parseIf :: Parser Arith
parseIf = (If <$> (reserved "if" *> parseArith) <*> (reserved "then" *> parseArith) <*> (reserved "else" *> parseArith))

parseLet :: Parser Arith
parseLet = Let
    <$> (reserved   "let" *> identifier)
    <*> (reservedOp "="   *> parseArith)
    <*> (reserved   "in"  *> parseArith)

parseArithAtom :: Parser Arith
parseArithAtom = parseBaLit
    <|> parseBam
    <|> parens parseArith
 -- <|> parseLet
 -- <|> Var <$> identifier
 -- <|> parseIf

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table = [ 
              [ Infix (Bin (Smart Times          ) <$ reservedOp  "*")  AssocLeft
              , Infix (Bin (Smart Div            ) <$ reservedOp  "/")  AssocLeft
              , Infix (Bin (Naive (LeftBaes Times )) <$ reservedOp "<*")  AssocLeft
              , Infix (Bin (Naive (LeftBaes Div   )) <$ reservedOp "</")  AssocLeft
              , Infix (Bin (Naive (RightBaes Times)) <$ reservedOp  "*>") AssocLeft
              , Infix (Bin (Naive (RightBaes Div  )) <$ reservedOp  "/>") AssocLeft
              , Infix (Bin (Naive (BothBaes Times )) <$ reservedOp "<*>") AssocLeft
              , Infix (Bin (Naive (BothBaes Div   )) <$ reservedOp "</>") AssocLeft
              ]
            , [ Infix (Bin (Smart Plus           ) <$ reservedOp  "+")  AssocLeft
              , Infix (Bin (Smart Minus          ) <$ reservedOp  "-")  AssocLeft
              , Infix (Bin (Naive (LeftBaes Plus  )) <$ reservedOp "<+")  AssocLeft
              , Infix (Bin (Naive (LeftBaes Minus )) <$ reservedOp "<-")  AssocLeft
              , Infix (Bin (Naive (RightBaes Plus )) <$ reservedOp  "+>") AssocLeft
              , Infix (Bin (Naive (RightBaes Minus)) <$ reservedOp  "->") AssocLeft
              , Infix (Bin (Naive (BothBaes Plus  )) <$ reservedOp "<+>") AssocLeft
              , Infix (Bin (Naive (BothBaes Minus )) <$ reservedOp "<->") AssocLeft
              ]
              , [
                Infix (Bin (Smart Pipe          ) <$ reservedOp  "|" ) AssocLeft
              , Infix (Bin (Naive (RightBaes Pipe)) <$ reservedOp  "|>") AssocLeft
              ]
--            , [ Infix (Bin GThan <$ reservedOp ">") AssocNone
  --          ,   Infix (Bin (LThan <$ reservedOp "<") AssocNone
    --        ,   Infix (Bin (DEq   <$ reservedOp "==") AssocNone
      --      ,   Infix (Bin (ELThn <$ reservedOp "<=") AssocNone
        --    ,   Infix (Bin (EGThn <$ reservedOp ">=") AssocNone
          --    ]
            ]

baesich :: Parser Arith
baesich = whiteSpace *> parseArith <* eof

cleanIntlst :: [Integer] -> [Integer]
cleanIntlst ((-2):xs) = xs
cleanIntlst (a:xs)    = a : (cleanIntlst xs) 

getOffset :: [Integer] -> Integer -> Integer
getOffset ((-2):xs) d = d
getOffset [] d        = 0
getOffset (x:xs) d    = getOffset xs (d - 1)




baesichEval :: String -> Arith
baesichEval s = case parse baesich s of 
    Left s  -> Var (show s)
    Right a -> a


--interpBaesich :: 