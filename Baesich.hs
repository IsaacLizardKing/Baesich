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
<*> b2 01
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
type Env  = M.Map String Bumbur
type Baes = Integer
type Offset = Integer

data Arith where
    Lit   :: Bumbur -> Arith
    Bin   :: Op -> Arith -> Arith -> Arith
    Var   :: String -> Arith 
    Let   :: String -> Arith -> Arith -> Arith
    Bex   :: Arith -> Arith -> Arith -- Naive base conversion
    Bart  :: Arith -> Arith -> Arith -- Smart base conversion
 
 -- If    :: Arith -> Arith -> Arith -> Arith
 -- Bart  :: Baes -> Arith
 -- BaLit :: Baes -> Arith -> Arith
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
    Pipe  :: SOp
 -- LThan :: SOp
 -- GThan :: SOp
 -- DEq   :: SOp
 -- NEq   :: SOp
 -- ELThn :: SOp
 -- EGThn :: SOp
 -- Amp   :: SOp
    deriving (Show, Eq)


data Baerror where
    UnknownChar  :: Char -> String -> Baerror
    UndefinedVar :: String -> Baerror
    DivByZero    :: Baerror
    Curse        :: Baerror
    deriving (Show)



data Bumbur where
    Bum :: [Integer] -> Bumbur -> Integer -> Bumbur
    Bam :: [Integer] -> Bumbur
    Bur :: Integer
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

parseDigit :: Parser Integer
parseDigit = (-2) <$ char '.'
         <|> (-1) <$ char '-'
           <|> 0  <$ char '0'
           <|> 1  <$ char '1'
           <|> 2  <$ char '2'
           <|> 3  <$ char '3'
           <|> 4  <$ char '4'
           <|> 5  <$ char '5'
           <|> 6  <$ char '6'
           <|> 7  <$ char '7'
           <|> 8  <$ char '8'
           <|> 9  <$ char '9'
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

parseBaes :: Parser Bumbur
parseBaes = char 'b' *> (Bam <$> parseDigits)

parseBarqs :: Parser Arith
parseBex = Bex  <$> (char 'n' *> (parens parseArith)) <*> parens parseArith
       <|> Bart <$> (char 'b' *> (parens parseArith)) <*> parens parseArith

parseBam :: Parser Arith
parseBam = Lit <$> (Bam (parseDigits))

parseIf :: Parser Arith
parseIf = (If <$> (reserved "if" *> parseArith) <*> (reserved "then" *> parseArith) <*> (reserved "else" *> parseArith))

parseLet :: Parser Arith
parseLet = Let
    <$> (reserved   "let" *> identifier)
    <*> (reservedOp "="   *> parseArith)
    <*> (reserved   "in"  *> parseArith)

parseArithAtom :: Parser Arith
parseArithAtom = parseBarqs
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


-- parsePrefix :: Parser Arith
-- parsePrefix = (Pre Neg <$> ((reservedOp "-") *> (parseArith <|> parens parseArith)))

-- parseFuncs  :: Parser Arith
-- parseFuncs  = (Pre Sin <$ (reserved "sin") <*> parens parseArith) 
  --        <|> (Pre Cos <$ (reserved "cos") <*> parens parseArith)
    --      <|> (Pre Tan <$ (reserved "tan") <*> parens parseArith)
      --    <|> (Pre Abs <$ (reserved "abs") <*> parens parseArith)
        --  <|> (Pre Sqt <$ (reserved "sqrt") <*> parens parseArith)


baesich :: Parser Arith
baesich = whiteSpace *> parseArith <* eof

showBumbur :: Bumbur -> String
showBumbur (Bur b) = show b
showBumbur b       = showBumbur (bumbur2Bur b)

thriBaes :: Integer -> Integer -> Baes -> Offset
thriBaes a b c = if (a < (c ^ b)) then (b - 1) else thriBaes a (b + 1) c

toBaes :: Integer -> Baes -> Offset -> [Integer]
toBaes a b c = 
    let r = (((a `mod` (b ^ c)) + (b ^ c)) `mod` (b ^ c)) in 
        toBaes r b (c - 1) ++ ((a - r) 'div' (b ^ c))
toBaes 0 _ _ = []



whiper :: Bumbur -> Bumbur
whiper (Bum a b o) = bur2Bum b (bum2Bur (Bum a b o))


bum2BurUnpacked :: [Integer] -> Baes -> Offset -> Integer
bum2BurUnpacked (a:as) x o = a * (x ^ o) + bum2BurUnpacked (as x (o + 1))
bum2BurUnpacked [] _ _ = 0

bum2Bur :: Bumbur -> Bumbur
bum2Bur (Bum i (Bur a) o) = Bur (bum2BurUnpacked i a o)

bum2Bam :: Bumbur -> Bumbur
bum2Bam (Bum i _ _) = Bam i

bur2Bum :: Baes -> Bumbur -> Bumbur
bur2Bum b (Bur i) = 
    let OOfset = (thriBaes i 0 b) in
    let digs   = (toBaes i b OOfset) in
    Bum digs b ((length digs) - OOfset)

bur2Bam :: Baes -> Bumbur -> Bumbur
bur2Bam b (Bur i) =
    let OOfset = (thriBaes i 0 b) in
    let digs   = (toBaes i b OOfset) in
    Bam digs

bam2Bur :: Baes -> Offset -> Bumbur -> Bumbur
bam2Bur b o (Bam a) = Bur (bum2BurUnpacked a b o)

bam2Bum :: Bumbur -> Baes -> Offset -> Bumbur
bam2Bum (Bam a) b o = Bum a b o


bumbur2Bur :: Bumbur -> Bumbur
bumbur2Bur (Bum a b o) = bum2Bur a b o 
bumbur2Bur (Bam a)     = bam2Bur a 10 0
bumbur2Bur (Bur a)     = Bur a



cleanIntlst :: [Integer] -> [Integer]
cleanIntlst ((-2):xs) = xs
cleanIntlst (a:xs)    = a : (cleanIntlst xs) 

getOffset :: [Integer] -> Integer -> Integer
getOffset ((-2):_) d = d
getOffset [] d       = 0
getOffset (x:xs) d   = getOffset xs (d - 1)

sopBetwixt2Burs :: SOp -> Bumbur -> Bumbur -> Bumbur
sopBetwixt2Burs Plus (Bur a) (Bur b)  = Bur (a + b)
sopBetwixt2Burs Minus (Bur a) (Bur b) = Bur (a - b)
sopBetwixt2Burs Times (Bur a) (Bur b) = Bur (a * b)
sopBetwixt2Burs Div (Bur a) (Bur b)   = if (b == 0) then (Bur 0) else (Bur (a 'div' b))
sopBetwixt2Burs Pipe (Bur a) (Bur b)  = bur2Bum b (Bur a)
sopBetwixt2Burs _ _ _                 = Bur (-69)


bur2Int :: Bumbur -> Integer
bur2Int (Bur a) = a

bum2Intarr :: Bumbur -> [Integer]
bum2Intarr (Bum a _ _) = a
bum2Intarr (Bam a)     = a

offsetMatch :: Bumbur -> Bumbur -> Bumbur
offsetMatch (Bum a1 b1 o1) (Bum a2 b2 o2) = if (o1 <= o2) then (Bum a1 b1 o1) else (Bum (0:a1) b1 (o1 - 1))

nopPrep :: Bumbur -> Bumbur -> ([Integer], [Integer])
nopPrep (Bum a1 b1 o1) (Bum a2 b2 o2) = ((bum2Intarr (offsetMatch (Bum a1 b1 o1) (Bum a2 b2 o2))), (bum2Intarr (offsetMatch (Bum a2 b2 o2) (Bum a1 b1 o1))))

naddition :: [Integer] -> [Integer] -> [Integer]
naddition (b:bs) (c:cs) = case sopBetwixt2Burs Plus (Bur b) (Bur c) of
                                (Bur geoff) -> geoff : naddition bs cs
naddition [] (c:cs)     = case sopBetwixt2Burs Plus (Bur 0) (Bur c) of
                                (Bur geoff) -> geoff : naddition [] cs
naddition (b:bs)  []    = case sopBetwixt2Burs Plus (Bur b) (Bur 0) of
                                (Bur geoff) -> geoff : naddition bs []
naddition [] []         = []

nultiplication :: [Bumbur] -> [Integer]
nultiplication ((Bam a):(Bam b):c) = nultiplication (Bam (naddition Plus a (0:b))) (times10 c)
nultiplication [Bam a, Bam b]      = naddition Plus a (0:b)

times10 :: [Bumbur] -> [Bumbur]
times10 ((Bam a):as) = (Bam (0:a)) : (times10 as)
times10 [] = []

nultiplicationHelper :: [Integer] -> [Integer] -> [Bumbur]
nultiplicationHelper (a:as) (b:bs) = (Bam [a, b]) : (nultiplicationHelper as bs)
nultiplicationHelper (a:as) []     = (Bam [a, 0]) : (nultiplicationHelper as [])
nultiplicationHelper []     (b:bs) = (Bam [0, b]) : (nultiplicationHelper [] bs)
nultiplicationHelper []     []     = []

nOperation :: Op -> ([Integer], [Integer]) -> [Integer]
nOperation Plus (a, b)  = naddition a b
nOperation Times (a, b) = nultiplication (nultiplicationHelper a b)

doOpDiDoo :: Op -> Bumbur -> Bumbur -> Either Baerror Bumbur
doOpDiDoo (Smart op) a b                                       = Right sopBetwixt2Burs (op (bumbur2Bur a) (bumbur2Bur b))
doOpDiDoo (Naive (LeftBaes op))  (Bum a1 b1 o1) (Bum a2 b2 o2) = whiper (Bum (nOperation op (nopPrep (Bum a1 b1 o1) (Bum a2 b1 o2))) b1 (min o1 o2))
doOpDiDoo (Naive (RightBaes op)) (Bum a1 b1 o1) (Bum a2 b2 o2) = whiper (Bum (nOperation op (nopPrep (Bum a1 b1 o1) (Bum a2 b1 o2))) b2 (min o1 o2))
doOpDiDoo (Naive (BothBaes op))  (Bum a1 b1 o1) (Bum a2 b2 o2) = whiper (Bum (nOperation op (nopPrep (Bum a1 b1 o1) (Bum a2 b1 o2))) (b1 + b2) (min o1 o2))

interpBaesich :: Env -> Arith -> Either Baerror Bumbur
interpBaesich e (Bart e1 e2)   = interpBaesich e e1 >>= \b1 -> 
                                 interpBaesich e e2 >>= \b2 -> 
                                    Right bur2Bum (bum2Bur b1) (Bur (bum2Bur b2))
interpBaesich e (Bex e1 e2)    = interpBaesich e e1 >>= \b1 -> 
                                 interpBaesich e e2 >>= \b2 -> case b2 of
                                    (Bum a b o) -> Right (Bum a b2 o) 
                                    (Bam a)     -> Right (Bum a b2 0) 
                                    (Bur i)     -> Right (bur2Bum b2 i)
interpBaesich _ (Lit (Bam n))  = bam2Bum (Bam (cleanIntlst (reverse n))) 10 (getOffset (reverse n))
interpBaesich e (Var s)        = case (M.lookup s e) of 
    Just b  -> Right b
    Nothing -> Left (UnknownChar 'v' "stupid bitch")
interpBaesich e (Let s e1 e2)  = interpBaesich e e1 >>= \b1 -> interpBaesich (M.insert s b1 e) e2
interpBaesich e (Bin op e1 e2) = interpBaesich e e1 >>= \b1 -> interpBaesich e e2 >>= \b2 -> doOpDiDoo


baesichEval :: String -> Arith
baesichEval s = case parse baesich s of 
    Left s  -> Var (show s)
    Right a -> case interpBaesich M.empty a of
        Left s  -> Var (show s)
        Right n -> Lit n


