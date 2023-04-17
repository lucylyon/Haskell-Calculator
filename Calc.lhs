 {-# OPTIONS_GHC -Wall #-}

> {-# LANGUAGE GADTs #-}

> module Calc where
> import           Parsing2
> import qualified Data.Map as M
> import Text.Printf
> import Data.Complex
  
 --------------------
 ---- Data Types ----        
 --------------------

   CompLit :: Complex Double -> Arith 

> data Arith where
>   Lit :: Double -> Arith                  -- this is basically the same as what we did in class, 
>   Neg :: Arith -> Arith                           -- but Lit is a double, and Neg allows negatives
>   Pi  :: Arith                                    -- supposed to be pi, but not fully
>   E   :: Arith                                    -- "" e
>   Trig :: TrigOp -> Arith -> Arith
>   -- Time :: Integer -> TimeUnit -> Arith 
>   Length :: Arith -> LengthUnit -> Arith 
>   Bin :: Op -> Arith -> Arith -> Arith            -- if there is anything after pi or e, it doesn't look at them
>   deriving (Show)                                 -- they should probably be Float -> Arith, but I couldn't get that to work either 
>                                                   -- so I am giving up
> 
> data Op where                                     -- from class, but with exponentiation
>   Plus      :: Op
>   Minus     :: Op
>   Times     :: Op
>   Divide    :: Op
>   Exponent  :: Op
>   deriving (Show, Eq)

> data TrigOp where
>   Sin :: TrigOp
>   Cos :: TrigOp
>   Tan :: TrigOp
>   Log :: TrigOp
>   Abs :: TrigOp
>   deriving (Show, Eq)

> data LengthUnit where
>   Inch :: LengthUnit
>   Foot :: LengthUnit
>   deriving (Show, Eq)

% >   Mile :: Unit
% >   Kilometer :: Unit
% >   Meter :: Unit
% >   Centimeter :: Unit

% > data Val where
% >   V :: Double -> Type -> Val
% >   deriving (Show, Eq)


% > data TimeUnit where
% >   Second :: TimeUnit
% >   Minute :: TimeUnit
% >   Hour   :: TimeUnit
% >   Day    :: TimeUnit
% >   Month  :: TimeUnit
% >   Year   :: TimeUnit
% >   deriving (Show, Eq)

> data Type where
>   TypeLit  :: Type
>   TypeTime :: Type
>   TypeLength :: Type

> data InterpError where                            -- from class
>   DivideByZero :: InterpError
>   TypeTimeError :: InterpError
>
> type Env = M.Map String Integer 

 ---------------------
 ----- Parsers -------       
 ---------------------

Add support for complex numbers. For example, the user should be able to enter expressions like 3 + 2i.

> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where                                                                 -- the same as what we did in class, but 
>     table = [ [ Prefix (Trig Sin      <$ reservedOp "sin") 
>               , Prefix (Trig Cos      <$ reservedOp "cos") 
>               , Prefix (Trig Tan      <$ reservedOp "tan")
>               , Prefix (Trig Log      <$ reservedOp "log") 
>               , Prefix (Trig Abs      <$ reservedOp "abs")  ]
>               , [ Infix (Bin Exponent <$ reservedOp "^") AssocRight ]    -- added exponentiation, highest precedence because PEMDAS
>               , [ Infix (Bin Times    <$ reservedOp "*") AssocLeft      -- ^ is AssocRight so it follows math rules
>               ,   Infix (Bin Divide   <$ reservedOp "/") AssocLeft ]     -- ex. 2^3^4 = 2^(3^4) instead of (2^3)^4 
>               , [ Infix (Bin Plus     <$ reservedOp "+") AssocLeft
>               ,   Infix (Bin Minus    <$ reservedOp "-") AssocLeft ]
>             ] 
             


                 <|> Lit <$> try (complex <* reserved "i") <|> float <|> fromIntegral <$> integer


> parseArithAtom :: Parser Arith
> parseArithAtom =  parens parseArith 
>                  <|> Pi  <$ (reserved "Pi" <|> reserved "pi")               -- parses Pi
>                  <|> E   <$ reserved "e"
>                  <|> Neg <$> (reservedOp "-" *> parseArithAtom)             -- parses negatives 
>                  <|> try parseLength
>                 -- <|> try parseTime 
>                  <|> parseLit
>                 -- <|> Lit <$> try (float <* reserved "i") <|> float <|> fromIntegral <$> integer   
>                                                                         -- tries to parse a double into a Lit 
>                                                                         -- if it can't, it parses it as an integer
>                                                                         -- then converts it to a double
>                                                                         -- this made more sense to me than naturalOrFloat

try (integer <* reserved "i") <|> Real <$> integer


% > parseTime :: Parser Arith
% > parseTime = Time <$> integer <*> parseTimeUnits

% > parseTimeUnits :: Parser TimeUnit
% > parseTimeUnits = (Second <$ reservedOp "sec") <|> (Minute <$ reservedOp "min") <|> (Hour <$ reservedOp "hr") <|> (Day <$ reservedOp "day") <|> (Month <$ reservedOp "month") <|> (Year <$ reservedOp "yr")

> parseLength :: Parser Arith 
> parseLength = Length <$> parseLit <*> parseLengthUnit


parseLength = Length <$> (parseLit <*> (LengthUnit <$> parseLengthUnit) )
parseLength = Lit <$> (Val <$> float <*> (Length <$> parseUnits))

> parseLengthUnit :: Parser LengthUnit
> parseLengthUnit = (Inch <$ reservedOp "in") <|> (Foot <$ reservedOp "ft")

> parseConvert :: Parser Arith
> parseConvert = Length <$> (parens parseArith) <* reservedOp "as" <*> parseLengthUnit

> parseLit :: Parser Arith
> parseLit = Lit <$> (try float <|> fromIntegral <$> integer)

> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof                                                
>
> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>   { reservedNames = ["sin", "cos", "tan", "log", "abs", "sec", "min", "hr", "day", "mon", "yr", "as", "ft", "in"] }
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reservedOp :: String -> Parser ()                                       -- a bajilion parsers from class
> reservedOp = getReservedOp lexer
>
> reserved :: String -> Parser ()
> reserved = getReserved lexer
>
> integer :: Parser Integer
> integer    = getInteger lexer
>
> float :: Parser Double
> float = getFloat lexer
>
> naturalOrFloat :: Parser (Either Integer Double)
> naturalOrFloat = getNaturalOrFloat lexer

-- (float <|> fromIntegral <$> integer) <* reserved "i"

> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> identifier :: Parser String
> identifier = getIdentifier lexer

 ------------------------
 ---- Interpreters ------       
 ------------------------

> showInterpError :: InterpError -> String                                          -- displays error messages
> showInterpError DivideByZero      = "you should know you can't divide by zero..."
> showInterpError TypeTimeError         = "either everything has to be a measurement of time, or nothing is. you can't have it all"

> interpArith :: Env -> Arith -> Either InterpError Double                         -- same as class, changed Integer to Double
> interpArith _ Pi                              = Right pi
> interpArith _ E                               = Right (exp 1)                     -- e = 2.71828
> interpArith _   (Lit x)                       = Right x
> interpArith env (Neg x)                       = negate <$> interpArith env x      -- Negates an arith
> interpArith env (Bin Plus arith1 arith2)      = (+) <$> interpArith env arith1 <*> interpArith env arith2
> interpArith env (Bin Minus arith1 arith2)     = (-) <$> interpArith env arith1 <*> interpArith env arith2
> interpArith env (Bin Times arith1 arith2)     = (*) <$> interpArith env arith1 <*> interpArith env arith2
> interpArith env (Bin Divide arith1 arith2)    = interpArith env arith2 >>= \v ->
>                                                   case v of
>                                                   0 -> Left DivideByZero
>                                                   _ -> (/) <$> interpArith env arith1 <*> Right v
> interpArith env (Bin Exponent arith1 arith2)  = (**) <$> interpArith env arith1 <*> interpArith env arith2 -- Exponentiation
> interpArith env (Trig Sin arith)              = sin <$> interpArith env arith 
> interpArith env (Trig Cos arith)              = cos <$> interpArith env arith 
> interpArith env (Trig Tan arith)              = tan <$> interpArith env arith 
> interpArith env (Trig Log arith)              = log <$> interpArith env arith 
> interpArith env (Trig Abs arith)              = abs <$> interpArith env arith 


 interpArith env (Time time unit)




> description :: String
> description = unlines
>   [ " "
>   ,  "math time :("
>   , "Features this calculator supports: math things. hopefully."
>   , "Type an expression, :help, or :quit."
>   ]

> helpMsg :: String
> helpMsg = unlines
>   [ "You can use integers or floating point values, pi and e (but only at the end of an expression)",
>     "negation, or standard arithmetic operators + - * / ^ ",
>     "if that doesn't work, try a TI-84"
>   ]

-- makes things look nice

!!!!! parentheses 

> prettyPrint :: Arith -> String
> prettyPrint E = "e"
> prettyPrint Pi = "π"
> prettyPrint (Lit x)                      = show x
> prettyPrint (Neg x)                      = " - " ++ prettyPrint x
> prettyPrint (Bin Plus arith1 arith2)     = "(" ++ prettyPrint arith1 ++ " + " ++ prettyPrint arith2 ++ ")"
> prettyPrint (Bin Minus arith1 arith2)    = prettyPrint arith1 ++ " - " ++ prettyPrint arith2
> prettyPrint (Bin Times arith1 arith2)    = prettyPrint arith1 ++ " * " ++ prettyPrint arith2
> prettyPrint (Bin Divide arith1 arith2)   = prettyPrint arith1 ++ " / " ++ prettyPrint arith2
> prettyPrint (Bin Exponent arith1 arith2) = prettyPrint arith1 ++ " ^ " ++ prettyPrint arith2
> prettyPrint (Trig Sin arith)             = "sin "  ++ prettyPrint arith
> prettyPrint (Trig Cos arith)             = "cos "  ++ prettyPrint arith
> prettyPrint (Trig Tan arith)             = "tan "  ++ prettyPrint arith
> prettyPrint (Trig Log arith)             = "log "  ++ prettyPrint arith
> prettyPrint (Trig Abs arith)             = "abs "  ++ prettyPrint arith


-- from mod 5. theres more. look at later 

% > prettyPrec :: Precedence -> Associativity -> Arith -> String
% > prettyPrec _ _ (Lit x) = show x
% > prettyPrec p a (Bin op x y)
% >   | p > prec op || (p == prec op && a /= assoc op) = "(" ++ prettyPrec (prec op) (assoc op) (Bin op x y) ++ ")"
% >   | otherwise = prettyPrec (prec op) (assoc op) x ++ opToCon op ++ prettyPrec (prec op) (assoc op) y
% >
% > prettyArith :: Arith -> String
% > prettyArith = prettyPrec 0 L


> calc :: String -> String
> calc input = case parse (parseArith <* eof) input of         
>          Left l -> show l                               -- if there's a parse error, show it
>          Right r -> case interpArith M.empty r of       -- interpret it
>                      Left err -> showInterpError err    -- if there's an interpretation error, show it
>                      Right answer -> prettyPrint r ++ "\n   = " ++ printf "%f" answer 
>                      -- everything works, do the math, display it 