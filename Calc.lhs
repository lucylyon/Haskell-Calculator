 {-# OPTIONS_GHC -Wall #-}

> {-# LANGUAGE GADTs #-}

> module Calc where
> import           Parsing2
> import qualified Data.Map as M
> import Text.Printf

  
 --------------------
 ---- Data Types ----        
 --------------------

> data Arith where
>   Lit     :: Double -> Arith                  -- this is basically the same as what we did in class, 
>   Neg     :: Arith -> Arith                           -- but Lit is a double, and Neg allows negatives
>   Pi      :: Arith                                    -- supposed to be pi, but not fully
>   E       :: Arith                                    -- "" e
>   Trig    :: TrigOp -> Arith -> Arith
>   Length  :: Arith -> LengthUnit -> Arith 
>   Bin     :: Op -> Arith -> Arith -> Arith            -- if there is anything after pi or e, it doesn't look at them
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

% >   Mile :: LengthUnit
% >   Kilometer :: LengthUnit
% >   Meter :: LengthUnit
% >   Centimeter :: LengthUnit

>   deriving (Show, Eq)


> data Type where             -- type checking based off Mod 10
>   TypeLit  :: Type
>   TypeLength :: LengthUnit -> Type
>   TypeOther :: Type
>   deriving (Show, Eq)

> data Value where                               
>   Value :: Double -> Type -> Value

> data InterpError where                            
>   DivideByZero :: InterpError


> type Env = M.Map String Integer 


 ---------------------
 ----- Parsers -------       
 ---------------------

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

> parseLength :: Parser Arith 
> parseLength = Length <$> parseLit <*> parseLengthUnit

> parseLengthUnit :: Parser LengthUnit
> parseLengthUnit = (Inch <$ reservedOp "in") <|> (Foot <$ reservedOp "ft")

> parseConvert :: Parser Arith
> parseConvert = Length <$> parens parseLit <* reservedOp "as" <*> parseLengthUnit

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

> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> identifier :: Parser String
> identifier = getIdentifier lexer


 ------------------------
 ---- Type Checking -----       
 ------------------------

!!!!!! REDO THIS!!!!!!!!!

> inferType :: Arith -> Either TypeError Type
> inferType (Lit t) = Right TypeLit
> inferType (Bin Plus e1 e2) = inferTerms e1 e2 checkPlusMinus
> inferType (Bin Minus e1 e2) = inferTerms e1 e2 checkPlusMinus
> inferType (Bin Times e1 e2) = inferTerms e1 e2 checkTimes
> inferType (Bin Divide e1 e2) = inferTerms e1 e2 checkDiv
> inferType (Bin Exponent e1 e2) = inferTerms e1 e2 checkExp
> inferType (Length l unit) = case inferType l of 
>                             Left l -> Left l 
>                             Right r -> Right (TypeLength unit)
> inferType _ = Right TypeOther

> inferTerms :: Arith -> Arith -> ( Type -> Type -> Either TypeError Type) -> Either TypeError Type
> inferTerms e1 e2 f = do
>    u1 <- inferType e1
>    u2 <- inferType e2
>    f u1 u2

> checkPlusMinus :: Type -> Type -> Either TypeError Type
> checkPlusMinus (TypeLength unit) (TypeLength unit2) = Right (TypeLength unit)
> checkPlusMinus TypeLit     TypeLit     = Right TypeLit
> checkPlusMinus _          _          = Left TypeMismatchError

> checkTimes :: Type -> Type -> Either TypeError Type
> checkTimes (TypeLength unit) TypeLit     = Right (TypeLength unit)
> checkTimes TypeLit     (TypeLength unit) = Right (TypeLength unit)
> checkTimes TypeLit     TypeLit     = Right TypeLit
> checkTimes _          _          = Left BadMultiplication


> checkDiv :: Type -> Type -> Either TypeError Type
> checkDiv (TypeLength _) (TypeLength _) = Right TypeLit
> checkDiv (TypeLength unit) TypeLit     = Right (TypeLength unit)
> checkDiv TypeLit     TypeLit     = Right TypeLit
> checkDiv _          _          = Left BadDivision


> checkExp :: Type -> Type -> Either TypeError Type
> checkExp TypeLit  TypeLit  = Right TypeLit
> checkExp _       _       = Left BadExponents


% > typeCheck :: Op -> Type -> Type -> Either TypeError Type
% > typeCheck _ TypeLit TypeLit = Right TypeLit
% > typeCheck Exponent _ _ = Left BadExponents
% > typeCheck Divide _ _ = Left BadDivision
% > typeCheck Times (TypeLength _) (TypeLength _) = Left BadMultiplication
% > typeCheck _ (TypeLength _) (TypeLength _) = Right (TypeLength Foot)  -- what do i put here??? gives units precedence, case precedence of x y 
% > typeCheck _ _ _ = Left TypeMismatchError



> data TypeError where
>   TypeMismatchError :: TypeError -- these types don't work well together. you should change them 
>   BadExponents :: TypeError -- can't do exponentation with length. thats silly
>   BadDivision :: TypeError -- mixing lengths and division is annoying. it's not gonna happen
>   BadMultiplication :: TypeError -- you cant multiply two lengths
>   DoLater :: TypeError

% > infer ctx (Boo x)      = Right TypeBool
% > infer ctx (Bin op a b) = case (beepbOPin op) of
% >                             (input1, input2, output) -> check ctx a input1 *> 
% >                                                         check ctx b input2 *>
% >                                                         Right output     
% > infer ctx (Var x)      = case M.lookup x ctx of 
% >                             Nothing -> Left $ UnboundError x
% >                             Just v  -> Right v
% > infer ctx (Con i t e) = check ctx i TypeBool *> case (infer ctx t) >>= \th -> check ctx e th of
% >                                                       Left l -> Left l
% >                                                       _      -> (infer ctx t) >>= \th -> Right th -- Don't yell at me please :(
% > infer ctx (Let x eq i) = (infer ctx eq) >>= \t -> check ctx i t *> Right t
       

% > check :: Ctx -> Arith -> Type -> Either TypeError ()
% > check ctx arith t = case infer ctx arith of
% >                            Left l -> Left l 
% >                            Right r
% >                             | r == t -> Right ()
% >                             | otherwise -> Left TypeMismatchError



 ------------------------
 ---- Interpreters ------       
 ------------------------

> showInterpError :: InterpError -> String                                          -- displays error messages
> showInterpError DivideByZero      = "you should know you can't divide by zero..."

> showTypeError :: TypeError -> String
> showTypeError TypeMismatchError =  "either everything has to be a measurement of length, or nothing is. you can't have it all"
> showTypeError BadExponents      = "you can't do exponentation with length. that's silly"
> showTypeError BadDivision       = "mixing lengths and division is annoying. it's not gonna happen"
> showTypeError BadMultiplication = "you can't multiply two lengths"

% > data TypeError where
% >   TypeMismatchError :: TypeError -- these types don't work well together. you should change them 
% >   BadExponents :: TypeError -- can't do exponentation with length. thats silly
% >   BadDivision :: TypeError -- mixing lengths and division is annoying. it's not gonna happen
% >   BadMultiplication :: TypeError -- you cant multiply two lengths


It should be possible to add two values with units, with conversion as appropriate. It should be an error to add a value with units to a value without units.

It should be possible to multiply a value with units by a value without units, or vice versa. It should be an error to multiply two values with units.

It is an error to do exponentiation with anything other than unitless values.

You will need to change your interpreter quite a bit: it will need to keep track of which values have units attached and which do not. It also now has the possibility of generating a runtime error.

typeCheck


> interpArith :: Env -> Arith -> Either InterpError Double                         -- same as class, changed Integer to Double
> interpArith _ Pi                            = Right pi
> interpArith _ E                             = Right (exp 1)                     -- e = 2.71828
> interpArith _   (Lit x)                     = Right  x 
> interpArith e (Neg x)                       = negate <$> interpArith e x      -- Negates an arith
> interpArith e (Bin Plus arith1 arith2)      = (+) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Minus arith1 arith2)     = (-) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Times arith1 arith2)     = (*) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Divide arith1 arith2)    = interpArith e arith2 >>= \v ->
>                                                   case v of
>                                                   0 -> Left DivideByZero
>                                                   _ -> (/) <$> interpArith e arith1 <*> Right v
> interpArith e (Bin Exponent arith1 arith2)  = (**) <$> interpArith e arith1 <*> interpArith e arith2 -- Exponentiation
> interpArith e (Trig Sin arith)              = sin <$> interpArith e arith 
> interpArith e (Trig Cos arith)              = cos <$> interpArith e arith 
> interpArith e (Trig Tan arith)              = tan <$> interpArith e arith 
> interpArith e (Trig Log arith)              = log <$> interpArith e arith 
> interpArith e (Trig Abs arith)              = abs <$> interpArith e arith 
> interpArith e (Length x Foot)               = interpArith e x
> interpArith e (Length x Inch)               = interpArith e x


% > interpArith e (Bin Plus arith1 arith2)      = case inferType arith1 of 
% > Left err -> Left err
% > Right r1 -> case inferType arith2 of
% > Left err -> Left err 
% > Right r2 -> 


Either TypeError Type

(+) <$> interpArith e arith1 <*> interpArith e arith2



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
> prettyPrint E                            = "e"
> prettyPrint Pi                           = "π"
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
> prettyPrint (Length x Foot)              = prettyPrint x ++ "ft"
> prettyPrint (Length x Inch)              = prettyPrint x ++ "in"

-- from mod 5. theres more. look at later 

% > prettyPrec :: Precedence -> Associativity -> Arith -> String
% > prettyPrec _ _ (Lit x) = show x
% > prettyPrec p a (Bin op x y)
% >   | p > prec op || (p == prec op && a /= assoc op) = "(" ++ prettyPrec (prec op) (assoc op) (Bin op x y) ++ ")"
% >   | otherwise = prettyPrec (prec op) (assoc op) x ++ opToCon op ++ prettyPrec (prec op) (assoc op) y
% >
% > prettyArith :: Arith -> String
% > prettyArith = prettyPrec 0 L


% > calc :: String -> String
% > calc input = case parse (parseArith <* eof) input of         
% >          Left l -> show l                               -- if there's a parse error, show it
% >          Right r -> case interpArith M.empty r of       -- interpret it
% >                      Left err -> showInterpError err    -- if there's an interpretation error, show it
% >                      Right answer -> prettyPrint r ++ "\n   = " ++ printf "%f" answer 
% >                      -- everything works, do the math, display it 

> calc2 :: String -> String
> calc2 input = case parse (parseArith <* eof) input of 
>         Left l -> show l 
>         Right expr -> case inferType expr of 
>               Left l -> showTypeError l 
>               Right r -> case interpArith M.empty expr of 
>                     Left err -> showInterpError err 
>                     Right answer -> prettyPrint expr ++ "\n" ++ showAs r answer 

> showResult :: Either InterpError String -> String
> showResult (Left interpError) = showInterpError interpError
> showResult (Right str) = " = " ++ str 

> showAs :: Type -> Double -> String
> showAs u x = showValue (toValue x u)

> showValue :: Value -> String
> showValue (Value x TypeLit)     = show x
> showValue (Value x (TypeLength units)) = show x ++ " " ++ showUnits units

> showUnits :: LengthUnit -> String
> showUnits Foot = "ft"
> showUnits Inch = "in"

> toValue :: Double -> Type -> Value
> toValue x TypeLit     = Value x TypeLit
> toValue l (TypeLength u) = Value (l / inBaseUnits u) (TypeLength u)


toValue l (Length u) = Value (l / (inBaseUnits u)) (Length u)

> inBaseUnits :: LengthUnit -> Double
> inBaseUnits Foot       =    12.0
> inBaseUnits Inch       =    1.0


> toNumber :: Value -> Double
> toNumber (Value x TypeLit)     = x
> toNumber (Value x (TypeLength u)) = x * inBaseUnits u