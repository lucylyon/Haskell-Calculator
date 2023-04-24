> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GADTs #-}

> module Calc where
> import           Parsing2
> import qualified Data.Map as M

  
 --------------------
 ---- Data Types ----        
 --------------------

> data Arith where
>   Lit     :: Value -> Arith                  
>   Neg     :: Arith -> Arith                        
>   Pi      :: Arith                                    
>   E       :: Arith                                    
>   Trig    :: TrigOp -> Arith -> Arith
>   Length  :: Arith -> Units -> Arith 
>   Bin     :: Op -> Arith -> Arith -> Arith           
>   deriving (Show)                                 
>                                                   
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

** ADD CENTIMETERS **

> data Units where
>    Meters :: Units
>    Kilometers :: Units
>    Miles :: Units
>    Feet :: Units
>    Inches :: Units
>    deriving (Show)

> data Type where
>     TypeLit :: Type
>     TypeLength :: Units -> Type
>     deriving (Show)

> data Value where
>    Value :: Double -> Type -> Value
>    deriving (Show)


-----------------------
------ Parsers --------       
-----------------------

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
> parseArithAtom = try parseLength
>                  <|> (Lit <$> (Value <$> double <*> pure TypeLit))
>                  <|> Neg <$> (reservedOp "-" *> parseArithAtom)
>                  <|> Pi  <$ (reserved "Pi" <|> reserved "pi")
>                  <|> E   <$ reserved "e"
>               --   <|> try parseCast
>                  <|> parens parseArith

CASTING DOESNT WORK - GET IT TO? 

> parseLength :: Parser Arith
> parseLength = Lit <$> (Value <$> double <*> (TypeLength <$> parseUnits))

% > parseCast :: Parser Arith
% > parseCast = Length <$> (parens parseArith) <* reservedOp "as" <*> parseUnits

> parseUnits :: Parser Units
> parseUnits =
>     (Miles      <$ reservedOp "miles")   <|>
>     (Miles      <$ reservedOp "mile")    <|>
>     (Miles      <$ reservedOp "mi")      <|>
>     (Meters     <$ reservedOp "meters")  <|>
>     (Meters     <$ reservedOp "meter")   <|>
>     (Meters     <$ reservedOp "m")       <|>
>     (Kilometers <$ reservedOp "km")      <|>
>     (Feet       <$ reservedOp "foot")    <|>
>     (Feet       <$ reservedOp "feet")    <|>
>     (Feet       <$ reservedOp "ft")      <|>
>     (Inches     <$ reservedOp "inches")  <|>
>     (Inches     <$ reservedOp "inch")    <|>
>     (Inches     <$ reservedOp "in")

> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>   { reservedNames = ["sin", "cos", "tan", "log", "abs", "sec", "min", "hr", "day", "mon", "yr", "as", "ft", "in"] }

> parens :: Parser a -> Parser a
> parens     = getParens lexer

> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer

> reserved :: String -> Parser ()
> reserved = getReserved lexer

> double :: Parser Double               -- CHANGE THIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
> double     = toDouble <$> getNaturalOrFloat lexer     -- ORIGINAL - PARSE DOUBLE
>    where
>        toDouble (Left i) = fromIntegral i
>        toDouble (Right f) = f

> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer

> integer :: Parser Integer
> integer    = getInteger lexer

> float :: Parser Double
> float = getFloat lexer

> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof 

------------------------
---- Type Checking -----       
------------------------

% > data TypeError where        THIS IS MY SHIT. CHANGE IT WHEN IT WORKS
% >   TypeMismatchError :: TypeError -- these types don't work well together. you should change them 
% >   BadExponents :: TypeError -- can't do exponentation with length. thats silly
% >   BadDivision :: TypeError -- mixing lengths and division is annoying. it's not gonna happen
% >   BadMultiplication :: TypeError -- you cant multiply two lengths
% >   DoLater :: TypeError

% > showTypeError :: TypeError -> String
% > showTypeError TypeMismatchError =  "either everything has to be a measurement of length, or nothing is. you can't have it all"
% > showTypeError BadExponents      = "you can't do exponentation with length. that's silly"
% > showTypeError BadDivision       = "mixing lengths and division is annoying. it's not gonna happen"
% > showTypeError BadMultiplication = "you can't multiply two lengths"

> data InferError where                 --- NOT MINE, CHANGE TO TYPEERROR
>     MismatchedUnits :: InferError
>     MaxOneUnit :: InferError
>     BadDivisorUnits :: InferError
>     BadExpTypes :: InferError
>     InvalidCast :: InferError
>     deriving (Show)

> showInferError :: InferError -> String
> showInferError MismatchedUnits = "Expression requires same units on both terms"
> showInferError MaxOneUnit = "Not more than one term can be length"
> showInferError BadDivisorUnits = "Only length can be divided by length"
> showInferError BadExpTypes = "Exponent can not have units"
> showInferError InvalidCast = "Cannot cast value without units"


CHANGE ALL OF THIS 

> inferType :: Arith -> Either InferError Type
> inferType (Lit (Value _ t)) = Right t
> inferType (Bin Plus e1 e2) = inferTerms e1 e2 inferAddSub
> inferType (Bin Minus e1 e2) = inferTerms e1 e2 inferAddSub
> inferType (Bin Times e1 e2) = inferTerms e1 e2 inferMul
> inferType (Bin Divide e1 e2) = inferTerms e1 e2 inferDiv
> inferType (Bin Exponent e1 e2) = inferTerms e1 e2 inferExp
> inferType (Length  e1 u)  = inferType (Length e1 u) >>= inferCast
>     where
>         inferCast TypeLit = Left InvalidCast
>         inferCast _      = Right (TypeLength u)
> inferType (Neg x) = inferType x
> inferType Pi = Right TypeLit
> inferType E = Right TypeLit
> inferType (Trig _ _) = Right TypeLit -- MAYBE?? CHANGE? IDK


> inferTerms :: Arith -> Arith -> (Type -> Type -> Either InferError Type) -> Either InferError Type
> inferTerms e1 e2 f = do
>     u1 <- inferType e1
>     u2 <- inferType e2
>     f u1 u2


CHANGE THIS. THIS ISNT HOW I CODE. 


> inferAddSub, inferMul, inferDiv, inferExp :: Type -> Type -> Either InferError Type

> inferAddSub (TypeLength u) (TypeLength _) = Right (TypeLength u)
> inferAddSub TypeLit     TypeLit     = Right TypeLit
> inferAddSub _          _          = Left MismatchedUnits

> inferMul (TypeLength u) TypeLit     = Right (TypeLength u)
> inferMul TypeLit     (TypeLength u) = Right (TypeLength u)
> inferMul TypeLit     TypeLit     = Right TypeLit
> inferMul _          _          = Left MaxOneUnit

> inferDiv (TypeLength _) (TypeLength _) = Right TypeLit
> inferDiv (TypeLength u) TypeLit     = Right (TypeLength u)
> inferDiv TypeLit     TypeLit     = Right TypeLit
> inferDiv _          _          = Left BadDivisorUnits

> inferExp TypeLit  TypeLit  = Right TypeLit
> inferExp _       _       = Left BadExpTypes

------------------------
----- Interpreters -----       
------------------------

> type Env = M.Map String Integer 

> data InterpError where                            
>   DivideByZero :: InterpError

> showInterpError :: InterpError -> String                                          -- displays error messages
> showInterpError DivideByZero      = "you should know you can't divide by zero..."


> interpArith :: Env -> Arith -> Either InterpError Double
> interpArith _ Pi                            = Right pi
> interpArith _ E                             = Right (exp 1)                     -- e = 2.71828
> interpArith _ (Lit i) = Right $ toNumber i -- CHANGE THIS HERE!!!!!!!!!!!!!!!!
> interpArith e (Neg x)                       = negate <$> interpArith e x      -- Negates an arith
> interpArith e (Length x _)               = interpArith e x
> interpArith e (Bin Plus arith1 arith2)      = (+) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Minus arith1 arith2)     = (-) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Times arith1 arith2)     = (*) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Divide arith1 arith2)    = interpArith e arith2 >>= \v ->
>                                                   case v of
>                                                   0 -> Left DivideByZero
>                                                   _ -> (/) <$> interpArith e arith1 <*> Right v
> interpArith e (Bin Exponent arith1 arith2)  = (**) <$> interpArith e arith1 <*> interpArith e arith2 -- Exponentiation
> interpArith e (Trig Sin x)              = sin <$> interpArith e x 
> interpArith e (Trig Cos x)              = cos <$> interpArith e x 
> interpArith e (Trig Tan x)              = tan <$> interpArith e x 
> interpArith e (Trig Log x)              = log <$> interpArith e x 
> interpArith e (Trig Abs x)              = abs <$> interpArith e x



------------------------
---- Display Stuff -----       
------------------------

> showValue :: Value -> String
> showValue (Value x TypeLit)     = show x
> showValue (Value x (TypeLength u)) = show x ++ " " ++ prettyLength u

> showAs :: Type -> Double -> String
> showAs u x = showValue (toValue x u)

> prettyPrint :: Arith -> String
> prettyPrint E                            = "e"
> prettyPrint Pi                           = "π"
> prettyPrint (Lit (Value x _))            = show x
> prettyPrint (Neg x)                      = " - " ++ prettyPrint x
> prettyPrint (Bin op arith1 arith2)       = prettyPrint arith1  ++ prettyOp op ++  prettyPrint arith2
> prettyPrint (Trig trigOp x)          = prettyTrigOp trigOp ++ prettyPrint x
> prettyPrint (Length x unit) = prettyPrint x ++ prettyLength unit

> prettyOp :: Op -> String
> prettyOp Plus = " + "
> prettyOp Minus = " - "
> prettyOp Divide = " / "
> prettyOp Times = " * "
> prettyOp Exponent = " ^ "

> prettyTrigOp :: TrigOp -> String
> prettyTrigOp Sin = " sin "
> prettyTrigOp Cos = " cos "
> prettyTrigOp Tan = " tan "
> prettyTrigOp Log = " log "
> prettyTrigOp Abs = " abs "

> prettyLength :: Units -> String
> prettyLength Inches     = "in"
> prettyLength Feet       = "ft" 
> prettyLength Miles      = "mi"        
> prettyLength Meters     = "m"
> prettyLength Kilometers = "km"




> inBaseUnits :: Units -> Double
> inBaseUnits Meters     =    1.0
> inBaseUnits Kilometers = 1000.0
> inBaseUnits Miles      = 1609.344
> inBaseUnits Feet       =    0.3048
> inBaseUnits Inches     =    0.0254

> toNumber :: Value -> Double
> toNumber (Value x TypeLit)     = x
> toNumber (Value x (TypeLength u)) = x * inBaseUnits u

> toValue :: Double -> Type -> Value
> toValue x TypeLit     = Value x TypeLit
> toValue l (TypeLength u) = Value (l / inBaseUnits u) (TypeLength u)


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

> calc :: String -> String
> calc input = case parse arith input of
>     Left err -> show err
>     Right expr -> case inferType expr of
>         Left inferErr -> showInferError inferErr
>         Right r -> case interpArith M.empty expr of 
>                       Left err -> showInterpError err 
>                       Right answer -> prettyPrint expr ++ "\n  = " ++ showAs r answer 