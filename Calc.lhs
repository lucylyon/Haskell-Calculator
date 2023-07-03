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
>   Length  :: Double -> Unit -> Arith 
>   Convert :: Arith -> Unit -> Arith 
>   Bin     :: Op -> Arith -> Arith -> Arith           
>   deriving (Show)                                 

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
>   Log :: TrigOp  -- okay so these two aren't trig but I added them after the trig stuff, so they go here anyways
>   Abs :: TrigOp
>   deriving (Show, Eq)

> data Unit where
>    Inch       :: Unit
>    Foot       :: Unit
>    Yard       :: Unit 
>    Mile       :: Unit
>    Centimeter :: Unit 
>    Meter      :: Unit
>    Kilometer :: Unit
>    deriving (Show)

> data Type where
>     TypeLit :: Type
>     TypeLength :: Unit -> Type
>     deriving (Show)

> data Value where
>    Value :: Double -> Type -> Value
>    deriving (Show)

> data Associativity where              -- precedence stuff from mod 5
>   L :: Associativity
>   R :: Associativity
>   deriving (Show, Eq)

> type Precedence = Int

> assoc :: Op -> Associativity
> assoc Exponent     = R
> assoc _       = L
>
> prec :: Op -> Precedence
> prec Plus    = 1
> prec Minus   = 1
> prec Times   = 2
> prec Divide  = 2
> prec Exponent = 3

-----------------------
------ Parsers --------       
-----------------------

> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where                                                                 -- the same as what we did in class, but cooler now
>     table = [ [ Prefix (Trig Sin      <$ reservedOp "sin") 
>               , Prefix (Trig Cos      <$ reservedOp "cos") 
>               , Prefix (Trig Tan      <$ reservedOp "tan")
>               , Prefix (Trig Log      <$ reservedOp "log") 
>               , Prefix (Trig Abs      <$ reservedOp "abs")  ]
>               , [ Infix (Bin Exponent <$ reservedOp "^") AssocRight ]    -- added exponentiation, highest precedence because PEMDAS
>               , [ Infix (Bin Times    <$ reservedOp "*") AssocLeft       -- ^ is AssocRight so it follows math rules
>               ,   Infix (Bin Divide   <$ reservedOp "/") AssocLeft ]     -- ex. 2^3^4 = 2^(3^4) instead of (2^3)^4 
>               , [ Infix (Bin Plus     <$ reservedOp "+") AssocLeft
>               ,   Infix (Bin Minus    <$ reservedOp "-") AssocLeft ]
>             ] 

> parseArithAtom :: Parser Arith
> parseArithAtom = try parseConvert
>                  <|>  try parseLength
>                  <|>  Neg <$> (reservedOp "-" *> parseArithAtom)
>                  <|> (Lit <$> (Value <$> parseDouble <*> pure TypeLit))
>                  <|> Pi  <$ (reserved "Pi" <|> reserved "pi")
>                  <|> E   <$ reserved "e"
>                  <|> parens parseArith

> parseLength :: Parser Arith
> parseLength = Lit <$> (Value <$> parseDouble <*> (TypeLength <$> parseUnit))

> parseDouble :: Parser Double 
> parseDouble = try float <|> fromIntegral <$> integer

> parseConvert :: Parser Arith
> parseConvert = Convert <$> (parens parseArith) <* (reservedOp "to" <|> reservedOp "in" <|> reservedOp "as") <*> parseUnit 

> parseUnit :: Parser Unit
> parseUnit =
>     (Inch     <$ reservedOp "Inches")    <|>
>     (Inch     <$ reservedOp "inches")    <|>
>     (Inch     <$ reservedOp "Inch")      <|>
>     (Inch     <$ reservedOp "inch")      <|>
>     (Inch     <$ reservedOp "in")        <|>

>     (Foot       <$ reservedOp "ft")      <|>
>     (Foot       <$ reservedOp "Foot")    <|>
>     (Foot       <$ reservedOp "foot")    <|>
>     (Foot       <$ reservedOp "Feet")    <|>
>     (Foot       <$ reservedOp "feet")    <|>

>     (Yard       <$ reservedOp "yd")      <|>
>     (Yard       <$ reservedOp "Yards")   <|>
>     (Yard       <$ reservedOp "yards")   <|>                           -- there is no way anyone can mess up the units 
>     (Yard       <$ reservedOp "Yard")    <|>                           -- (that is a hyperbole. people always manage to break things
>     (Yard       <$ reservedOp "yard")    <|>                           -- but i think this is a pretty intense parser) 

>     (Mile      <$ reservedOp "Miles")   <|>
>     (Mile      <$ reservedOp "miles")   <|>
>     (Mile      <$ reservedOp "Mile")    <|>
>     (Mile      <$ reservedOp "mile")    <|>
>     (Mile      <$ reservedOp "mi")      <|>

>     (Centimeter       <$ reservedOp "cm")           <|>
>     (Centimeter       <$ reservedOp "Centimeters")  <|>
>     (Centimeter       <$ reservedOp "centimeters")  <|>
>     (Centimeter       <$ reservedOp "Centimeter")   <|>
>     (Centimeter       <$ reservedOp "centimeter")   <|>

>     (Kilometer <$ reservedOp "km")            <|>
>     (Kilometer <$ reservedOp "Kilometers")    <|>
>     (Kilometer <$ reservedOp "kilometers")    <|>  
>     (Kilometer <$ reservedOp "Kilometer")     <|>
>     (Kilometer <$ reservedOp "kilometer")     <|>

>     (Meter     <$ reservedOp "Meters")  <|>
>     (Meter     <$ reservedOp "meters")  <|>
>     (Meter     <$ reservedOp "Meter")   <|>
>     (Meter     <$ reservedOp "meter")   <|>
>     (Meter     <$ reservedOp "m")       


> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>   { reservedNames = ["sin", "cos", "tan", "log", "abs", "in", "to", "as"] }

> parens :: Parser a -> Parser a
> parens     = getParens lexer

> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer

> reserved :: String -> Parser ()
> reserved = getReserved lexer

> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer

> integer :: Parser Integer
> integer    = getInteger lexer

> float :: Parser Double
> float = getFloat lexer

> identifier :: Parser String
> identifier = getIdentifier lexer

> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof 

------------------------
---- Type Checking -----       
------------------------

> type Ctx = M.Map String Type 

> data TypeError where       
>   TypeMismatchError   :: TypeError 
>   BadExponents        :: TypeError 
>   BadDivision         :: TypeError
>   BadMultiplication   :: TypeError
>   BadTrig             :: TypeError
>   BadConversion       :: TypeError


> showTypeError :: TypeError -> String
> showTypeError TypeMismatchError = "these types don't work well together. you should change them"
> showTypeError BadExponents      = "you can't do exponentation with length. that's silly"
> showTypeError BadDivision       = "mixing lengths and division is annoying. it's not gonna happen"
> showTypeError BadMultiplication = "you can't multiply two lengths"
> showTypeError BadTrig           = "it's unecessary to do trigonometry on lengths. stop it"
> showTypeError BadConversion     = "why would you try to convert that."


> infer :: Ctx -> Arith -> Either TypeError Type                -- based off mod 9
> infer _ (Lit (Value _ t))   = Right t 
> infer _ Pi                  = Right TypeLit
> infer _ E                   = Right TypeLit
> infer ctx (Neg a)           = infer ctx a
> infer ctx (Bin op a b)      = case infer ctx a of 
>                                    Left err -> Left err
>                                    Right aType -> case infer ctx b of
>                                          Left err -> Left err 
>                                          Right bType -> typeCheck op aType bType
> infer ctx (Trig _ a)         =  case infer ctx a of 
>                                    Left err -> Left err 
>                                    Right TypeLit -> Right TypeLit
>                                    Right _ -> Left BadTrig
> infer _ (Length _ unit)      = Right (TypeLength unit)
> infer ctx (Convert x unit)   = case infer ctx x of 
>                                   Left err -> Left err 
>                                   Right TypeLit -> Left BadConversion
>                                   Right _ -> Right (TypeLength unit) 

> typeCheck :: Op -> Type -> Type -> Either TypeError Type                  -- checks to see if you can actually do the operation of the two types
> typeCheck _ TypeLit TypeLit                    = Right TypeLit            -- any lit works with any lit
> typeCheck Exponent _ _                         = Left BadExponents
> typeCheck Divide (TypeLength u) TypeLit        = Right (TypeLength u)
> typeCheck Divide _          _                  = Left BadDivision
> typeCheck Times (TypeLength u) TypeLit         = Right (TypeLength u)
> typeCheck Times TypeLit     (TypeLength u)     = Right (TypeLength u)
> typeCheck Times  _          _                  = Left TypeMismatchError
> typeCheck _ (TypeLength u) (TypeLength _)      = Right (TypeLength u) -- the only two left are addition & subtraction and they follow the same rules 
> typeCheck _ _ _                                = Left TypeMismatchError

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
> interpArith _ (Lit x)                       = Right (toDouble x) 
> interpArith e (Neg x)                       = negate <$> interpArith e x        -- Negates an arith
> interpArith _ (Length x _)                  = Right x 
> interpArith e (Convert a _)                 = case (interpArith e a) of
>                                                   Left err -> Left err 
>                                                   Right r  -> Right r
> interpArith e (Bin Plus arith1 arith2)      = (+) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Minus arith1 arith2)     = (-) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Times arith1 arith2)     = (*) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Divide arith1 arith2)    = interpArith e arith2 >>= \v ->
>                                                   case v of
>                                                   0 -> Left DivideByZero
>                                                   _ -> (/) <$> interpArith e arith1 <*> Right v
> interpArith e (Bin Exponent arith1 arith2)  = (**) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Trig Sin x)                  = sin <$> interpArith e x             -- woohoo trig functions
> interpArith e (Trig Cos x)                  = cos <$> interpArith e x 
> interpArith e (Trig Tan x)                  = tan <$> interpArith e x 
> interpArith e (Trig Log x)                  = log <$> interpArith e x 
> interpArith e (Trig Abs x)                  = abs <$> interpArith e x

------------------------
----- Conversions ------       
------------------------

> inInches :: Double -> Unit -> Double      -- doing all the math in inches and converting it at the end. 
> inInches x Inch       = x                 -- everything else was too hard 
> inInches x Foot       = x * 12
> inInches x Yard       = x * 36
> inInches x Mile       = x * 63360
> inInches x Centimeter = x * 0.393701
> inInches x Meter      = x * 39.3701
> inInches x Kilometer  = x * 39370.1 

> fromInches :: Double -> Unit -> Double   -- putting everything back into whatever it was originally
> fromInches x Inch       = x                 
> fromInches x Foot       = x / 12
> fromInches x Yard       = x / 36
> fromInches x Mile       = x / 63360
> fromInches x Centimeter = x / 0.393701
> fromInches x Meter      = x / 39.3701
> fromInches x Kilometer  = x / 39370.1  

> toDouble :: Value -> Double                   -- takes a value & if it's a lit, returns it,
> toDouble (Value x TypeLit)         = x        --  if it's length, it puts it into inches
> toDouble (Value x (TypeLength u))  = inInches x u     

> toValue :: Double -> Type -> Value               -- opposite of toDouble
> toValue x TypeLit          = Value x TypeLit   
> toValue x (TypeLength u)   = Value (fromInches x u) (TypeLength u) 

------------------------
---- Display Stuff -----       
------------------------

> prettyValue :: Value -> String            -- pretty prints values 
> prettyValue (Value x TypeLit)           = show x
> prettyValue (Value x (TypeLength unit)) = show x ++ " " ++ prettyLength unit

> prettyPrint :: Precedence -> Associativity -> Arith -> String     -- pretty prints ariths
> prettyPrint _ _ E                            = "e"
> prettyPrint _ _ Pi                           = "π"
> prettyPrint _ _ (Lit x)                      =  prettyValue x
> prettyPrint p a (Neg x)                      = "-" ++ prettyPrint p a x
> prettyPrint p a (Trig trigOp x)              = prettyTrigOp trigOp ++ "(" ++ prettyPrint p a x ++ ")"
> prettyPrint _ _ (Length x unit)              = show x ++ prettyLength unit
> prettyPrint p a (Convert x unit)             = prettyPrint p a x ++ " to " ++ prettyLength unit 
> prettyPrint p a (Bin op arith1 arith2)       = 
>  if p > prec op || (p == prec op && assoc op /= a)
>     then "(" ++ prettyPrec op arith1 arith1 ++ ")"
>          else prettyPrec op arith1 arith2 

> prettyPrec :: Op -> Arith -> Arith -> String      -- checks precedence & pretty prints
> prettyPrec op e1 e2 = prettyPrint (prec op) (assoc op) e1  ++ prettyOp op ++ prettyPrint (prec op) (assoc op) e2

> prettyOp :: Op -> String
> prettyOp Plus         = " + "
> prettyOp Minus        = " - "
> prettyOp Divide       = " / "
> prettyOp Times        = " * "
> prettyOp Exponent     = " ^ "

> prettyTrigOp :: TrigOp -> String
> prettyTrigOp Sin = "sin"
> prettyTrigOp Cos = "cos"
> prettyTrigOp Tan = "tan"
> prettyTrigOp Log = "log"
> prettyTrigOp Abs = "abs"

> prettyLength :: Unit -> String
> prettyLength Inch       = "in"
> prettyLength Foot       = "ft" 
> prettyLength Yard       = "yd"
> prettyLength Mile       = "mi"      
> prettyLength Centimeter = "cm"
> prettyLength Meter      = "m"
> prettyLength Kilometer  = "km"  

> description :: String
> description = unlines
>   [ "math time :("
>   , "Features this calculator supports: more math things than last time."
>   , "such as sin, cos, tan, log, abs, and math with length (unless it's dumb math)"
>   , "Type an expression, :help, or :quit."
>   ]

> helpMsg :: String
> helpMsg = unlines
>   [ "You can use integers or floating point values, pi and e "
>     , "negation, or standard arithmetic operators + - * / ^ "
>     , "as well as using inches, feet, yards, miles, centimeters, meters, and kilometers."
>     , "You can also convert units of measurement."
>     , "if that doesn't work, try a TI-84 or google"
>   ]

> calc :: String -> String
> calc input = case parse (arith <* eof) input of
>     Left err -> show err
>     Right expr -> case infer M.empty expr of
>         Left typeErr ->  showTypeError typeErr
>         Right r -> case interpArith M.empty expr of 
>                       Left err -> showInterpError err 
>                       Right answer -> prettyPrint 0 L expr ++ "\n  = " ++  prettyValue (toValue answer r) 
