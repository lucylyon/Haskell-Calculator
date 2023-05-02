> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GADTs #-}

> module Calc where
> import           Parsing2
> import qualified Data.Map as M
  
 --------------------
 ---- Data Types ----        
 --------------------

 TO DO 
 implement casting
 all the type checking/inferring - look at mod 10? 

 fix the fucking parentheses. that is the one thing i missed. god damn. 
 also the middle line in pretty printing, units are gone, bring them back 


> data Arith where
>   Lit     :: Value -> Arith                  
>   Neg     :: Arith -> Arith                        
>   Pi      :: Arith                                    
>   E       :: Arith                                    
>   Trig    :: TrigOp -> Arith -> Arith
>   Length  :: Double -> Unit -> Arith 
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
>   Log :: TrigOp  -- okay so these two aren't trig but I added them later, so they go here anyways
>   Abs :: TrigOp
>   deriving (Show, Eq)

> data Unit where            -- objectively the metric system is better, but it was hard to have 
>    Inch       :: Unit      -- metric & imperial, so i decided only imperial 
>    Foot       :: Unit
>    Yard       :: Unit 
>    Mile       :: Unit
>    deriving (Show)


> data Type where
>     TypeLit :: Type
>     TypeLength :: Unit -> Type
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
>                  <|> (Lit <$> (Value <$> parseDouble <*> pure TypeLit))
>                  <|> Neg <$> (reservedOp "-" *> parseArithAtom)
>                  <|> Pi  <$ (reserved "Pi" <|> reserved "pi")
>                  <|> E   <$ reserved "e"
>               --   <|> try parseCast
>                  <|> parens parseArith

CASTING DOESNT WORK - GET IT TO? 

> parseLength :: Parser Arith
> parseLength = Lit <$> (Value <$> parseDouble <*> (TypeLength <$> parseUnit))

> parseDouble :: Parser Double 
> parseDouble = try float <|> fromIntegral <$> integer

% > parseCast :: Parser Arith
% > parseCast = Length <$> (parens parseArith) <* reservedOp "as" <*> parseUnit

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
>     (Yard       <$ reservedOp "yards")   <|>
>     (Yard       <$ reservedOp "Yard")    <|>
>     (Yard       <$ reservedOp "yard")    <|>

>     (Mile      <$ reservedOp "Miles")   <|>
>     (Mile      <$ reservedOp "miles")   <|>
>     (Mile      <$ reservedOp "Mile")    <|>
>     (Mile      <$ reservedOp "mile")    <|>
>     (Mile      <$ reservedOp "mi")      

> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>   { reservedNames = ["sin", "cos", "tan", "log", "abs", "sec", "min", "hr", "day", "mon", "yr", "as", "ft", "in"] }

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


> showTypeError :: TypeError -> String
> showTypeError TypeMismatchError = "these types don't work well together. you should change them"
> showTypeError BadExponents      = "you can't do exponentation with length. that's silly"
> showTypeError BadDivision       = "mixing lengths and division is annoying. it's not gonna happen"
> showTypeError BadMultiplication = "you can't multiply two lengths"
> showTypeError BadTrig           = "it's unecessary to do trigonometry on lengths. stop it"


> infer :: Ctx -> Arith -> Either TypeError Type
> infer ctx (Lit (Value x t))   = Right t 
> infer ctx Pi                  = Right TypeLit
> infer ctx E                   = Right TypeLit
> infer ctx (Neg arith)         = infer ctx arith
> infer ctx (Bin op a b)        = case infer ctx a of 
>                                       Left err -> Left err
>                                       Right aType -> case infer ctx b of
>                                               Left err -> Left err 
>                                               Right bType -> typeCheck op aType bType
> infer ctx (Trig trigOp arith) =  case infer ctx arith of 
>                                       Left err -> Left err 
>                                       Right TypeLit -> Right TypeLit
>                                       Right _ -> Left BadTrig
> infer ctx (Length _ unit)     = Right (TypeLength unit)



> typeCheck :: Op -> Type -> Type -> Either TypeError Type
> typeCheck Exponent TypeLit TypeLit             = Right TypeLit
> typeCheck Exponent _ _                         = Left BadExponents
> typeCheck Divide (TypeLength _) (TypeLength _) = Right TypeLit
> typeCheck Divide TypeLit     TypeLit           = Right TypeLit
> typeCheck Divide (TypeLength u) TypeLit        = Right (TypeLength u)
> typeCheck Divide _          _                  = Left BadDivision
> typeCheck Times (TypeLength u) TypeLit         = Right (TypeLength u)
> typeCheck Times TypeLit     (TypeLength u)     = Right (TypeLength u)
> typeCheck Times TypeLit     TypeLit            = Right TypeLit
> typeCheck Times  _          _                  = Left TypeMismatchError
> typeCheck _ (TypeLength u) (TypeLength _)      = Right (TypeLength u) -- the only two left are addition & subtraction
> typeCheck _ TypeLit     TypeLit                = Right TypeLit        -- and they follow the same rules 
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
> interpArith e (Neg x)                       = negate <$> interpArith e x      -- Negates an arith
> interpArith e (Length x _)                  = Right x 
> interpArith e (Bin Plus arith1 arith2)      = (+) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Minus arith1 arith2)     = (-) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Times arith1 arith2)     = (*) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Bin Divide arith1 arith2)    = interpArith e arith2 >>= \v ->
>                                                   case v of
>                                                   0 -> Left DivideByZero
>                                                   _ -> (/) <$> interpArith e arith1 <*> Right v
> interpArith e (Bin Exponent arith1 arith2)  = (**) <$> interpArith e arith1 <*> interpArith e arith2
> interpArith e (Trig Sin x)              = sin <$> interpArith e x 
> interpArith e (Trig Cos x)              = cos <$> interpArith e x 
> interpArith e (Trig Tan x)              = tan <$> interpArith e x 
> interpArith e (Trig Log x)              = log <$> interpArith e x 
> interpArith e (Trig Abs x)              = abs <$> interpArith e x

------------------------
----- Conversions ------       
------------------------

> inInches :: Double -> Unit -> Double      -- doing all the math in inches and converting it at the end. 
> inInches x Inch  = x                      -- everything else was too hard 
> inInches x Foot  = x * 12
> inInches x Yard  = x * 36
> inInches x Mile  = x * 63360

> fromInches :: Double -> Unit -> Double   -- putting everything back into whatever it was originally
> fromInches x Inch   = x                 
> fromInches x Foot   = x / 12
> fromInches x Yard   = x / 36
> fromInches x Mile   = x / 63360

> toDouble :: Value -> Double
> toDouble (Value x TypeLit)            = x
> toDouble (Value x (TypeLength unit))  = inInches x unit     

> toValue :: Double -> Type -> Value
> toValue x TypeLit             = Value x TypeLit
> toValue x (TypeLength unit)   = Value (fromInches x unit) (TypeLength unit)

------------------------
---- Display Stuff -----       
------------------------

> prettyValue :: Value -> String
> prettyValue (Value x TypeLit)           = show x
> prettyValue (Value x (TypeLength unit)) = show x ++ " " ++ prettyLength unit

> prettyPrint :: Arith -> String
> prettyPrint E                            = "e"
> prettyPrint Pi                           = "π"
> prettyPrint (Lit (Value x _))            = show x
> prettyPrint (Neg x)                      = " - " ++ prettyPrint x
> prettyPrint (Bin op arith1 arith2)       = prettyPrint arith1  ++ prettyOp op ++  prettyPrint arith2
> prettyPrint (Trig trigOp x)              = prettyTrigOp trigOp ++ prettyPrint x
> prettyPrint (Length x unit)              = show x ++ prettyLength unit

> prettyOp :: Op -> String
> prettyOp Plus         = " + "
> prettyOp Minus        = " - "
> prettyOp Divide       = " / "
> prettyOp Times        = " * "
> prettyOp Exponent     = " ^ "

> prettyTrigOp :: TrigOp -> String
> prettyTrigOp Sin = " sin "
> prettyTrigOp Cos = " cos "
> prettyTrigOp Tan = " tan "
> prettyTrigOp Log = " log "
> prettyTrigOp Abs = " abs "

> prettyLength :: Unit -> String
> prettyLength Inch     = "in"
> prettyLength Foot     = "ft" 
> prettyLength Yard     = "yd"
> prettyLength Mile     = "mi"        

> description :: String
> description = unlines
>   [ " "
>   ,  "math time :("
>   , "Features this calculator supports: more math things than last time."
>   , "ADD SHIT TO THIS. IT IS INCOMPLETE. also the help. can only do imperial math. fuck the metric system "
>   , "Type an expression, :help, or :quit."
>   ]

> helpMsg :: String
> helpMsg = unlines
>   [ "You can use integers or floating point values, pi and e (but only at the end of an expression)",
>     "negation, or standard arithmetic operators + - * / ^ ",
>     "if that doesn't work, try a TI-84"
>   ]

> calc :: String -> String
> calc input = case parse (arith <* eof) input of
>     Left err -> show err
>     Right expr -> case infer M.empty expr of
>         Left typeErr ->  showTypeError typeErr
>         Right r -> case interpArith M.empty expr of 
>                       Left err -> showInterpError err 
>                       Right answer -> prettyPrint expr ++ "\n  = " ++ prettyValue (toValue answer r) 
