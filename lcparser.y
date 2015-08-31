
{
module LCParser where
import Data.Char
import System.Random
import System.IO.Unsafe
}



%name lcparse
%tokentype { Token }
%error { parseError }

%token 
      var             { TokenVar $$ }
      '='             { TokenEq }
      '.'             { TokenBody }
      ';'             { TokenSc }
      '\\'            { TokenLambda }
      '['             { TokenOB }
      ']'             { TokenCB }

%right var '=' '.' ';' '\\' ']' '['
 
%monad { P }{ thenP }{ returnP }
%lexer { lexer }{ TokenEOF }

%%

Program   : Line                             { Pr [$1 (E [] [])] }
          | Program Line                     { Pr (($2 (E [] (getEnv $1))):(getLines $1)) }

Line      : var '=' Explist ';'              { \e -> Eq $1 ($3 e) } 
          | var '=' Exp ';'                  { \e -> Eq $1 ($3 e) }

            
Varlist   : var                              { [$1] }
          | var Varlist                      { $1:$2 }


Explist   : Exp Exp                          { \e -> A ($1 e) ($2 e) }
          | Explist Exp                      { \e -> A ($1 e) ($2 e) }
          
Exp       : '\\' Varlist '.' Explist         { lambdafy $2 $4 }
          | '\\' Varlist '.' Exp             { lambdafy $2 $4 }
          | '[' Explist ']'                  { $2 }
          | '[' Exp ']'                      { $2 }
          | var          {% \s p -> Ok (\(E l g) -> case lookup $1 l of
                                             Just i -> F i
                                             Nothing -> case lookup $1 g of
                                               Nothing -> error $ errorString 
                                                        ("Unbound identifier \"" ++
                                                         $1 ++ "\"") s p 
                                               Just i -> i ) }
{
pep = [
  "It's ok. No big deal.",
  "You have a vision; don't stop here. You can do it!",
  "Why don't you take a break and contemplate how awesome you are for a bit.",
  "It's just an error.",
  "Almost perfect code!",
  "Nice work so far.",
  "Why not smoke a blunt and have some coffee?",
  "Beep.\a",
  "I bet there's only 1 error.",
  "If I can write this, you can write that, for sure.",
  "Go programmer go!",
  "Probably just cosmic rays.",
  "Just one tiny error.",
  "Such a fine error!",
  "You know, this is just an opportunity to learn something.",
  "I think you dropped a bracket.",
  "RA RA RA!",
  "Don't quit!"
  ]


getPep :: String
getPep = "\n" ++ (pep !! (fst (randomR (0::Int, length pep - 1) (unsafePerformIO getStdGen)))) ++ "\n"
        
            
parseError :: Token -> P a
parseError t = failP ("Parse error on " ++ show t)

             
data Exp
  = A Exp Exp
  | L String Exp
  | F Int

data Line
  = Eq String Exp

data Program
  = Pr [Line]

lambdafy :: [String] -> (Env -> Exp) -> (Env -> Exp)
lambdafy [] f e = f e
lambdafy (h:t) f e = this
  where this = L h bdy
        bdy = lambdafy t f e'
        e' = addLocal e h
        
addLocal :: Env -> String -> Env
addLocal (E l g) h = E ((h,0):(map (\(s,i) -> (s,i+1)) l)) g

getLines :: Program -> [Line]
getLines (Pr l) = l

getEnv :: Program -> [(String, Exp)]
getEnv (Pr []) = []
getEnv (Pr ((Eq nm e):t)) = (nm, e):(getEnv (Pr t))


-- The monad
data ParseResult a = Ok a | Failed String
data ParsePosition = Location String Int Int
data Env = E [(String,Int)]  [(String,Exp)]
type P a = String -> ParsePosition -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s p ->
  case m s p of 
   Ok a -> k a s p
   Failed er -> Failed er

returnP :: a -> P a
returnP a = \s p -> Ok a

failP :: String -> P a
failP err = \s p -> Failed (errorString err s p)

errorString err s p = ((errorPrefix s p) ++ err ++ getPep)

errorPrefix :: String -> ParsePosition -> String
errorPrefix s (Location nm ln cr) = "\n" ++ nm ++ ":" ++ show ln ++ ":" ++ show cr ++
                                        "\nAt or near \"" ++ take 10 s ++ "\"\n"


unpackP :: P a -> String -> ParsePosition -> a
unpackP x s p = upp (x s p)
  where upp (Ok a) = a
        upp (Failed e) = error e




-- Tokens and lexer.
data Token
      = TokenEq
      | TokenBody
      | TokenLambda
      | TokenVar String
      | TokenSc
      | TokenOB
      | TokenCB
      | TokenEOF
      deriving Show


lexer :: (Token -> P a) -> P a
lexer cont [] p = cont TokenEOF "" p
lexer cont s@(c:cs) p 
      | isSpace c = lexer cont cs (addposChar p c)
      | isAlphaNum c = cont (TokenVar var) rest (addposWord p var)
  where (var,rest) = span isAlphaNum s
lexer cont s@(c:cs) poss = toke
  where np = (addposChar poss c)
        toke = case c of
          ';' -> cont TokenSc cs np
          '.' -> cont TokenBody cs np
          '\\' -> cont TokenLambda cs np 
          '[' -> cont TokenOB cs np
          ']' -> cont TokenCB cs np
          '=' -> cont TokenEq cs np
          _ -> failP ("Unknown character '" ++ [c] ++ "'") s poss



-- Utility.
addposChar :: ParsePosition -> Char -> ParsePosition
addposChar (Location nm ln cr) c = if c == '\n' then Location nm (ln + 1) 1 else Location nm ln (cr + 1)

addposWord :: ParsePosition -> String -> ParsePosition
addposWord l = foldl addposChar l

lcparseString :: String -> Program
lcparseString s = unpackP lcparse s (Location "<string>" 1 1)

lcparseFile :: String -> IO Program
lcparseFile fn = do f <- readFile fn
                    return (unpackP lcparse f (Location fn 1 1))

programToString :: Program -> String
programToString (Pr []) = ""
programToString (Pr (h:t)) = lineToString h ++ programToString (Pr t)

getLineString :: Program -> String -> String
getLineString p s = case lookup s (getEnv p) of
  Just e -> expToString e
  Nothing -> error "Failed lookup in lineToString"


lineToString :: Line -> String
lineToString (Eq s exp) = s ++ " = " ++ expToString exp ++ ";\n"

expToString :: Exp -> String
expToString (A func arg) = "[" ++ expToString func ++ " " ++ expToString arg ++ "]"
expToString (L nm bdy) = "[\\" ++ nm ++ "." ++ expToString bdy ++ "]"
expToString (F i) = show i


}
 
 
