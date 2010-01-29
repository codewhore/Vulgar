import Text.ParserCombinators.ReadP hiding (get) -- avoid clash with State.get
import Char
import System.IO
import System.Process
import Control.Monad.Trans
import Control.Monad.State

import System.IO.Unsafe

data Term = S | K | I
          | Application Term Term
          | K1 Term 
          | S1 Term
          | S2 Term Term
          deriving(Eq, Show)
-- S x y z = x z (y z)

next ' ' = 'a'
next 'z' = ' '
next ch = chr $ (ord ch) + 1

nextStr "" = "a"
nextStr (a:s) = (next a):s

eval (Application operator operand) = do operator' <- eval operator
                                         operand' <- eval operand
                                         apply operator' operand'
eval other = return other

apply I t = do str <- get
               put (nextStr str)
               return t
apply K t = return $ K1 t
apply (K1 t) i = do str <- get 
                    put ('a':str)
                    return t
apply S x = return $ S1 x
apply (S1 x) y = return $ S2 x y
apply (S2 x y) z = do str <- get
                      (liftIO . putStrLn) str
                      put ""
                      operator <- apply x z
                      operand <- apply y z
                      apply operator operand

ass = "( | )"
boob = "(.)(.)" -- haskell unicode support is apparently balls. "(.π.)" breaks.
dick = "8==D"

ass_combinator = string ass
boob_combinator = string boob
dick_combinator = string dick

s = do ass_combinator
       return S
k = do boob_combinator
       return K
i = do dick_combinator
       return I

application = do char '$'
                 skipMany (char ' ')
                 operator <- term
                 skipMany (char ' ')
                 operand <- term
                 skipMany (char ' ')
                 char '\n'
                 skipSpaces
                 return $ Application operator operand

term = do r <- choice [application, s, k, i]
          return r

term_s = readP_to_S term

instance Read Term where
    readsPrec p = term_s

main = do text <- hGetContents stdin
          (runStateT (eval (read text :: Term)) "")
          

