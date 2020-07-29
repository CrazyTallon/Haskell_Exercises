import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

f :: Expr -> Expr
f (Var i) = Var i
f (Let [i] e1 e2) = e1
f (App e1 e2) = e1

exprParser :: Parser Expr
exprParser = appExpr <|> letExpr <|> varExpr <|> bracketExpr
deepAppParser = letExpr <|> varExpr <|> bracketExpr


varExpr :: Parser Expr
varExpr = do 
            space
            char 'x'
            n <- natural
            return (Var n)


letExpr :: Parser Expr
letExpr = do
            symbol "let"
            v1 <- varExpr
            vi <- many varExpr
            symbol "="
            e1 <- exprParser
            symbol "in"
            e2 <- exprParser
            let nl = fromVarExpr ([v1] ++ vi)
            return (Let nl e1 e2)

fromVarExpr :: [Expr] -> [Int]
fromVarExpr v = map getInt v
                where
                    getInt :: Expr -> Int
                    getInt (Var i) = i



appExpr :: Parser Expr
appExpr = do
            space
            e1 <- deepAppParser
            space 
            e2 <- deepAppParser
            er <- many deepAppParser
            let e = rightApp ([e1] ++ [e2] ++ er)
            return e


rightApp :: [Expr] -> Expr
rightApp [] = error "No expressions to Group!"
rightApp (x:xs) | (length ([x] ++ xs)) < 2 = error "Not enough expressions to Group!"
                | otherwise = foldl application x xs
                where
                    application :: Expr -> Expr -> Expr
                    application e1 e2 = App e1 e2          
                    
                    
bracketExpr :: Parser Expr
bracketExpr = do
                symbol "("
                e1 <- exprParser
                symbol ")"
                return e1



abstractionReduction :: LamExpr -> LamExpr -> LamExpr
abstractionReduction (LamAbs x e1) e = substitution e e1 x
                            where
                                substitution :: LamExpr -> LamExpr -> Int -> LamExpr
                                substitution e (LamVar i) x     | i == x = e
                                                                | otherwise = (LamVar i)
                                substitution e (LamApp e1 e2) x = LamApp (substitution e e1 x) (substitution e e2 x) 
                                substitution e (LamAbs i e1) x  = LamAbs i (substitution e e1 x)


rightMostReduction :: LamExpr -> (LamExpr, Int)
rightMostReduction (LamApp (LamAbs x e1) e2) = (e, (1 + rc))
                                    where
                                        (ei, rc) = rightMostReduction e1
                                        e = abstractionReduction (LamAbs x ei) e2
rightMostReduction (LamApp e1 e2)   | abstractionCheck lRExpr = (expr, (lRCount + rRCount + rCount))
                                    | otherwise = ((LamApp lRExpr rRExpr), (lRCount + rRCount))
                                    where
                                        (lRExpr, lRCount) = rightMostReduction e1
                                        (rRExpr, rRCount) = rightMostReduction e2
                                        (expr, rCount) = rightMostReduction (LamApp lRExpr rRExpr)   

rightMostReduction (LamVar i) = (LamVar i, 0)
rightMostReduction (LamAbs x e) = ((LamAbs x e1), rc)
                                    where
                                        (e1, rc) = rightMostReduction e



leftMostReduction :: LamExpr -> (LamExpr, Int)
leftMostReduction (LamApp (LamAbs x e1) e2) = (e, (1 + rc))
                                    where
                                        (ei, rc) = leftMostReduction e1
                                        e = abstractionReduction (LamAbs x ei) e2
leftMostReduction (LamApp e1 e2)    | abstractionCheck lRExpr = (expr, (lRCount + rCount))
                                    | otherwise = ((LamApp lRExpr rRExpr), (lRCount + rRCount))
                                    where
                                        (lRExpr, lRCount) = leftMostReduction e1
                                        (rRExpr, rRCount) = leftMostReduction e2
                                        (expr, rCount) = leftMostReduction (LamApp lRExpr e2)                  
leftMostReduction (LamVar i) = (LamVar i, 0)
leftMostReduction (LamAbs x e) = ((LamAbs x e1), rc)
                                    where
                                        (e1, rc) = leftMostReduction e


abstractionCheck :: LamExpr -> Bool
abstractionCheck (LamAbs a b) = True
abstractionCheck e = False



lamNumber :: Int -> LamExpr
lamNumber 0 = LamVar 2
lamNumber i = LamApp (LamVar 1) (lamNumber (i-1))

makeVar :: Int -> LamExpr
makeVar i = LamVar i