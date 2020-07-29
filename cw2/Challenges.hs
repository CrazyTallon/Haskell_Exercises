-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
{-
In this challenge my approach is as follows:
        - I convert both the Exprs App and Var into their LamExpr equivelents dirrectly using pattern matching
        - For App, I then recursively call convertLet on to the Expr that are used in its construction.
            This is outermost reduction.

        - For Let however I needed to rearrange it, to form its equivelent LamExpr equivelent which is a mix of LamApp and LamAbs
        - I first formed constant base of the equivelent LamAbs i (convertLet e2)) 
        - For the expression that was then LamApped together with this base I created a recursive function tailConvert that returns the 2nd expression in the LamApp constructor.
-}
convertLet (Var i) = LamVar i
convertLet (App e1 e2) = LamApp (convertLet e1) (convertLet e2)
convertLet (Let (i:is) e1 e2) = LamApp (LamAbs i (convertLet e2)) (tailConvert is e1)

                                where
                                    tailConvert :: [Int] -> Expr -> LamExpr
                                    tailConvert (i:is) e = LamAbs i (tailConvert is e)
                                    tailConvert [] e = convertLet e
                                    --tailConvert is recursive in that it takes the tail list of the main function then creates a new LamAbs for each value in that list.
                                    --Once the tail list has been emptied it then converts the expression it was given originally and places it in the final LamAbs.


-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
{-
In this Challenge:
        - Var and Let are simply converted into their respective prettyPrint String form, no brackets are added, Let has a helper function to convert its [Int] to a pretty String
        - App is where () are applied as it is only when expressions are applied that brackets are needed. The rules to those brackets are stated under the App pattern match.
-}
prettyPrint (Var i) = "x" ++ (show i)
prettyPrint (App e1 e2) = (appPrint e1 e2)
                                    where
                                        appPrint :: Expr -> Expr -> String
                                        appPrint (Var i) (App a1 a2) = (prettyPrint (Var i)) ++ " (" ++ (prettyPrint (App a1 a2)) ++ ")"
                                        appPrint (App a1 a2) (App e1 e2) = "(" ++ (prettyPrint (App a1 a2)) ++ ") (" ++ (prettyPrint (App e1 e2)) ++ ")"
                                        appPrint (Let is a1 a2) e = "(" ++ (prettyPrint (Let is a1 a2)) ++ ") " ++ (prettyPrint e)
                                        appPrint e (Let is a1 a2) = (prettyPrint e) ++ " (" ++ (prettyPrint (Let is a1 a2)) ++ ")"
                                        appPrint e1 e2 = (prettyPrint e1) ++ " " ++ (prettyPrint e2)
                                        {-Rules:
                                            If a Var precedes an App - The App is closed in brackets.
                                            If there are 2 App then both are closed in brackets.
                                            If a Let is present in anyway - The Let is closed in Brackets while prettyPrint is called on the other Exprs.
                                            Otherwise do not include brackets and simply prettyPrint the Exprs.
                                        -}  

prettyPrint (Let is e1 e2) = "let " ++ (prettyPrintList is) ++ "= " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
                                    where
                                        prettyPrintList :: [Int] -> String
                                        prettyPrintList [] = ""
                                        prettyPrintList (i:is) = "x" ++ (show i) ++ " " ++ (prettyPrintList is)
                                        --A simple helper function to convert the [Int] into a string
                                        --i.e. [1,2,5] -> "x1 x2 x5 "



-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
-- Setup function for the parser, and returns desired format.
parseLet s  | r == "" = Just expr
            | otherwise = Nothing
            where
                (expr, r) = getFirstExpr $ parse (exprParser) s
                
-- method to extract the tuple from the parse result without erroring if the parse fails to pattern match anything.
getFirstExpr :: [(Expr, String)] -> (Expr, String)
getFirstExpr [] = (Var 0, "Nothing")
getFirstExpr (x:xs) = x

-- Base Parser
exprParser :: Parser Expr
exprParser = appExpr <|> letExpr <|> varExpr <|> bracketExpr
deepAppParser = letExpr <|> varExpr <|> bracketExpr

-- Pattern matches for 2 expressions from deepAppParser which lacks appExpr. If it was done via exprParser, It would fall into an infinite loop.
-- However this does not prevent the following (App (App x1 x2) (App x1 x2)) due to bracketExpr at the end, this effectively changes the priority of the parser
-- from appExpr being first to instead being last. 
appExpr :: Parser Expr
appExpr = do
            space
            e1 <- deepAppParser
            space 
            e2 <- deepAppParser
            er <- many deepAppParser
            let e = leftGroup application ([e1] ++ [e2] ++ er)
            return e

-- Takes a function accepting 2 inputs of type a and a list of type a; outputing a single value of type a processing the values in the list by calling foldl f on it
leftGroup :: (a -> a -> a) -> [a] -> a
leftGroup f [] = error "No expressions to Group!"
leftGroup f (x:xs)  | (length ([x] ++ xs)) == 1 = x
                    | otherwise = foldl f x xs

application :: Expr -> Expr -> Expr
application e1 e2 = App e1 e2             


-- Pattern matches for let x1 x2.... = Expr in Expr where x1 and x2 could be any Variables and returns the equivelent 
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

-- Used to convert a list of Var instances into a list of their stored Ints.
fromVarExpr :: [Expr] -> [Int]
fromVarExpr v = map getInt v
                where
                    getInt :: Expr -> Int
                    getInt (Var i) = i
                    
-- Designed to convert a xNo. into it's equivelent Variable instantiation
varExpr :: Parser Expr
varExpr = do 
            space
            char 'x'
            n <- natural
            return (Var n)
            
-- Patern matching for Brackets with an expression inside it
bracketExpr :: Parser Expr
bracketExpr = do
                symbol "("
                e1 <- exprParser
                symbol ")"
                return e1


-- Challenge 4
-- count reductions using two different strategies 
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution
countReds e limit   | (lmrc <= limit) && (rmrc <= limit) = (Just lmrc, Just rmrc)   
                    | (lmrc <= limit) && (rmrc > limit) = (Just lmrc, Nothing)
                    | (lmrc <= limit) && (rmrc <= limit) = (Nothing, Just rmrc)
                    | otherwise = (Nothing, Nothing)
            where
                
                (e1, lmrc) = leftMostReduction e
                (e2, rmrc) = rightMostReduction e

                leftMostReduction :: LamExpr -> (LamExpr, Int)

                -- Given that (LamApp (LamAbs x e1) e2) is pattern matched then it first reduces the internal expression inside the LamAbs as much as possible. 
                -- then calls the reduction method and adds 1 to the total reduction count from the inside expression.
                leftMostReduction (LamApp (LamAbs x e1) e2) = (e, (1 + rc))
                                                    where
                                                        (ei, rc) = leftMostReduction e1
                                                        e = abstractionReduction (LamAbs x ei) e2

                {- Gets the recursive call of the left internal expression and 
                has lazy evaluation for recursive call of the right expression and the recursive call on the LamApp of the resulting left expression and the original right expression,
                It only evaluates the new LamAbs expr if the resulting left expression is a LamAbs expression when it is determined that the resulting left expression is a LamAbs, meaning it can be reduced further by the above pattern match.
                Otherwise it just returns LamApp of the resulting left and right expressions and the total reduction count during their reduction.
                -}
                leftMostReduction (LamApp e1 e2)    | abstractionCheck lRExpr = (expr, (lRCount + rCount))
                                                    | otherwise = ((LamApp lRExpr rRExpr), (lRCount + rRCount))
                                                    where
                                                        (lRExpr, lRCount) = leftMostReduction e1
                                                        (rRExpr, rRCount) = leftMostReduction e2
                                                        (expr, rCount) = leftMostReduction (LamApp lRExpr e2)                  

                -- No complex requirments for these pattern matches - Just returning the nessary instantiation 
                leftMostReduction (LamVar i) = (LamVar i, 0)
                leftMostReduction (LamAbs x e) = ((LamAbs x e1), rc)
                                                    where
                                                        (e1, rc) = leftMostReduction e

            
                rightMostReduction :: LamExpr -> (LamExpr, Int)
                
                -- Given that (LamApp (LamAbs x e1) e2) is pattern matched then it first reduces all internal expressions inside as much as possible 
                -- then calls the reduction method and adds 1 to the total reduction count from the inside expressions.
                rightMostReduction (LamApp (LamAbs x e1) e2) = (e, (1 + rc + lc))
                                                    where
                                                        (el, rc) = rightMostReduction e1
                                                        (er, lc) = rightMostReduction e2
                                                        e = abstractionReduction (LamAbs x el) er

                {- Gets the recursive call of the 2 internal expressions and has lazy evaluation for when recursive call on the LamApp of the 2 resulting expressions,
                It is only evaluated when it is determined that the resulting left expr is a LamAbs, meaning it can be reduced further by the above pattern match.
                Otherwise it just returns LamApp of those expressions and the total reduction count during their reduction.
                -}
                rightMostReduction (LamApp e1 e2)   | abstractionCheck lRExpr = (expr, (lRCount + rRCount + rCount))
                                                    | otherwise = ((LamApp lRExpr rRExpr), (lRCount + rRCount))
                                                    where
                                                        (lRExpr, lRCount) = rightMostReduction e1
                                                        (rRExpr, rRCount) = rightMostReduction e2
                                                        (expr, rCount) = rightMostReduction (LamApp lRExpr rRExpr)   

                -- No complex requirments for these pattern matches - Just returning the nessary instantiation 
                rightMostReduction (LamVar i) = (LamVar i, 0)
                rightMostReduction (LamAbs x e) = ((LamAbs x e1), rc)
                                                    where
                                                        (e1, rc) = rightMostReduction e

                -- The method used to reduce a given LamAbs and the desired substitution method.
                abstractionReduction :: LamExpr -> LamExpr -> LamExpr
                abstractionReduction (LamAbs x e1) e = substitution e e1 x
                            where
                                substitution :: LamExpr -> LamExpr -> Int -> LamExpr
                                substitution e (LamVar i) x     | i == x = e
                                                                | otherwise = (LamVar i)
                                substitution e (LamApp e1 e2) x = LamApp (substitution e e1 x) (substitution e e2 x) 
                                substitution e (LamAbs i e1) x  = LamAbs i (substitution e e1 x)

                -- A method to help me check if an expression is of the instance LamAbs.
                abstractionCheck :: LamExpr -> Bool
                abstractionCheck (LamAbs a b) = True
                abstractionCheck e = False



-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
-- returns the expr directly, does not have an intermediate grammer (having now programmed this i now understand why it would be so useful).
compileArith s  | r == "" = Just expr
                | otherwise = Nothing
                where
                    (expr, r) = getFirstLam $ parse (arithmeticExpression) s 


-- performs the same function as getFirstExpr buf for LamExprs
getFirstLam :: [(LamExpr, String)] -> (LamExpr, String)
getFirstLam [] = (LamVar 0, "Nothing")
getFirstLam (x:xs) = x

-- base Parser
arithmeticExpression :: Parser LamExpr
arithmeticExpression = value <|> section

-- pattern matching for section - s is the successor function equivelent
section :: Parser LamExpr
section = do
        symbol "("
        symbol "+"
        v <- value
        symbol ")"
        let s = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))
        return (LamApp v s)

-- v1(Section Value) v2(Natural) v3(Value "+" Value) v4(Brackets)
-- base parser for value - valueDeep was used as when trying to perform "1+1" if v2(natural) is ahead of v3(Value "+" Value) it evaluates to (LamVar 1) 
-- but if v3 is ahead of v2 it went into an infinte loop, hense valueDeep is used in v3 to prevent this and has a modification to allow for value + value + value....
value :: Parser LamExpr
value = v1 <|> v3 <|> v2 <|> v4
valueDeep = v1 <|> v2 <|> v4

-- v1(Section Value) returns the equivelent in LamExpr
v1 :: Parser LamExpr
v1 = do 
        e1 <- section
        e2 <- value
        return (LamApp e1 e2)

-- v2 (Natural) returns a natural number that is then converted into a list of [LamVar 1,LamVar 1...] with a LamVar 2 attached at the end, 
-- then using RightInnerMost ordering, I grouped them into LamApps before finally LamAbs 1 (LamAbs 2 e) 
v2 :: Parser LamExpr
v2 = do
        i <- natural
        let l = (map (makeLamVar) $ take i [1,1..]) ++ [LamVar 2]
        return (LamAbs 1 (LamAbs 2 (rightGroupLam l)))

-- Used to convert an int into a LamVar
makeLamVar :: Int -> LamExpr
makeLamVar i = LamVar i

-- RightInnerMost grouping
rightGroupLam :: [LamExpr] -> LamExpr
rightGroupLam [] = error "No expressions to Group!"
rightGroupLam (x:xs)    | (length ([x] ++ xs)) == 1 = x
                        | otherwise = (LamApp x (rightGroupLam xs))

-- v3(Value "+" Value) this is where i discovered having an intermediate grammer would have been very useful.
-- it take a minimum of 2 valueDeeps with a "+" between them and then can take as many "+" valueDeeps as it likes before combining them all into one list
-- Then through LeftInnerMost grouping combined them into LamApps with a s(the Successor function) between each and every value.
v3 :: Parser LamExpr
v3 = do
        space
        e1 <- valueDeep
        symbol "+"
        e2 <- valueDeep
        space
        el <- many v3repeat
        let e = [e1] ++ [e2] ++ el
        return $ leftGroup lamAdder e

-- The parser to allow for a repeat of "+" value at the end of value "+" value
v3repeat :: Parser LamExpr
v3repeat = do 
            symbol "+"
            e <- valueDeep
            space
            return e

-- Used in v3 with leftGroup to 'add' the values together.
lamAdder :: LamExpr -> LamExpr -> LamExpr
lamAdder e1 e2 = LamApp (LamApp e1 s) e2
                where
                    s = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))

-- Bracket parser to check for brackets and return the inside value.
v4 :: Parser LamExpr
v4 = do 
        symbol "("
        e <- value
        symbol ")"
        return e



