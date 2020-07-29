guardTest :: Int -> Int
guardTest x | x < 0 = -1
            | x >= 0 = 1


removeSingleElement :: Int -> [Int] -> [Int]
removeSingleElement _ [] = []            
removeSingleElement x (y:ys)    | x == y    = ys
                                | otherwise = y : removeSingleElement x ys


removeCommonElement :: Int -> [Int] -> [Int]
removeCommonElement _ [] = []       
removeCommonElement x xs        | any (x==) xs == True = removeSingleElement x xs
                                | otherwise = xs

removeIntersection :: [Int] -> [Int] -> [Int]
removeIntersection xs xs'       = foldr removeCommonElement xs' xs


intersect :: [Int] -> [Int] -> [Int]
intersect _ []                  = []
intersect [] _                  = []
intersect ns ns'                = removeIntersection (removeIntersection ns ns') ns'

--------------------------------------------------------------------------------------------

data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
moduleTest :: [ModuleResult] -> ModuleResult -> [ModuleResult]
moduleTest zs x     | (mark x) < 40 = (zs ++ [x])
                    | otherwise = zs

avgMark xs = sumList (listOfMarks xs) `div` length xs

failedCredits xs = sumList (listOfCredits (failedModules xs))
failedModules xs = foldl (moduleTest) [] xs

sumList xs =  foldl (+) 0 (xs)
listOfMarks xs = map (mark) xs
listOfCredits xs = map (credit) xs

-------------------------------------------------------------------------------------------

weightedMark mr = (credit mr) * (fromIntegral (mark mr))

weighting [] = []
weighting (x:x':xs) = [x'] ++ map (doubleWeight) xs

doubleWeight [] = []
doubleWeight (x:xs) = [ModuleResult ((credit x)*2) (mark x)] ++ (doubleWeight xs)

comparison [] _ = []
comparison (x:xs) n | x >= n = [x] ++ comparison xs n
                    | otherwise = comparison xs n


percentailCheck xs n    | ((length (comparison xs n)) * 2) >= (length xs) = True
                        | otherwise = False


-----------------------------------------------------------------------------------

polynomialFormation :: [Float] -> Integer -> (Float -> Float) 
polynomialFormation [] _ = (\x -> 0) 
polynomialFormation (c:cs) i = (\x -> d x + f x)
                where
                    d = polynomialFormation cs (i+1)
                    f = (\x -> c*(x^i))      

-------------------------------------------------------------------

data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)

logList x   | x == 0 = []
            | otherwise = [log] ++ logList d
        where
            d   = x - (2^log) 
            log = floor (logBase 2 x)

addDupMult (x:xs) = noOfDupsMults x xs
    where 
        noOfDupsMults a as  | (a == 0) && any (a==) as = [Duplicate]
                            | (a == 0) = []
                            | any (a==) as = (noOfDupsMults (a-1) as) ++ [Duplicate, Multiply, Duplicate ] 
                            | otherwise = (noOfDupsMults (a-1) as) ++ [Duplicate, Multiply]

addAdders 0 = []                                        
addAdders x = [Add] ++ addAdders (x-1)

testBranch n n' x   | n == n' = [x ++ [1], x ++[2]] 
                    | n > n' = [x ++ [1]]
                    | n < n' = [x ++ [2]]


nextBranch (x:x':xs) cp | x == 0 = [cp ++ [Pop], cp ++ [Add]]
                        | a == b = [cp ++ [Add], cp++ [Multiply]]
                        | a > b = [cp ++ [Add]]
                        | a < b = [cp ++ [Multiply]]
                    where
                        a = x + x'
                        b = x * x'

------------------------------------------------------------------------------------------------------------------

data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)


collectPoints rs = getUniquePoints [] rs
                where
                    getUniquePoints ps [] = ps
                    getUniquePoints ps (r:rs) = getUniquePoints nps rs 
                            where
                                nps = ps ++ ups
                                ups = concat $ map (removeRepeats ps) (getPoints r)
                                removeRepeats ps p  | elem p ps = []
                                                    | otherwise = [p]


getPoints (Rectangle p1 p2) = concat $ map (lineOfPoints xlist) ylist
                where
                xlist = [(fst p1).. (fst p2)]
                ylist = [(snd p1).. (snd p2)]
                                                                                        
                lineOfPoints xs y = zip xs ys
                        where
                            ys = take (length xs) (repeat y)

floodWithRect [] = []
floodWithRect (p:ps) = rs ++ (floodWithRect ps)
        where
            rs = concat (map (formRect p) ([p]++ps))
            formRect p1 p2  | p1 == p2 = [Rectangle p1 p2]
                            | ((fst p1) <= (fst p2)) && ((snd p1) <= (snd p2)) = [Rectangle p1 p2] 
                            | ((fst p1) >= (fst p2)) && ((snd p1) >= (snd p2)) = [Rectangle p2 p1]
                            | otherwise = []


removeFalseRect ps [] = []
removeFalseRect ps (r:rs)   | elem False truePoints = removeFalseRect ps rs
                            | otherwise = [r] ++ removeFalseRect ps rs
        where
            truePoints = map (getTruePoints ps) rectPoints
            rectPoints = getPoints r
            getTruePoints ps rp | elem rp ps = True
                                | otherwise = False


removeContained rs = removeOverLap [] noNegitives
        where

            noNegitives = removeNegitiveRectangles rs

            removeOverLap xs [] = xs
            removeOverLap xs (r:rs) | any (==True) ts = removeOverLap xs rs
                                    | otherwise = removeOverLap (xs ++ [r]) rs
                            where
                                ts = map (checkContained r) cs
                                cs = xs ++ rs

            checkContained (Rectangle x1 x2) (Rectangle a1 a2)  | ((fst x1) < (fst a1)) || ((snd x1) < (snd a1)) = False
                                                                | ((fst x2) > (fst a2)) || ((snd x2) > (snd a2)) = False
                                                                | otherwise = True

            removeNegitiveRectangles xs = concat $ map (removeNegitiveRect) xs
                        
            removeNegitiveRect :: Rectangle -> [Rectangle]
            removeNegitiveRect (Rectangle x1 x2)    | ((fst x1) > (fst x2)) || ((snd x1) > (snd x2)) = []
                                                    | otherwise = [Rectangle x1 x2]


-----------------------------------------------------------------------------------------------------------------

removeNegitiveRect :: Rectangle -> [Rectangle]
removeNegitiveRect (Rectangle x1 x2)    | ((fst x1) == (fst x2)) && ((snd x1) == (snd x2)) = []
                                        | ((fst x1) > (fst x2)) || ((snd x1) > (snd x2)) = []
                                        | otherwise = [Rectangle x1 x2] 

removeNegitiveRectangles xs = concat $ map (removeNegitiveRect) xs


getContainerRectangle :: Float -> Float -> Float -> Float -> Rectangle                                                        
getContainerRectangle x y a b = Rectangle (x1,y1) (x2,y2)
        where
            x1 = floor (x - a)
            y1 = ceiling (y - b)

            x2 = floor (x + a)
            y2 = ceiling (y + b)         
            
findInsidePoints :: Float -> Float -> Float -> Float -> [(Int,Int)] -> [(Int,Int)]
findInsidePoints _ _ _ _ [] = []
findInsidePoints x y a b ps = concat (map (checkIfIn x y a b) ps) 
        where      
            checkIfIn x y a b p | r <= 1 = [p]
                                | otherwise = []
                    where
                        r = (( (fromIntegral (fst p))**2 - (x**2))/(a**2)) + (( (fromIntegral(snd p))**2 - (y**2))/(b**2))

---------------------------------------------------------------------------------------------------------------------

convert [] = []
convert (c:c':cs)   | odd $ length (c:c':cs) = "Error: secret code is not an even length"
                    | (c == '0' && c' == '0') = ['a'] ++ (convert cs)
                    | (c == '0' && c' == '1') = ['b'] ++ (convert cs)
                    | (c == '1' && c' == '0') = ['c'] ++ (convert cs)
                    | (c == '1' && c' == '1') = ['d'] ++ (convert cs)
                    | otherwise = "Error: incorrect inputs"

----------------------------------------------------------------------------------------------------------------------

differentStream :: [[Int]] -> [Int]
differentStream ss = cantorTheorem ss 0
                where
                    cantorTheorem [] _ = [1..]
                    cantorTheorem (s:ss) i = [v] ++ $ cantorTheorem ss (i+1)
                            where 
                                v   | even $ s !! i = 1
                                    | otherwise = 0





------------------------------------------------------------------------------------------------------------------------
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = uncurry f p
        where
            p = unravel n

            unravel :: Int -> (Int, Int)
            unravel x   | ((x - (m^2)) < m) = (x-(m^2),m)
                        | otherwise = (m,(m^2) + (2*m) - x)
                    where
                        m =  floor $ sqrt $ fromIntegral x

----------------------------------------------------------------------------------------------------------------------

isShellTreeSum :: Int -> Bool
isShellTreeSum n = (sumOfTree == y)
        where
            (x,y) = unPair n
            sumOfTree = treeSum x

            treeSum n   | y == 1 || y == 0 = x
                        | otherwise = x + (treeSum leftTree) + (treeSum rightTree)
                    where
                        (x,y) = unPair n
                        (leftTree, rightTree) = unPair y
                        
            
            unPair n = unPairAndApply n (\x y -> (x,y))