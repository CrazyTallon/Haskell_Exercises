-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]] 
splitSort [] = []
splitSort [n] = [[n]]
splitSort (n:n':ns) = ([n,n']++passed) : (splitSort (failed))
    where
        passed = map snd (fst setTuple)
        failed = map snd (snd setTuple)
        setTuple = span ((state==).(uncurry compare)) (pairs) 
        state = compare n n'
        pairs = zip (n':ns) ns

-- Exercise 2
-- longest common sub-list of a finite list of finite lists
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList [x] = x
longestCommonSubList (xs:xs':xss) = longestCommonSubList (il:xss)
    where 
        il = intersect xs xs'

        intersect :: Eq a => [a] -> [a] -> [a]
        intersect _ []                  = []
        intersect [] _                  = []
        intersect ns ns'                = removeIntersection (removeIntersection ns ns') ns'
        
        removeIntersection :: Eq a => [a] -> [a] -> [a]
        removeIntersection _ []         = []
        removeIntersection [] xs'       = xs'
        removeIntersection xs xs'       = foldr removeCommonElement xs' xs 
        
        removeCommonElement :: Eq a => a -> [a] -> [a]
        removeCommonElement _ [] = []
        removeCommonElement x xs        | any (x==) xs == True = removeSingleElement x xs
                                        | otherwise = xs

        removeSingleElement :: Eq a => a -> [a] -> [a]
        removeSingleElement _ []        = []
        removeSingleElement x (y:ys)    | x == y    = ys
                                        | otherwise = y : removeSingleElement x ys
        --intersect xs (longestCommonSubList (xs':xss))

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress [] = False
canProgress ms = (totalCredits >= 60) && passed
    where
        passed = ((avgMark ms) >= 40) && ((failedCredits ms) <= 15)
        totalCredits = sumList (listOfCredits ms)
        
        avgMark xs = sumList (listOfMarks xs) `div` length xs
        failedCredits xs = sumList (listOfCredits (failedModules xs))
        failedModules xs = foldl (moduleTest) [] xs
                
        sumList xs =  foldl (+) 0 (xs)
        listOfMarks xs = map (mark) xs
        listOfCredits xs = map (credit) xs
        
        moduleTest :: [ModuleResult] -> ModuleResult -> [ModuleResult]
        moduleTest zs x | (mark x) < 40 = (zs ++ [x])
                        | otherwise = zs

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify [] = Third
classify ms | length ms == 4 = classification
            | length ms == 3 = classification
            | otherwise = Third
            where
                classification  | finalMark >= 70 = First
                                | (finalMark >= 68) && percentailCheck weighted 70 = First
                                | finalMark >= 60 = UpperSecond
                                | (finalMark >= 58) && percentailCheck weighted 60 = UpperSecond
                                | finalMark >= 50 = LowerSecond
                                | (finalMark >= 48) && percentailCheck weighted 50 = LowerSecond
                                | otherwise = Third

                finalMark = (sumList weightedMarks) / (sumList weightedCredits)
                
                percentailCheck xs n    | sumList(listOfCredits (comparison weighted n)) >= (sumList weightedCredits) = True
                                        | otherwise = False
                
                weightedMarks = map (weightedMark) (weighted)
                weightedCredits = listOfCredits (weighted)
                weighted = weighting ms

                sumList xs =  foldl (+) 0 xs
                listOfCredits xs = map (credit) xs

                comparison :: [ModuleResult] -> Int -> [ModuleResult]
                comparison [] n = []
                comparison (x:xs) n | (mark x) >= n = [x] ++ comparison xs n
                                    | otherwise = comparison xs n

                weightedMark :: ModuleResult -> Float
                weightedMark mr = (credit mr) * (fromIntegral (mark mr))

                weighting :: [[ModuleResult]] -> [ModuleResult]
                weighting [] = []
                weighting (x:x':xs) = concat([x'] ++ map (doubleWeight) xs) 

                doubleWeight :: [ModuleResult] -> [ModuleResult]
                doubleWeight [] = []
                doubleWeight (x:xs) = [ModuleResult ((credit x)*2) (mark x)] ++ doubleWeight xs

-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps    | diff <= (sqrt eps)     = result  
                        | y1 <= y2           = hillClimb d x1 x' eps
                        | y1 > y2            = hillClimb d x x2 eps
                where

                    result  | y1 >= y2 = x1
                            | y1 < y2 = x2

                    y1 = d x1
                    y2 = d x2
                    
                    x1  = x' - phiDistance
                    x2  = x + phiDistance

                    phiDistance = 0.618 * diff
                    diff = x' - x
  

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb d x x' eps
                    where
                        
                        d = (\x -> (-(f x)^2))
                        f = polynomialFormation xs 0

                        polynomialFormation :: [Float] -> Integer -> (Float -> Float) 
                        polynomialFormation [] _ = (\x -> 0) 
                        polynomialFormation (c:cs) i = (\x -> d x + f x)
                            where
                                d = polynomialFormation cs (i+1)
                                f = (\x -> c*(x^i))


-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence [] _ = []
executeInstructionSequence ns [] = ns
executeInstructionSequence ns (i:ins)   | i == Pop = pop ns ins
                                        | i == Duplicate = dup ns ins
                                        | (length ns) == 1 = ns
                                        | i == Multiply = multi ns ins
                                        | i == Add = add ns ins
                                        | i == Subtract = sub ns ins
                                        | otherwise = []
                                
                                        where
                                            pop (x:xs) is = executeInstructionSequence xs is
                                            dup (x:xs) is = executeInstructionSequence ([x,x] ++ xs) is
                                            multi (x:x':xs) is = executeInstructionSequence ([(x * x')] ++ xs) is
                                            sub (x:x':xs) is = executeInstructionSequence ([(x - x')] ++ xs) is
                                            add (x:x':xs) is = executeInstructionSequence ([(x + x')] ++ xs) is
                                            

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence n = dupMultInstructions ++ adders
                where
                    dupMultInstructions = addDupMult logValues
                    adders = addFinalMultipliers ((length logValues) - 1)
                    logValues = logList (fromIntegral n) 
                    
                    logList x   | x == 0 = []
                                | otherwise = [log] ++ logList d
                                where
                                    d   = x - (2^log) 
                                    log = floor $ logBase 2 x

                    addDupMult (x:xs) = noOfDupsMults x xs
                                    where 
                                        noOfDupsMults a as  | (a == 0) && any (a==) as = [Duplicate]
                                                            | (a == 0) = []
                                                            | any (a==) as = (noOfDupsMults (a-1) as) ++ [Duplicate, Multiply, Duplicate] 
                                                            | otherwise = (noOfDupsMults (a-1) as) ++ [Duplicate, Multiply]

                    addFinalMultipliers 0 = []                                        
                    addFinalMultipliers x = [Multiply] ++ addFinalMultipliers (x-1)                       

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers ns = findAllRoutes (nextOptimalList ns) (nextBranch ns [])
                where 
                    findAllRoutes xs iss    | length xs == 1 = iss
                                            | otherwise = findAllRoutes as ins
                                            where
                                                as  = nextOptimalList xs
                                                ins = concat(map (nextBranch xs) iss)

                    nextOptimalList (x:x':xs)   | x == 0 = [x'] ++ xs
                                                | a >= b = [a] ++ xs
                                                | a < b = [b] ++ xs
                                                where
                                                    a = x + x'
                                                    b = x * x'

                    nextBranch (x:x':xs) cp | x == 0 = [cp ++ [Pop], cp ++ [Add]]
                                            | a == b = [cp ++ [Add], cp++ [Multiply]]
                                            | a > b = [cp ++ [Add]]
                                            | a < b = [cp ++ [Multiply]]
                                        where
                                            a = x + x'
                                            b = x * x'
                

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = removeContained allTrueRect
                    where

                        listOfPoints = collectPoints rs
                        allPosibleRect = floodWithRect listOfPoints
                        allTrueRect = removeFalseRect listOfPoints allPosibleRect

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

                                                                        


-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = removeContained floodedElipse
                    where 
                        
                        containerRect = getContainerRectangle x y a b
                        rectPoints = getPoints containerRect
                        insidePoints = findInsidePoints x y a b rectPoints
                        floodedElipse = checkListCorners x y a b (floodWithRect insidePoints)


                        getContainerRectangle x y a b = Rectangle (x1,y1) (x2,y2)
                                where
                                    x1 = floor (x - a)
                                    y1 = ceiling (y - b)
                        
                                    x2 = floor (x + a)
                                    y2 = ceiling (y + b)


                        getPoints (Rectangle p1 p2) = concat $ map (lineOfPoints xlist) ylist
                                where
                                    xlist = [(fst p1).. (fst p2)]
                                    ylist = [(snd p1).. (snd p2)]
                                                
                                    lineOfPoints xs y = zip xs ys
                                        where
                                            ys = take (length xs) (repeat y)


                        findInsidePoints :: Float -> Float -> Float -> Float -> [(Int,Int)] -> [(Int,Int)]   
                        findInsidePoints _ _ _ _ [] = []
                        findInsidePoints x y a b ps = concat (map (checkIfIn x y a b) ps) 
                              

                        checkIfIn x y a b p | r <= 1 = [p]
                                            | otherwise = []
                                    where
                                        r = xCal + yCal
                                        xCal = (( (fromIntegral (fst p))**2 - (x**2))/(a**2))
                                        yCal = (( (fromIntegral (snd p))**2 - (y**2))/(b**2))

                        
                        floodWithRect [] = []
                        floodWithRect (p:ps) = rs ++ (floodWithRect ps)
                                    where
                                        rs = concat (map (formRect p) ([p]++ps))
                                        formRect p1 p2  | p1 == p2 = [Rectangle p1 p2]
                                                        | ((fst p1) <= (fst p2)) && ((snd p1) <= (snd p2)) = [Rectangle p1 p2] 
                                                        | ((fst p1) >= (fst p2)) && ((snd p1) >= (snd p2)) = [Rectangle p2 p1]
                                                        | otherwise = []


                        checkListCorners x y a b [] = []
                        checkListCorners x y a b rs = concat(map (checkCorners x y a b) rs)
                                    where
                                        checkCorners x y a b (Rectangle p1 p2)  | fstCheck && sndCheck = [Rectangle p1 p2]
                                                                                | otherwise = []
                                                            where
                                                                fstCheck = (tlc == [ ((fst p1),(snd p2)) ] )
                                                                sndCheck = (brc == [ ((snd p1),(fst p2)) ] )

                                                                tlc = checkIfIn x y a b ((fst p1),(snd p2))
                                                                brc = checkIfIn x y a b ((snd p1),(fst p2))


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


-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = convert (extract s)
                where
                    extract [] = []
                    extract (c:cs)  | (c == '0' || c == '1') = [c] ++ (extract cs)
                                    | otherwise = extract cs

                    convert [] = []
                    convert (c:c':cs)   | odd $ length (c:c':cs) = "Error: secret code is not an even length"
                                        | (c == '0' && c' == '0') = ['a'] ++ (convert cs)
                                        | (c == '0' && c' == '1') = ['b'] ++ (convert cs)
                                        | (c == '1' && c' == '0') = ['c'] ++ (convert cs)
                                        | (c == '1' && c' == '1') = ['d'] ++ (convert cs)
                                        | otherwise = "Error: incorrect inputs"

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
differentStream ss = cantorTheorem ss 0
                where
                    cantorTheorem [] _ = [1..]
                    cantorTheorem (s:ss) i = [v] ++ (cantorTheorem ss (i+1))
                            where 
                                v   | even $ s !! i = 1
                                    | otherwise = 0

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = uncurry f p
        where
            p = unravel n

            unravel :: Int -> (Int, Int)
            unravel x   | ((x - (m^2)) < m) = (x-(m^2),m)
                        | otherwise = (m,(m^2) + (2*m) - x)
                    where
                        m =  floor $ sqrt $ fromIntegral x

-- Exercise 15
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

