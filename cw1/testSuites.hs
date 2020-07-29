-- Published Tests for COMP2209 exercises, assignment 1, 2018
-- (c) Andy Gravell, University of Southampton
import Exercises

-- Test Suites, one per exercise

tests :: [(String, [(String, Bool)])]
tests = 
  [ ("Exercise 1: splitSort",
      [ 
        ("splitSort [] = []", splitSort emptyIntList == []),
        ("splitSort[1,2,3,2,1,1,1] = [[1,2,3],[2,1],[1,1]]", splitSort[1,2,3,2,1,1,1] == [[1,2,3],[2,1],[1,1]])
      ]),
    ("Exercise 2: longestCommonSubList",
      [ 
        ("longestCommonSubList [[1,2,3], [0,1,3], [1,3,4]] = [1,3]", 
         longestCommonSubList [[1,2,3], [0,1,3], [1,3,4]] == [1,3]),
         ("longestCommonSubList [] = []", longestCommonSubList emptyIntListList == [])
      ]),
    ("Exercise 3: canProgress",
      [ 
        ("canProgress [(ModuleResult 40.0 50), (ModuleResult 20.0 50)]", 
          canProgress [(ModuleResult 40.0 50), (ModuleResult 20.0 50)]), 
        ("canProgress [(ModuleResult 20.0 50), (ModuleResult 20.0 50), (ModuleResult 20.0 30)] = False", 
          canProgress [(ModuleResult 20.0 50), (ModuleResult 20.0 50), (ModuleResult 20.0 30)] == False)  
      ]),
    ("Exercise 4: classify",
      [ 
         ("classify [[ModuleResult 60.0 45], [ModuleResult 60.0 45], [ModuleResult 60.0 45]] = Third", classify [[ModuleResult 60.0 45], [ModuleResult 60.0 45], [ModuleResult 60.0 45]] == Third),
         ("classify [[ModuleResult 60.0 45], [ModuleResult 60.0 55], [ModuleResult 60.0 65]] = UpperSecond", classify [[ModuleResult 60.0 45], [ModuleResult 60.0 55], [ModuleResult 60.0 65]] == UpperSecond)
      ]),
    ("Exercise 5: hillClimb",
      [ 
         ("hillClimb (\\x -> 4.0-x*x) (-2.5) 2.5 1e-10 ~= 0.0", 
          approxEqual (hillClimb (\x -> 4.0-x*x) (-2.5) 2.5 1e-10) 0.0),
         ("hillClimb (sin) 0.5 2.0 1e-10 ~= pi / 2", 
          approxEqual (hillClimb (sin) 0.5 2.0 1e-10) (pi / 2.0))
      ]),
    ("Exercise 6: nearestRoot",
      [ 
         ("nearestRoot [-18.0, 0.0, 2.0] 0.0 5.0 1e-5 ~= 3.0",
          approxEqual (nearestRoot [-18.0, 0.0, 2.0] 0.0 5.0 1e-5) 3.0),
         ("nearestRoot [-18.0, 1.0] 0.0 5.0 1e-5 ~= 18.0",
          approxEqual (nearestRoot [-18.0, 1.0] 15.0 20.0 1e-5) 18.0)
      ]),
    ("Exercise 7: executeInstructionSequence",
      [ 
        ("executeInstructionSequence [4, 5] [Add] = [9]", executeInstructionSequence [4, 5] [Add] == [9]),
        ("executeInstructionSequence [4, 5, 6, 7] [Pop, Duplicate] = [5, 5, 6, 7]", executeInstructionSequence [4, 5, 6, 7] [Pop, Duplicate] == [5, 5, 6, 7])
      ]),  
    ("Exercise 8: optimalSequence",
      [ 
        ("optimalSequence 1 = []", optimalSequence 1 == []),
        ("optimalSequence 2 = [Duplicate, Multiply]", optimalSequence 2 == [Duplicate, Multiply]) 
      ]),
    ("Exercise 9: findBusyBeavers",
      [ 
        ("findBusyBeavers [0,1] = [[Pop],[Add]]", findBusyBeavers [0,1] `elem` [[[Pop],[Add]],[[Add],[Pop]]] ),
        ("findBusyBeavers [2,2] = [[Add],[Multiply]]", findBusyBeavers [2,2] `elem` [[[Add],[Multiply]],[[Multiply],[Add]]])
      ]),
    ("Exercise 10: simplifyRectangleList",
      [ 
        ("simplifyRectangleList [Rectangle (2,2) (0,0)] = []", 
          simplifyRectangleList [Rectangle (2,2) (0,0)] == []),
        ("simplifyRectangleList [Rectangle (0,0) (2,2), Rectangle (0,0) (1,1)] = [Rectangle (0,0) (2,2)]", 
          simplifyRectangleList [Rectangle (0,0) (2,2), Rectangle (0,0) (1,1)] == [Rectangle (0,0) (2,2)])
      ]),
    ("Exercise 11: drawEllipse",
      [ 
         ("drawEllipse 0.0 0.0 0.1 0.1 = [Rectangle (0,0) (0,0)]", 
           drawEllipse 0.0 0.0 0.1 0.1 == [Rectangle (0,0) (0,0)]),
         ("drawEllipse 0.0 0.0 1.0 1.0 = [Rectangle (-1,0) (1,0),Rectangle (0,-1) (0,1)] or vice versa", 
           drawEllipse 0.0 0.0 1.0 1.0 `elem` [[Rectangle (-1,0) (1,0),Rectangle (0,-1) (0,1)],
              [Rectangle (0,-1) (0,1),Rectangle (-1,0) (1,0)]])
      ]),
    ("Exercise 12: extractMessage",
      [ 
        ("extractMessage \"B10L0G1CAL\" = \"cb\"", extractMessage "B10L0G1CAL" == "cb"),
        ("extractMessage \"HI H0W ARE YOU DO1NG?  I AM D0ING FINE, 0K!  1S 1T TIME TO GO?\" = \"bad\"", extractMessage "HI H0W ARE YOU DO1NG?  I AM D0ING FINE, 0K!  1S 1T TIME TO GO?" == "bad")
      ]),    
    ("Exercise 13: differentStream",
      [ 
        ("isDifferentStream testStream1 (differentStream testStream1) 10",
          isDifferentStream testStream1 (differentStream testStream1) 10),
        ("isDifferentStream testStream2 (differentStream testStream2) 10",
          isDifferentStream testStream2 (differentStream testStream2) 10)
      ]),
    ("Exercise 14: unPairAndApply",
      [ 
         ("unPairAndApply 13 (-) = 1", unPairAndApply 13 (-) == 1),
         ("unPairAndApply 10 max = 3", unPairAndApply 10 max == 3)
      ]),
    ("Exercise 15: isShellTreeSum",
      [ 
        ("isShellTreeSum 14", isShellTreeSum 14),
        ("isShellTreeSum 15 = False", isShellTreeSum 15 == False)
      ])
  ]

-- constants and functions for testing purposes
emptyIntList :: [Int]
emptyIntList = []

emptyIntListList :: [[Int]]
emptyIntListList = []

-- a couple of streams to test
testStream1 :: [[Int]]
testStream1 = [0..]:testStream1
testStream2 :: [[Int]]
testStream2 = repeatStream 0
repeatStream :: Int -> [[Int]]
repeatStream x = [x..]:(repeatStream (x+1))

-- test stream is actually different (looking at first n elements only)
isDifferentStream :: [[Int]] -> [Int] -> Int -> Bool
isDifferentStream nns ns n = 
    let ns' = take n ns
        nns' = map (take n) (take n nns) 
    in length ns' == 10 && 
       all (\xs -> length xs == 10) nns' &&
       not (ns' `elem` nns')

-- coarse test for approximation floating point equality 
approxEqual :: Float -> Float -> Bool
approxEqual x y = abs (x - y) < 1e-2

-- Main program checks the results of the tests and produces scores
main :: IO ()
main = 
  do
    putStrLn ""
    testSuite tests

outPrefix msg = "  " ++ msg

testSuite :: [(String, [(String,Bool)])] -> IO ()
testSuite [] = putStrLn ""
testSuite ((s,tc):ts) =
  do
    putStrLn s
    testCases tc
    putStrLn (outPrefix (message tc)) 
    testSuite ts

testCases :: [(String,Bool)] -> IO ()
testCases [] = putStr ""
testCases ((s,False):ts) =
  do
    putStr (outPrefix "Did not satisfy assertion: ")
    putStrLn s 
    testCases ts
testCases ((s,True):ts) =
  do
    testCases ts

-- Auxiliary functions to support testing 
message :: [(String,Bool)] -> String
message ts =
  let failures = [(s,b) | (s,b) <- ts , b == False] in
  if failures == [] then "All test cases passed"
  else "Failed " ++ (intToString (length failures)) ++ " out of " ++ (intToString (length ts)) ++ " test cases"

intToString :: Int -> String
intToString 0 = "0"
intToString n =
  if n < 10 then digitToString n
  else (intToString (n `div` 10)) ++ (digitToString (n `mod` 10)) 

digitToString :: Int -> String
digitToString 0 = "0"
digitToString 1 = "1"
digitToString 2 = "2"
digitToString 3 = "3"
digitToString 4 = "4"
digitToString 5 = "5"
digitToString 6 = "6"
digitToString 7 = "7"
digitToString 8 = "8"
digitToString 9 = "9"
