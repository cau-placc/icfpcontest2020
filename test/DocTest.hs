module DocTest (runDocTests) where

import           Control.Monad (forM_)

import           Syntax
import           Interpreter
import           Interpreter.Data
import           Modulation (bitsToString)

runDocTests :: IO ()
runDocTests = do
  -- Doc 1 is just basic    numbers
  -- Doc 2 is just further  numbers
  -- Doc 3 is just negative numbers
  testDocs4
  testDocs5
  testDocs6
  testDocs7
  -- Doc 8 is introduction of variables
  testDocs9
  testDocs10
  testDocs11
  testDocs12
  testDocs13
  -- Doc 14 is demodulate and contains no testable examples
  -- Doc 15 is send, not offline/reproducible testable
  testDocs16
  testDocs17
  testDocs18
  testDocs19
  -- Doc 20 is B Combinator and contains no testable examples
  testDocs21
  -- Doc 22 is False and contains no testable examples
  testDocs23
  testDocs24
  -- Doc 25 is Cons/Pair and contains no testable examples
  -- Doc 26 is Car and contains no testable examples
  -- Doc 27 is Cdr and contains no testable examples
  testDocs28
  testDocs29
  -- Doc 30 is List Construction Syntax and contains no testable examples
  -- Doc 31 is Vector as alias of Cons and Contains no testable examples
  -- Doc 32 is Draw and Contains no testable examples
  -- Doc 33 is Checkerboard and Contains no testable examples
  testDocs34
  testDocs35
  -- TODO write tests or reason for no test for remaining doc chapters
  
  
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


testDocs4 :: IO ()
testDocs4 = do
  let tests = [ testEq (Number   0 ) (Number   0 )
              , testEq (Number   1 ) (Number   1 )
              , testEq (Number   2 ) (Number   2 )
              , testEq (Number   3 ) (Number   3 )
              , testEq (Number  10 ) (Number  10 )
              , testEq (Number  11 ) (Number  11 )
              , testEq (Number (-1)) (Number (-1))
              , testEq (Number (-2)) (Number (-2))
              ]
  runTests 4 tests

testDocs5 :: IO ()
testDocs5 = do
  let tests = [ testEq (app Inc [Number    0 ]) (Number    1 )
              , testEq (app Inc [Number    1 ]) (Number    2 )
              , testEq (app Inc [Number    2 ]) (Number    3 )
              , testEq (app Inc [Number    3 ]) (Number    4 )
              , testEq (app Inc [Number  300 ]) (Number  301 )
              , testEq (app Inc [Number  301 ]) (Number  302 )
              , testEq (app Inc [Number ( -1)]) (Number    0 )
              , testEq (app Inc [Number ( -2)]) (Number ( -1))
              , testEq (app Inc [Number ( -3)]) (Number ( -2))
              ]
  runTests 5 tests

testDocs6 :: IO ()
testDocs6 = do
  let tests = [ testEq (app Dec [Number     1 ]) (Number     0 )
              , testEq (app Dec [Number     2 ]) (Number     1 )
              , testEq (app Dec [Number     3 ]) (Number     2 )
              , testEq (app Dec [Number     4 ]) (Number     3 )
              , testEq (app Dec [Number  1024 ]) (Number  1023 )
              , testEq (app Dec [Number     0 ]) (Number (  -1))
              , testEq (app Dec [Number (  -1)]) (Number (  -2))
              , testEq (app Dec [Number (  -2)]) (Number (  -3))
              ]
  runTests 6 tests

testDocs7 :: IO ()
testDocs7 = do
  let tests = [ testEq (app Add [Number 1, Number 2]) (Number 3 )
              , testEq (app Add [Number 2, Number 1]) (Number 3 )
              , testEq (app Add [Number 0, Number 1]) (Number 1 )
              , testEq (app Add [Number 2, Number 3]) (Number 5 )
              , testEq (app Add [Number 3, Number 5]) (Number 8 )
              ]
  runTests 7 tests

testDocs9 :: IO ()
testDocs9 = do
  let tests = [ testEq (app Mul [Number  4, Number   2 ]) (Number   8 )
              , testEq (app Mul [Number  3, Number   4 ]) (Number  12 )
              , testEq (app Mul [Number  3, Number (-2)]) (Number (-6))
              ]
  runTests 9 tests

-- found error, we where using div instead of quot
testDocs10 :: IO ()
testDocs10 = do
  let tests = [ testEq (app Div [Number    4 , Number   2 ]) (Number   2 )
              , testEq (app Div [Number    4 , Number   3 ]) (Number   1 )
              , testEq (app Div [Number    4 , Number   4 ]) (Number   1 )
              , testEq (app Div [Number    4 , Number   5 ]) (Number   0 )
              , testEq (app Div [Number    5 , Number   2 ]) (Number   2 )
              , testEq (app Div [Number    6 , Number (-2)]) (Number (-3))
              , testEq (app Div [Number    5 , Number (-3)]) (Number (-1))
              , testEq (app Div [Number  (-5), Number   3 ]) (Number (-1))
              , testEq (app Div [Number  (-5), Number (-3)]) (Number   1 )
              ]
  runTests 10 tests

testDocs11 :: IO ()
testDocs11 = do
  let tests = [ assertNot $ app Eq [Number    0 , Number   ( -2)]
              , assertNot $ app Eq [Number    0 , Number   ( -1)]
              , assert    $ app Eq [Number    0 , Number      0 ]
              , assertNot $ app Eq [Number    0 , Number      1 ]
              , assertNot $ app Eq [Number    0 , Number      2 ]

              , assertNot $ app Eq [Number    1 , Number   ( -1)]
              , assertNot $ app Eq [Number    1 , Number      0 ]
              , assert    $ app Eq [Number    1 , Number      1 ]
              , assertNot $ app Eq [Number    1 , Number      2 ]
              , assertNot $ app Eq [Number    1 , Number      3 ]

              , assertNot $ app Eq [Number    2 , Number      0 ]
              , assertNot $ app Eq [Number    2 , Number      1 ]
              , assert    $ app Eq [Number    2 , Number      2 ]
              , assertNot $ app Eq [Number    2 , Number      3 ]
              , assertNot $ app Eq [Number    2 , Number      4 ]

              , assertNot $ app Eq [Number   19 , Number     20 ]
              , assert    $ app Eq [Number   20 , Number     20 ]
              , assertNot $ app Eq [Number   21 , Number     20 ]

              , assertNot $ app Eq [Number (-19), Number   (-20)]
              , assert    $ app Eq [Number (-20), Number   (-20)]
              , assertNot $ app Eq [Number (-21), Number   (-20)]
              ]

  runTests 11 tests


testDocs12 :: IO ()
testDocs12 = do
  let tests = [ assertNot $ app Lt [Number    0 , Number   ( -1)]
              , assertNot $ app Lt [Number    0 , Number      0 ]
              , assert    $ app Lt [Number    0 , Number      1 ]
              , assert    $ app Lt [Number    0 , Number      2 ]

              , assertNot $ app Lt [Number    1 , Number      0 ]
              , assertNot $ app Lt [Number    1 , Number      1 ]
              , assert    $ app Lt [Number    1 , Number      2 ]
              , assert    $ app Lt [Number    1 , Number      3 ]

              , assertNot $ app Lt [Number    2 , Number      1 ]
              , assertNot $ app Lt [Number    2 , Number      2 ]
              , assert    $ app Lt [Number    2 , Number      3 ]
              , assert    $ app Lt [Number    2 , Number      4 ]

              , assert    $ app Lt [Number   19 , Number     20 ]
              , assertNot $ app Lt [Number   20 , Number     20 ]
              , assertNot $ app Lt [Number   21 , Number     20 ]

              , assertNot $ app Lt [Number (-19), Number   (-20)]
              , assertNot $ app Lt [Number (-20), Number   (-20)]
              , assert    $ app Lt [Number (-21), Number   (-20)]
              ]

  runTests 12 tests


testDocs13 :: IO ()
testDocs13 = do
  let tests = [ testModulatedEq (Number     0 ) "010"
              , testModulatedEq (Number     1 ) "01100001"
              , testModulatedEq (Number  ( -1)) "10100001"
              , testModulatedEq (Number     2 ) "01100010"
              , testModulatedEq (Number  ( -2)) "10100010"
              , testModulatedEq (Number    16 ) "0111000010000"
              , testModulatedEq (Number  (-16)) "1011000010000"
              , testModulatedEq (Number   255 ) "0111011111111"
              , testModulatedEq (Number (-255)) "1011011111111"
              , testModulatedEq (Number   256 ) "011110000100000000"
              , testModulatedEq (Number (-256)) "101110000100000000"
              ]

  runTests 13 tests


testDocs16 :: IO ()
testDocs16 = do
  let tests = [ testEq (app Neg [Number    0 ]) (Number    0 )
              , testEq (app Neg [Number    1 ]) (Number  (-1))
              , testEq (app Neg [Number  (-1)]) (Number    1 )
              , testEq (app Neg [Number    2 ]) (Number  (-2))
              , testEq (app Neg [Number  (-2)]) (Number    2 )
              ]

  runTests 16 tests

testDocs17 :: IO ()
testDocs17 = do
  let tests = [ testEq (app Inc [app Inc [Number 0]])                     (Number 2)
              , testEq (app Inc [app Inc [app Inc [Number 0]]])           (Number 3)
              , testEq (app Add [app Add [Number 2, Number 3], Number 4]) (Number 9)
              , testEq (app Add [Number 2, app Add [Number 3, Number 4]]) (Number 9)
              , testEq (app Add [app Mul [Number 2, Number 3], Number 4]) (Number 10)
              , testEq (app Mul [Number 2, app Add [Number 3, Number 4]]) (Number 14)
              ]

  runTests 17 tests

testDocs18 :: IO ()
testDocs18 = do
  let tests = [ testEq (app S [Func Add, Func Inc          , Number 1]) (Number  3)
              , testEq (app S [Func Mul, app Add [Number 1], Number 6]) (Number 42)
              ]

  runTests 18 tests

testDocs19 :: IO ()
testDocs19 = do
  let tests = [ testEq (app C [Func Add, Number 1 , Number 2]) (Number  3)
              ]

  runTests 19 tests


testDocs21 :: IO ()
testDocs21 = do
  let tests = [ testEq (app T [Number 1 , Number 5]) (Number  1)
              , assert (app T [Func T   , Func I])
              , assert (app T [Func T, app Inc [Number 5]])
              , testEq (app T [app Inc [Number 5], Func T]) (Number  6)
              ]

  runTests 21 tests

testDocs23 :: IO ()
testDocs23 = do
  let tests = [ testEq (app Pwr2 [Number 0]) (app S [app C [app Eq [Number 0],Number 1],app B [app Mul [Number 2],app B [Func Pwr2,app Add [(Number (-1))]]],Number 0])
              , testEq (app Pwr2 [Number 0]) (app C [app Eq [Number 0],Number 1,Number 0,app B [app Mul [Number 2],app B [Func  Pwr2,app Add [Number (-1)]],Number 0]])
              , testEq (app Pwr2 [Number 0]) (app Eq [Number 0,Number 0,Number 1,app B [app Mul [Number 2],app B [Func  Pwr2,app Add [Number (-1)]],Number 0]])
              , testEq (app Pwr2 [Number 0]) (app T [Number 1,app B [app Mul [Number 2],app B [Func  Pwr2,app Add [Number (-1)]],Number 0]])
              , testEq (app Pwr2 [Number 0]) (Number 1)

              -- skipping some equations with unevaluated right side from here on
              , testEq (app Pwr2 [Number 1]) (app S [app C [app Eq [Number 0],Number 1],app B [app Mul [Number 2],app B [Func Pwr2,app Add [(Number (-1))]]],Number 1])
              , testEq (app Pwr2 [Number 1]) (Number 2)
              , testEq (app Pwr2 [Number 2]) (app S [app C [app Eq [Number 0],Number 1],app B [app Mul [Number 2],app B [Func Pwr2,app Add [(Number (-1))]]],Number 2])

              , testEq (app Pwr2 [Number 2]) (Number   4)
              , testEq (app Pwr2 [Number 3]) (Number   8)
              , testEq (app Pwr2 [Number 4]) (Number  16)
              , testEq (app Pwr2 [Number 5]) (Number  32)
              , testEq (app Pwr2 [Number 6]) (Number  64)
              , testEq (app Pwr2 [Number 7]) (Number 128)
              , testEq (app Pwr2 [Number 8]) (Number 256)
              ]

  runTests 23 tests


testDocs24 :: IO ()
testDocs24 = do
  let tests = [ testEq (app I [Number 1]) (Number 1)
              ]
  runTests 24 tests

testDocs28 :: IO ()
testDocs28 = do
  let tests = [ assert (app Nil [Ident $ FuncName 0])
              ]
  runTests 28 tests

testDocs29 :: IO ()
testDocs29 = do
  let tests = [ assert    (app IsNil [app Nil []])
              , assertNot (app IsNil [app Cons [Ident $ FuncName 0, Ident $ FuncName 1]])
              ]
  runTests 29 tests

testDocs34 :: IO ()
testDocs34 = do
  let tests = [assert (app IsNil [app MultipleDraw [Func Nil]])]
  runTests 34 tests

-- found error, Missing prefix 11 for Cons
testDocs35 :: IO ()
testDocs35 = do
  let tests = [ testModulatedEq (Func Nil)                                            "00"
              , testModulatedEq (app Cons [Func Nil, Func Nil])                       "110000"
              , testModulatedEq (app Cons [Number 0, Func Nil])                       "1101000"
              , testModulatedEq (app Cons [Number 1, Number 2])                       "110110000101100010"
              , testModulatedEq (app Cons [Number 1, app Cons [Number 2, Func Nil]])  "1101100001110110001000"
              -- skipping last two tests as they use list construction syntax
              ]
  runTests 35 tests

{-
testDocsXX :: IO ()
testDocsXX = do
  let tests = []
  runTests XX tests
-}


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- TODO move Test Utility functions to separate module

runTests :: Integer -> [MIB TestResult] -> IO ()
runTests docIndex tests = do
  let tests' = zip [(1::Integer)..] tests
  forM_ tests' $ \(index, test) -> do
      testResult <- runMIB test
      case testResult of
          Right (Right Nothing ) -> putStrLn $ "Test " <> show index <> " for Doc #"<> show docIndex <> " succeeded!"
          Right (Right (Just r)) -> putStrLn $ "Test " <> show index <> " for Doc #"<> show docIndex <> " failed, got unexpected:" <> show r
          Right (Left  e       ) -> putStrLn $ "Test " <> show index <> " for Doc #"<> show docIndex <> " had an internal error:\n" <> show e
          Left  e                -> putStrLn $ "Test " <> show index <> " for Doc #"<> show docIndex <> " errored:\n" <> show e

type TestResult = Either Data (Maybe Data)

assert    :: AlienExpr -> MIB (Either Data (Maybe Data))
assert expr = do
  result <- runExpr expr
  case result of
      Part T [] -> pure $ Right Nothing
      Part F [] -> pure $ Right $ Just $ Part F []
      other     -> pure $ Left other

assertNot :: AlienExpr -> MIB (Either Data (Maybe Data))
assertNot expr = do
  result <- runExpr expr
  case result of
      Part T [] -> pure $ Right $ Just $ Part T []
      Part F [] -> pure $ Right Nothing
      other     -> pure $ Left other

testModulatedEq :: AlienExpr -> String -> MIB (Either Data (Maybe Data))
testModulatedEq expr res = do
    modulated <- runExpr $ app Mod [expr]
    case modulated of
      Modulated modulated' ->
        if bitsToString modulated' == res then
            pure $ Right Nothing
        else
            pure $ Right $ Just modulated
      _ ->  pure $ Left modulated

testEq :: AlienExpr -> AlienExpr -> MIB (Either Data (Maybe Data))
testEq expr result = do
  boolExpr <- runExpr $ app Eq [expr,result]
  dataExpr <- runExpr $ expr
  case boolExpr of
    Part T [] -> pure $ Right Nothing
    Part F [] -> pure $ Right $ Just dataExpr
    other     -> pure $ Left  other