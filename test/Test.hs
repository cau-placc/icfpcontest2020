import Parser

main :: IO ()
main = do
  galaxy <- readFile "galaxy.txt"
  let code = unlines $ lines galaxy
  print $ parseAlienProg code

