module Main where

import Foreign.RSDD

main :: IO ()
main = do
  mgr <- roBddBuilderDefaultOrder 0
  (la, a) <- newVar mgr True
  print a
  (lb, b) <- newVar mgr True
  print b
  ite mgr a b t >>= print
  ite mgr t t t >>= print
  w <- wmcParamsIO
  setWeight w la $ Weight { hi = 1/3, lo = 2/3 }
  setWeight w lb $ Weight { hi = 1/4, lo = 3/4 }
  print $ varWeight w la
  print $ varWeight w lb
  a_or_b <- bddOr mgr a b
  printBdd a_or_b >>= putStrLn
  let x = bddWmc a_or_b w
  print ("hello", x)
  print $ bddEq mgr a b
  pure ()
  where t = ptrTrue
