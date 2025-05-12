{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Python.Inline
import Python.Inline.QQ

main :: IO ()
main = withPython $ do
  result <- [pye| 
    from sympy import integrate, symbols
    x = symbols('x')
    str(integrate(x**2, x))  # Compute the integral of x^2
  |]
  putStrLn $ "Integral: " ++ result