module Main where

import Numeric.LinearAlgebra

data Model = Model { w::Matrix R, beta::Vector R} deriving Show

-- sigmoid
sigmoid :: R -> R
sigmoid x = 1 / (1 + exp (-x))

elm :: Matrix R -> Vector R -> Int -> IO Model
elm x y n = do _w <- randn n $ cols x
               let hidden = cmap sigmoid $ _w <> (tr x)
                   _beta = tr (pinv hidden) #> y
               return Model { w = _w, beta = _beta}

classify :: Model -> Matrix R -> Vector R
classify m x =
  let _w = w m
      _beta = beta m
  in cmap signum $ tr (cmap sigmoid $ _w <> (tr x)) #> _beta

main :: IO ()
main = do
  model <- elm x y n
  print "class label"
  print y
  print $ "result: hidden_neuron = " ++ show n
  print $ classify model x
    where x = matrix 3 [0, 0, 1,
                        0, 1, 1,
                        1, 0, 1,
                        1, 1, 1]
          y = vector [-1, 1, 1, -1]
          n = 10
