module HSexamples where

import Task3

gcd :: Expression ()
gcd =
  Var (\a ->
    Var (\b ->
      a @= Int32 372 #
      b @= Int32 108 #

      While ((a `Gt` Int32 0) `And` (b `Gt` Int32 0)) (
        (If   $ a `Gt` b)
        (Then $ a @= a `Mod` b)
        (Else $ b @= b `Mod` a)
      ) #

      Print (a `Plus` b)
    )
  )

example :: Expression ()
example =
  Var (\res ->
    Var (\a ->
      Var (\p ->
        res @= Str "" #
        a @= Str "HEL" #
        p @= Str "P" #

        Var (\i ->
          i @= Int32 0 #
          While (Int32 10 `Gt` i) (
            res @= res `Conc` a `Conc` p `Conc` Str " ME, DUDE! " #
            i @= i `Plus` Int32 1
          )
        )
      )
    ) #
    Print res
  )

funcFunc :: Expression ()
funcFunc =
  Fun (\x -> x `Mult` x) (\sq ->
    Var (\a ->
      a @= Int32 16 #
      Print (sq a)   
    )
  ) #
  Fun2 (\x y -> (x `Mult` x) `Plus` (y `Mult` y)) (\sqLen ->
    Var (\x ->
    Var (\y ->
      x @= Int32 3 #
      y @= Int32 4 #
      Print (sqLen x y)
    )
    )
  )

-- TODO: now we cant use `div` for Double
--sqrtMe :: Expression ()
--sqrtMe =
--  Var (\x ->
--  Var (\i ->
--  Var (\l ->
--  Var (\m ->
--  Var (\r ->
--    x @= Dbl 3025 #
--    i @= Int32 100 #
--    l @= Dbl (10.3 / 3.9) #
--    m @= Dbl 0 #
--    r @= x #
--
--    While (Int32 100 `Gt` i) (
--      m @= (l `Plus` r) `Div` Dbl 2 #
--      (If $ (m `Mult` m) `Gt` x)
--      (Then $ r @= m)
--      (Else $ l @= m) #
--      
--      i @= i `Plus` Int32 1
--    ) #
--    
--    Print r
--  )
--  )
--  )
--  )
--  )
