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
      Print (Call sq a)  
    )
  ) #
  Fun2 (\x y -> (x `Mult` x) `Plus` (y `Mult` y)) (\sqLen ->
    Var (\x ->
    Var (\y ->
      x @= Int32 3 #
      y @= Int32 4 #
      Print (Call2 sqLen x y)
    )
    )
  )
  