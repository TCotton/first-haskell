
check :: String -> String -> Char -> (Bool, String)
check word display c =
  ( c `elem` word,
    [ if x == c
        then c
        else y
      | (x, y) <- zip word display
    ]
  )

