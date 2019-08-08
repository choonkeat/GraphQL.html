module Dev exposing (log, toString)


toString : a -> String
toString _ =
    ""


log : String -> a -> a
log string a =
    a
