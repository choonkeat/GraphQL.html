module Human exposing (..)


string : String -> String
string s =
    if String.endsWith "_id" s then
        String.dropRight 3 (String.replace "_" " " s) ++ " ID"

    else
        String.replace "_" " " s
