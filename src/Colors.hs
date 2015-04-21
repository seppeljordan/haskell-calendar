module Colors (green, red, normal)
where

esc :: String
esc = "\x1b"

normal :: String
normal = esc ++ "[0m"

green :: String
green =  esc ++ "[32m"

red :: String
red =    esc ++ "[31m"
