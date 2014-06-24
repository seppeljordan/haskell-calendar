module Colors (green, red, normal)
where

esc = "\x1b"
normal = esc ++ "[0m"
green =  esc ++ "[32m"
red =    esc ++ "[31m"
