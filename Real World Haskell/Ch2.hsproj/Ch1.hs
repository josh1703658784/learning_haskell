
lastButOne :: [a] -> a
lastButOne = last . take 2 . reverse


