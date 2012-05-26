myLast (l:[]) = l
myLast (h:t)  = myLast t

lastButOne (x:y:[]) = x
lastButOne (h:t) = lastButOne t
