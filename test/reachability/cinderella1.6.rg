vars: b1, b2, b3, b4, b5
init: b1 >= 0 && b2 >= 0 && b3 >= 0 && b4 >= 0 && b5 >= 0
      && b1 + b2 + b3 + b4 + b5 = 1
safe: b1 <= 16/10 && b2 <= 16/10 && b3 <= 16/10 && b4 <= 16/10 && b5 <= 16/10
      && ((b1' = 0 && b2' = 0 && b3' = b3 && b4' = b4 && b5' = b5)
          || (b1' = b1 && b2' = 0 && b3' = 0 && b4' = b4 && b5' = b5)
	  || (b1' = b1 && b2' = b2 && b3' = 0 && b4' = 0 && b5' = b5)
	  || (b1' = b1 && b2' = b2 && b3' = b3 && b4' = 0 && b5' = 0)
	  || (b1' = 0 && b2' = b2 && b3' = b3 && b4' = b4 && b5' = 0))
reach: ((b1' >= b1 && b2' >= b2 && b3' >= b3 && b4' >= b4 && b5' >= b5)
       && b1' + b2' + b3' + b4' + b5' = b1 + b2 + b3 + b4 + b5 + 1)
