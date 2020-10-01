vars: x
init: x = 1
safe: !(x % 3 = 0)
      && (x' = x || x' = x+2)  

reach: (x' = x+1)
