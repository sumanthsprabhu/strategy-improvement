vars: x, y
init: y = 0
safe: y <= 1 && -1 <= y 
      && (y' = y + 1 || y' = y - 1 || y' = y)
      && (x' = x + 1 || x' = x - 1 || x' = x)

reach: (y' = y + 1 || y' = y - 1 || y' = y)
      && (x' = x + 1 || x' = x - 1 || x' = x)
