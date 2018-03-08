#test if two regions are overlappng
is.overlap <- function(x,y) max(x) >= min(y) && min(x) <= max(y)
