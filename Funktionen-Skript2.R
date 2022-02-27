# Funktionen Skript 2

mymosaic <- function(x, y, z, main, ...){
  mosaicplot(~ x + y + z, main = main, off = 4, ... )
}