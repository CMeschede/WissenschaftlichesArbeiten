# Funktionen-Skript 1
# x = Datensatz, der analysiert werden soll


# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen 
# berechnet und ausgibt
Teila <- function(x){
  y <- 0
  X <- as.data.frame(matrix(0, 7, ncol(x)))
  names(X) <- names(x)
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
      X[1, i] <- min(x[,i])
      X[2, i] <- quantile(x[,i], .25)
      X[3, i] <- quantile(x[,i], .5)
      X[4, i] <- mean(x[,i])
      X[5, i] <- quantile(x[,i], .75)
      X[6, i] <- max(x[,i])
      X[7, i] <- var(x[,i])
    }
    if(sum(X[,i]) == 0){
      y <- c(y, i)
    }
  }
  X <- X[,-y]
  rownames(X) <- c("Minimum", "25 % Quantil", "Median", "Mittelwert", "75 % Quantil", "Maximum", "Varianz")
  return(X)
}

# (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für kategoriale Variablen 
# berechnet und ausgibt


# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammenhang zwischen 
# zwei kategorialen Variablen berechnet ausgibt

apply(x, 2, table)
