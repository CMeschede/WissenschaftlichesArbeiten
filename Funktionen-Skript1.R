# Funktionen-Skript 1
# x = Datensatz, der analysiert werden soll


# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen 
# berechnet und ausgibt
TeilA <- function(x){
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
TeilB <- function(x){
  List <- list(0, 0, 0, 0, 0, 0)
  for(i in 1:ncol(x)){
    X <- numeric(length(unique(x[,i])))
    for(j in 1:length(unique(x[,i]))){
      X[j] <-  sum(Datensatz[,i] == unique(Datensatz[,i])[j])
      names(X) <- unique(x[,i])
      List[[i]] <- X
    }
  }
  names(List) <- names(x)
  List[[1]] <- NULL
  List[[1]] <- NULL
  return(List)
}

# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammenhang zwischen 
# zwei kategorialen Variablen berechnet ausgibt

# x und y kategoriale Variablen
TeilC <- function(x, y){
  X <- matrix(0, length(unique(x)), length(unique(y)))
  rownames(X) <- names(x)
  names(X) <- names(y)
  for(i in 1:length(x)){
    for(j in 1:length(unique(x))){
      for(k in 1:length(unique(y))){
        if(x[i] == sort(unique(x))[j] && y[i] == sort(unique(y))[k]){
          X[j, k] <- X[j, k] + 1
        }
      }
    }
  }
  X <- as.data.frame(X)
  names(X) <- sort(unique(y))
  rownames(X) <- sort(unique(x))
  return(X)
}


# (d) Eine Funktion, die geeignete deskriptive Statistiken für den Zusammenhang
# zwischen einer metrischen und einer dichotomen Variable berechnet und ausgibt

# x = metrische Variable; y = dichotome Variable
TeilD <- function(x,y){
  # Frage ab, ob die eingegebenen Variablen die erforderliche Form haben
  if(!is.numeric(x)){
    stop("Die Variable x ist nicht numerisch.")
  }
  if(!is.factor(y)){
    stop("Die Variable y ist kein Factor.")
  }
  if(length(levels(y)) != 2){
    stop("Die Variable y ist nicht dichotom.")
  }
  # Erstelle Teildatensaetze der Variable x getrennt nach den Levels von y
  A <- x[which(y == levels(y)[1])]
  B <- x[which(y == levels(y)[2])]
  # Gebe arithmetisches Mittel, Median, Varianz und Standardabweichung aus, um auf Lage
  # und Streuung zu vergleichen.
  cat("Für y =", levels(y)[1], ":", " arithmetisches Mittel:", mean(A), ", Median:", median(A),
      ",\n\t\tVarianz:", var(A), ", Standardabweichung:", sd(A),
      "\nFür y =", levels(y)[2], ":", " arithmetisches Mittel:", mean(B), ", Median:", median(B),
      ",\n\t\t Varianz:", var(B), ", Standardabweichung:", sd(B),
      "\nDifferenz der arithmetischen Mittel:", mean(A) - mean(B),
      "\nDifferenz der Standardabweichungen:", sd(A) - sd (B))
}
# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
# quantilbasiert kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)
TeilE <-function(x){
  dummy <- numeric(length(x))
  if(is.numeric(x)){
    X <- numeric(2)
    X[1] <- quantile(x, 0.33)
    X[2] <- quantile(x, 0.66)
  }else {stop("Die Eingabe ist nicht nummerisch.")}
  for(j in 1:length(x)){
    if(x[j] < X[1]){
      dummy[j] <- "niedrig"
    }
    else{
      if(x[j] > X[2]){
        dummy[j] <- "hoch"
      }
      else{
        dummy[j] <- "mittel"
      }
    }
  }
  return(table(dummy))
}

# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen
# Variablen erstellt

#Mosaikplot mit drei Variablen:
TeilF1 <- function(x, y, z, main = "Mosaikplot von drei Variablen",...){
  # mit ... koennen der Plot-Funktion zusaetzliche Parameter uebergeben werden
  mymosaic(x, y, z, main, ...)
}

#Barplots fuer mehrere Variablen:
TeilF2 <- function(x, main = names(x), ...){
  # mit ... koennen der Plot-Funktion zusaetzliche Parameter uebergeben werden
  p <- par(mfrow = c(1,length(x)))
  for(i in seq(1, length(x))){
    barplot(table(x[i]), main = main[i], ...)
  }
  par(p)
}

