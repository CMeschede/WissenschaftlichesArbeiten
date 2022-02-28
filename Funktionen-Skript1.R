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
Teilb <- function(x){
  #Verteilung der einzelnen Studiengaenge
  anzahl_ds <- length(which(x$studienfach=="Data Science"))
  anzahl_info <- length(which(x$studienfach=="Informatik"))
  anzahl_mathe <- length(which(x$studienfach=="Mathematik"))
  anzahl_stat <- length(which(x$studienfach=="Statistik"))
  
  #Verteilung Mathe_LK insgesamt
  mathelk_ins <- length(which(x$mathe_LK=="ja"))
  
  #Verteilung Mathe LK zum Studiengang Data Science
  lk_ds <- length(which(x$studienfach=="Data Science" & x$mathe_LK=="ja"))
  
  #Verteilung Mathe LK zum Studiengang Informatik
  lk_inf <- length(which(x$studienfach=="Informatik" & x$mathe_LK=="ja"))
  
  #Verteilung Mathe LK zum Studiengang Mathematik
  lk_math <- length(which(x$studienfach=="Mathematik" & x$mathe_LK=="ja"))
  
  #Verteilung Mathe LK zum Studiengang Statistik
  lk_stat <- length(which(x$studienfach=="Statistik" & x$mathe_LK=="ja"))
}


# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammenhang zwischen 
# zwei kategorialen Variablen berechnet ausgibt

apply(x, 2, table)


# (d) Eine Funktion, die geeignete deskriptive Statistiken für den Zusammenhang
# zwischen einer metrischen und einer dichotomen Variable berechnet und ausgibt

# x = metrische Variable; y = dichotome Variable
Teild <- function(x,y){
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

# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen
# Variablen erstellt

#Mosaikplot mit drei Variablen:
Teilf1 <- function(x, y, z, main = "Mosaikplot von drei Variablen",...){
  # mit ... koennen der Plot-Funktion zusaetzliche Parameter uebergeben werden
  mymosaic(x, y, z, main, ...)
}

#Barplots fuer mehrere Variablen:
Teilf2 <- function(x, main = names(x), ...){
  # mit ... koennen der Plot-Funktion zusaetzliche Parameter uebergeben werden
  p <- par(mfrow = c(1,length(x)))
  for(i in seq(1, length(x))){
    barplot(table(x[i]), main = main[i], ...)
  }
  par(p)
}

