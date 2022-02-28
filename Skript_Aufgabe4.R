# Deskription und Visualisierung

#Daten einlesen
daten <- read.csv("data.csv")
daten$studienfach <- as.factor(daten$studienfach)
daten$mathe_LK <- as.factor(daten$mathe_LK)

# (a)

# (b)

# (c)

# (d)

# (e)

# (f)

TeilF1(daten$interesse_an_mathe,  daten$studienfach, daten$mathe_LK,
       main = "Mosaikplot: Interesse an Mathe, Studienfach, Mathe LK",
       xlab = "Interesse an Mathe", ylab = "Studienfach",
       col = c("steelblue1", "springgreen3"))

TeilF2(daten[2:6], main = c("Alter", "Studienfach", "Interesse an Mathe",
                            "Interesse an Programmieren", "Mathe LK"))

