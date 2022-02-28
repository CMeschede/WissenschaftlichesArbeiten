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

#Boxplots fuer Alter, Interesse an Mathe und Interesse an Programmieren
p <- par(mfrow = c(1, 2), las = 1)
boxplot(daten[2], main = "Boxplot des Alters", ylab = "Alter")
boxplot(daten[4:5],names = c("Interesse an Mathe", "Interesse an Programmieren"),
        main = "Boxplots von Interesse an Mathe und an Programmieren")
par(p)
