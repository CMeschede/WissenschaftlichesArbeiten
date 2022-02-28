# Deskription und Visualisierung

#Daten einlesen
daten <- read.csv("data.csv")
daten$studienfach <- as.factor(daten$studienfach)
daten$mathe_LK <- as.factor(daten$mathe_LK)

# (a)

# (b)

# (c)

# (d)

TeilD(daten$alter, daten$mathe_LK)
# Für y = ja :  arithmetisches Mittel: 25.29577 , Median: 26 ,
#               Varianz: 3.725553 , Standardabweichung: 1.930169 
# Für y = nein :  arithmetisches Mittel: 24.96552 , Median: 25 ,
#                 Varianz: 4.320197 , Standardabweichung: 2.078508 
# Differenz der arithmetischen Mittel: 0.3302574 
# Differenz der Standardabweichungen: -0.1483391

# Personen mit Mathe-LK sind tendenziell etwas aelter als Personen ohne Mathe-LK.
# Das Alter von Personen ohne Mathe-LK streut etwas mehr als das Alter von Personen 
# mit Mathe-LK.

# (e)

TeilE(daten$alter)
# dummy
# hoch  mittel niedrig 
# 26      56      18 

# Da das Alter als ganze Zahl kodiert ist und einige Werte doppelt vorkommen und die
# Quartilwerte selbst in die Kategorie "mittel" eingeordnet werden, befinden sich
# etwas ueber die Haelfte der Werte in der Katgorie "mittel". In der Kategorie "hoch"
# sind etwas mehr Werte enthalten als in der Kategorie "niedrig".

TeilE(daten$interesse_an_mathe)
# dummy
# hoch mittel 
# 29     71 

# Aus den selben Gruenden wie beim Alter finden sich ein Grossteil der Werte in der
# Kategorie "mittel". Es werden keine Werte in die Kategorie "niedrig" eingeordnet, da
# zu viele Werte bei 3 liegen, als dass der Wert unter dem Quantil von 0.33 liegen
# wuerde.

TeilE(daten$interesse_an_prog)
# dummy
# mittel niedrig 
# 81      19
# Auch hier befinden sich aus den gleichen Gruenden der Grossteil der Werte in der
# Kategorie "mittel". Im Gegensatz zum Interesse an Mathe liegen hier keine Werte
# in der Kategorie "hoch", aber welche in der Kategorie "niedrig". Der Grund ist
# allerdings identisch wie zuvor, nur in die andere "Richtung".

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
