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

# Personen mit Studienfach Mathematik haben alle maximales Interesse an Mathe.
# Informatikstudenten haben alle ein Interesse des Wertes 3 an Mathematik.
# Obwohl alle Studiengaenge relativ mathematisch sind, ist der kleinste angenommene
# Wert fuer Interesse an Mathe der Groesste, allerdings gibt es keine Personen mit
# einem Wert fuer Interesse unter 3.

TeilF2(daten[2:6], main = c("Alter", "Studienfach", "Interesse an Mathe",
                            "Interesse an Programmieren", "Mathe LK"))

# Das Alter scheint etwa normalverteilt zu sein.
# Die Studienfaecher Data Science und Statistik sind am haeufigsten vertreten,
# das Fach Informatik etwas weniger und Mathe nur verhaeltnismaessig selten.
# Das Interesse an Mathe ist bei keinem niedriger als 3, allerdings ist 3 der am
# haefigsten auftretende Wert fuer Interesse an Mathe. Die ungeraden Zahlen treten
# haeufiger auf, als die geraden Werte.
# Das Interesse an Programmieren ist deutlich linksschief verteilt. Der maximale
# Wert 7 tritt am haefigsten auf.
# Ein deutlich groesserer Teil der Personen hat einen Mathe-LK belegt.


#Boxplots fuer Alter, Interesse an Mathe und Interesse an Programmieren
p <- par(mfrow = c(1, 2), las = 1)
boxplot(daten[2], main = "Alter", ylab = "Alter")
boxplot(daten[4:5] ,names = c("Interesse an Mathe", "Interesse an Programmieren"),
        main = "Interesse an Mathe und an Programmieren")
par(p)

# Im Boxplot fuer die Variable Alter liegt der Median etwas unter der Mitte der Box,
# die Whiskers sind etwa gleich lang, nach unten gibt es einen Ausreißer.
# Das Interesse an Mathe liegt immer ueber dem Wert 3, da die Box bei drei schon
# beginnt und der Median eher unter der Mitte der Box liegt, scheint das Interesse
# an Mathe innerhalb des Intervalls [3,7] eher niedrig zu sein.
# Die Box fuer das Interesse an Programmieren liegt ganz oben, der Median liegt mittig
# in der Box. Das Interesse an Programmieren ist also insgesamt recht hoch.
# Nach unten gibt es einen Ausreisser.



#Dichteschaetzung fuer die Variable Alter:
plot(density(daten$alter, bw = 0.7), main = "Dichteschätzung für das Alters",
     xlab = "Alter, N = 100, Bandweite = 0.7",ylab = "Dichte")
# Das Alter scheint etwa normalverteilt zu sein, mit einem Erwartungswert von etwa 26

#Dichteschaetzung fuer die Variable Interesse an Mathematik:
plot(density(daten$interesse_an_mathe, bw = 0.6),
     xlab = "Interesse an Mathe, N = 100, Bandweite = 0.6", ylab = "Dichte",
     main = "Dichteschätzung für das Interesse an Mathematik")
# Das Interesse an Mathematik weist ein deutliches Maximum bei 3 auf, bei ungeraden
# sind jeweils lokale Maximalstellen.

#Dichteschaetzung fuer die Variable Interesse an Programmieren:
plot(density(daten$interesse_an_prog, bw = 0.5),
     xlab = "Interesse an Programmieren, N = 100, Bandweite = 0.5", ylab = "Dichte",
     main = "Dichteschätzung für das Interesse an Programmieren")
# Die geschaetzte Dichte fuer das Interesse an Programmieren ist linksschief mit einem
# Maximum bei 7.