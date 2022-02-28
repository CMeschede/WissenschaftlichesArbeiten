# Deskription und Visualisierung

#Daten einlesen
daten <- read.csv("data.csv")
daten$studienfach <- as.factor(daten$studienfach)
daten$mathe_LK <- as.factor(daten$mathe_LK)

# (a)
TeilA(daten)


#                   id     alter interesse_an_mathe interesse_an_prog
#Minimum        1.0000 19.000000           3.000000          1.000000
#25 % Quantil  25.7500 24.000000           3.000000          5.000000
#Median        50.5000 25.000000           4.000000          6.000000
#Mittelwert    50.5000 25.200000           4.530000          5.580000
#75 % Quantil  75.2500 27.000000           6.000000          7.000000
#Maximum      100.0000 29.000000           7.000000          7.000000
#Varianz      841.6667  3.878788           2.574848          2.286465

#Das Alter liegt zwischen 19 und 29, mit einem Mittelwert von 25.2, einem Median von 25 und einer Varianz von 3.88.
#Das Interesse an Mathematik liegt zwischen 3 und 7, mit einem Mittelwert von 4.53, einem Median von 4 und einer Varianz von 2.57.
#Das Interesse an Programmieren liegt zwischen 1 und 7, mit einem Mittelwert von 5.58, einem Median von 6 und einer Varianz von 2.28.

#  (b)
TeilB(daten)

#$studienfach
#Mathematik   Informatik Data Science    Statistik 
#12           26           31           31 
#Am meisten studieren Statistik oder Data Science, etwas weniger Informatik und sehr wenige Mathematik.
#$interesse_an_mathe
#7  3  4  6  5 
#21 44  9  8 18 
#Das Interesse an Mathematik liegt hauptsächlich bei 3, 5 und 7.

#$interesse_an_prog
#2  7  5  6  3  4  1 
#5 36 21 24  6  7  1 
#Das Interesse an Programmieren ist hauptsächlich in den höheren Bereichen (zwischen 5 und 7)

#$mathe_LK
#ja nein 
#71   29 

#  (c)
TeilC(daten$studienfach, daten$interesse_an_mathe)
#              3 4 5 6  7
#Data Science  8 6 9 4  4
#Informatik   26 0 0 0  0
#Mathematik    0 0 0 0 12
#Statistik    10 3 9 4  5
#Deutlich sichtbar: Alle Informatikstudenten haben ein Interesse von 3 an Mathematik
#Alle Mathematikstudenten haben ein Interesse von 7 an Mathematik.
#Bei den Statistik und Data Science Studenten ist es auf die Klassen 3 bis 7 verteilt.

TeilC(daten$studienfach, daten$interesse_an_prog)
#             1 2 3 4  5  6  7
#Data Science 0 0 3 4 11  8  5
#Informatik   0 0 0 0  0  0 26
#Mathematik   1 5 2 0  2  2  0
#Statistik    0 0 1 3  8 14  5
#Alle Informatikstudenten haben ein Interesse von 7 an Programmieren.
#Das Interesse an Programmieren ist von Statistik und Data Science Studenten höher als von Mathematikstudenten.

TeilC(daten$interesse_an_mathe, daten$interesse_an_prog)
#  1 2 3 4 5 6  7
#3 0 0 1 1 8 6 28
#4 0 0 1 0 3 4  1
#5 0 0 1 4 3 7  3
#6 0 0 1 1 2 1  3
#7 1 5 2 1 5 6  1
#Es fällt auf: Die Größte Gruppe sind diejenigen, die ein Interesse von 7 am Programmieren und ein Interesse von 3 an Mathematik haben.
#Weiter oben ließ sich bereits feststellen, dass 26 dieser 28 die Informatikstudenten sind. Es scheint, als ob ein hohes Interesse an
#Programmieren mit einem niedrigen Interesse an Mathematik zusammenhängt.


TeilC(daten$studienfach, daten$mathe_LK)
#             ja nein
#Data Science 20   11
#Informatik   19    7
#Mathematik    9    3
#Statistik    23    8

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
