# Dieses Skript wurde von Arthur von der Heyden und Dagmar Lux geschrieben

#(1)
alter <- round(rnorm(n=100, mean=25, sd=2))
alter

#(2)
studienfach <- sample(c("Statistik", "Data Science", "Informatik", "Mathematik"), size = 100, replace = TRUE, prob = c(30, 30, 25, 15))
studienfach

#(3)
#Zusammenhaenge: 
# Interesse an Mathe: 7 = Mathestudent*in, 3-7 = Statistik- oder Data Sciencestudent*in und 3 = Informatikstudent*in
simulate_interest_math <- function( studienfach ){
  iam <- c()
  for (i in studienfach){
    if(i == "Mathematik"){
      iam <- append(iam, 7)
    }
    if(i == "Statistik" | i == "Data Science"){
      iam <- append(iam, sample(3:7, size = 1, replace = TRUE))
    }
    if(i == "Informatik"){
      iam <- append(iam, 3)
    }
  }
  return(iam)
}
interesse_an_mathe <- simulate_interest_math(studienfach)
interesse_an_mathe

# (4) Interesse an Programmieren:
# Interesse an Programmieren: Antworten 1-6 beliebig = Mathestudent*in, 
# Antworten 3-7 mit Wahscheinlichkeiten = Statistik- oder Data Sciencestudent*in 
# und 7 = Informatikstudent*in

simulate_interest_prog <- function( studienfach ){
  iap <- c()
  for (i in studienfach){
    if(i == "Mathematik"){
      iap <- append(iap, sample(1:6, size = 1, replace = TRUE))
    }
    if(i == "Statistik" | i == "Data Science"){
      iap <- append(iap, sample(3:7, size = 1, replace = TRUE, prob = c(0.1, 0.2, 0.25, 0.25, 0.2)))
    }
    if(i == "Informatik"){
      iap <- append(iap, 7)
    }
  }
  return(iap)
}
interesse_an_prog <- simulate_interest_prog(studienfach)
interesse_an_prog

# (5) 

mathe_LK <- sample(c("ja", "nein"), 100, replace=TRUE, prob = c(0.7, 0.3))
mathe_LK



# ID-Spalte
id <- c(1:100)



  
# Dataframe erstellen

data <- data.frame(id, alter, studienfach, interesse_an_mathe, interesse_an_prog, mathe_LK)
data

# Dataframe in CSV konvertieren und bereitstellen (Sie wird gesondert kopiert)
write.csv(data, "data.csv", row.names = FALSE)
