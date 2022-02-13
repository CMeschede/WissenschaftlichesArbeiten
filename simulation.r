# Dieses Skript wurde von [x] und [y] geschrieben

#(1)
alter <- round(rnorm(n=100, mean=25, sd=2))
alter

#(2)
studienfach <- sample(c("Statistik", "Data Science", "Informatik", "Mathematik"), size = 100, replace = TRUE, prob = c(30, 30, 25, 15))
studienfach

#(3)
#Zusammenhaenge: 
# Interesse an Mathe: 7 = Mathestudent*in, 5 = Statistik- oder Data Sciencestudent*in und 3 = Informatikstudent*in
simulate_interest_math <- function( studienfach ){
  iam <- c()
  for (i in studienfach){
    if(i == "Mathematik"){
      iam <- append(iam, 7)
    }
    if(i == "Statistik" | i == "Data Science"){
      iam <- append(iam, 5)
    }
    if(i == "Informatik"){
      iam <- append(iam, 3)
    }
  }
  return(iam)
}
interesse_an_mathe <- simulate_interest_math(studienfach)
interesse_an_mathe