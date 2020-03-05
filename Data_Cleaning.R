Data <- read.csv("C:/Users/mkg4541/Downloads/Sorted.csv", header = TRUE)
City <- Data$BH007
State <- Data$BH008
cities <- list()
cities
for (i in City) {
  if (length(cities)==0) {
    cities <- append(cities,i)
  }
  else if (!i %in% cities) {
    cities <- append(cities,i)
  }
}

cities <- head(cities,-(length(cities)-100))
index <- which(City %in% cities[100])[1]
s <- State[index]
for (i in which(City %in% cities[100])) {
  if(State[i] == s) {
    iny <- i
  }
  else{
    break
  }
}


new_Data <- Data[1:iny,]

Counted <- list()
sum = list()
AA <- list()
Murder <- list()
Robbery <- list()
sum[1] = 0
AA[1] <- 0
Murder[1] <- 0
Robbery[1] = 0
index <- 1
for (i in 1:iny) {
  if (!as.character(new_Data$BH007[i]) %in% Counted) {
    Counted[index] <- as.character(new_Data$BH007[i])
    sum[index] <- 1
    if (as.character(new_Data$V20061[i]) =="120") {
      Robbery[index] <- 1
    }
    else {
      Robbery[index] <- 0
    }
    if (as.character(new_Data$V20062[i]) =="120") {
      Robbery[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(Robbery[match(as.character(new_Data$BH007[i]),Counted)])
    }
    if (as.character(new_Data$V20063[i]) =="120") {
      Robbery[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(Robbery[match(as.character(new_Data$BH007[i]),Counted)])
    }
    if (as.character(new_Data$V20061[i]) =="91") {
      Murder[index] <- 1
    }
    else {
      Murder[index] <- 0
    }
    if (as.character(new_Data$V20062[i]) =="91") {
      Murder[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(Murder[match(as.character(new_Data$BH007[i]),Counted)])
    }
    if (as.character(new_Data$V20063[i]) =="91") {
      Murder[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(Murder[match(as.character(new_Data$BH007[i]),Counted)])
    }
    if (as.character(new_Data$V20061[i]) =="131") {
      AA[index] <- 1
    }
    
    else {
      AA[index] <- 0
    }
    if (as.character(new_Data$V20062[i]) =="131") {
      AA[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(AA[match(as.character(new_Data$BH007[i]),Counted)])
    }
    if (as.character(new_Data$V20063[i]) =="131") {
      AA[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(AA[match(as.character(new_Data$BH007[i]),Counted)])
    }
    index <- index + 1
    
  }
  else {
    sum[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(sum[match(as.character(new_Data$BH007[i]),Counted)])
    if (as.character(new_Data$V40071[i])=="120") {
      Robbery[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(Robbery[match(as.character(new_Data$BH007[i]),Counted)])
    }
    if (as.character(new_Data$V40071[i])=="91") {
      Murder[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(Robbery[match(as.character(new_Data$BH007[i]),Counted)])
    }
    if (as.character(new_Data$V40071[i])=="131") {
      AA[match(as.character(new_Data$BH007[i]),Counted)] <- 1 + as.numeric(Robbery[match(as.character(new_Data$BH007[i]),Counted)])
    }
  }
  
  
}
final_city <- Counted
final_state <- list()
final_population <- list()
j <-1
for (i in final_city) {
  final_state[j] <- as.character(new_Data$BH008[match(i,new_Data$BH007)])
  final_population[j] <- as.character(new_Data$BH019[match(i,new_Data$BH007)])
  j <- j + 1
}


library("xlsx")

tes <- data.frame("Total Population" = matrix(unlist(final_population)), "City" = matrix(unlist(final_city)), "State" = matrix(unlist(final_state)), "Total Incidents by city" = matrix(unlist(sum)), "Aggravated Assault Incidents" = matrix(unlist(AA)), "Murder/Nonnegligent Manslaughter Incidents" = matrix(unlist(Murder)), "Robbery Incidents" = matrix(unlist(Robbery)))
write.xlsx2(tes, file = "testing.xlsx", col.names = TRUE, row.names = TRUE)
