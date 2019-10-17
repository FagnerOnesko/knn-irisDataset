irisCopia <- iris
irisNorm <- iris

#normalizar
for(col in 1:4){
  for (lin in 1:150) {
    irisNorm[lin,col] <- (irisCopia[lin,col] - min(irisCopia[,col])) / (max(irisCopia[,col]) - min(irisCopia[,col]))
  }
}

treinamento <- irisNorm[c(1:40,51:90, 101:140),]
teste <-irisNorm[c(41:50,91:100, 141:150),]

