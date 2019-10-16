irisCopia <- iris
irisNorm <- iris

#normalizar
for(col in 1:4){
  for (lin in 1:150) {
    irisNorm[lin,col] <- (irisCopia[lin,col] - min(irisCopia[,col])) / (max(irisCopia[,col]) - min(irisCopia[,col]))
  }
}

