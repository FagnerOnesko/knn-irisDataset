irisCopia <- iris
irisNorm <- iris

#normalizar
for(col in 1:4){
  for (lin in 1:150) {
    irisNorm[lin,col] <- (irisCopia[lin,col] - min(irisCopia[,col])) / (max(irisCopia[,col]) - min(irisCopia[,col]))
  }
}

#dividir dataset em treinamento e teste
treinamento <- irisNorm[c(1:40,51:90, 101:140),]
teste <-irisNorm[c(41:50,91:100, 141:150),]

#distancia euclidiana
matrizDist <- matrix(nrow=30,ncol=120);

for(linTeste in 1:30){
  for (linTreino in 1:120) {
    matrizDist[linTeste, linTreino] <- sqrt((teste[linTeste,1]-treinamento[linTreino,1])^2 + 
                                              (teste[linTeste,2]-treinamento[linTreino,2])^2 +
                                              (teste[linTeste,3]-treinamento[linTreino,3])^2 + 
                                              (teste[linTeste,4]-treinamento[linTreino,4])^2)
  }
}

#knn
k <- 5;
matrizKnn <- matrix(nrow=30,ncol=k)
matrizDistCopia <- matrizDist

for (lin in 1:30) {
  for (col in 1:k) {
    matrizKnn[lin,col] <- which.min(matrizDistCopia[lin,])
    matrizDistCopia[lin,matrizKnn[lin,col]] <- 2
  }
}

#classificar
matrizKnnClasses <- matrix(nrow=30,ncol=1)

for (lin in 1:30){
  setosa= versicolor= virginica = 0
  for (col in 1:k) {
    if(treinamento[matrizKnn[lin,col],5] == "setosa" ){
      setosa = setosa + 1
    }
    else if(treinamento[matrizKnn[lin,col],5] == "versicolor"){
      versicolor = versicolor + 1
    }
    else{
      virginica = virginica + 1
    }
  }
  if(setosa > versicolor && setosa > virginica){
    matrizKnnClasses[lin,1] <- "setosa"
  }
  
  else if(versicolor > setosa && versicolor > virginica){
    matrizKnnClasses[lin,1] <- "versicolor"
  }
  else{
    matrizKnnClasses[lin,1] <- "virginica"
  }
}


