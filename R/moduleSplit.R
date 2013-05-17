
moduleSplit <- function(input)

  {
  
  DataBase <- input$DataBase
  
  output <- switch(DataBase,
                   "UNIMANshort" = split.uniS(input),
                   "SanDiego" = split.sanD(input),
                   "SanDiego-sinBatch" =split.uniS(input),
                   "other"=split.uniS(input),
                   stop("Error in switch"))
  
  output <- c(input, output) 
  return(output)
}
 
### split by UnimanShort - otra base de datos

split.uniS<-function(input)
{
  X<-input$X
  Y<-input$Y

  stopifnot(all(c("X", "Y", "trainSize") %in% names(input)))
  
  ### do splitting
  trainInd <- 1:input$trainSize
  testInd <- seq(input$trainSize + 1, nrow(input$X))
  
  ### return output
  output <- list(trainInd = trainInd, testInd = testInd)
  stopifnot(!any(names(input) %in% names(output)))
    
  return(output)
}


##################### split para San Diego

split.sanD<-function(input)
  
{
  train<-train.SanDiego(input)
  test<-test.SanDiego(input)
  output<-list(train=train, test=test)
  return(output)
} 

train.SanDiego<- function(input)

  {
  
  ### dataBase
  fichero<-"/home/susana/Documents/projetcs/driftout/data/data bases UCI"
  files=file.path(fichero, list.files(fichero))
  
  ### input
  stopifnot("trainIni" %in% names(input) )
  stopifnot("trainFin" %in% names(input) )
  
  
  ini<-input$trainIni
  fin<-input$trainFin
    
  ## matriz X, Y vacía
  Xtrain<- matrix(nrow=0, ncol=128)
  Ytrain<- matrix(nrow=0, ncol=1)
  
  ### leer los datos
  
  
  for(i in ini:fin)
  {
    data <- read.matrix.csr(files[i])
    X0<-(as.matrix(data$x))
    Xtrain<-rbind(Xtrain,X0)
    
    Y0<-as.matrix(data$y)
    Ytrain<-rbind(Ytrain,Y0)
  }
    
  Ytrain<-as.factor(Ytrain)
  Ytrain<-(LETTERS[Ytrain])
 
  
  output<-list(Xtrain=Xtrain, Ytrain=as.factor(Ytrain))
 
  
  return(output)
  
}


test.SanDiego<- function(input)
  
{
  
  ### dataBase
  fichero<-"/home/susana/Documents/projetcs/driftout/data/data bases UCI"
  files=file.path(fichero, list.files(fichero))
  
  ### input
  stopifnot("trainIni" %in% names(input) )
  stopifnot("trainFin" %in% names(input) )
    
  ini<-input$trainFin + 1
   
  
  ### leer los datos
  
    data <- read.matrix.csr(files[ini])
    Xtest<-(as.matrix(data$x))
    Ytest<-as.matrix(data$y)
      
  Ytest<-as.factor(Ytest)
  Ytest<-(LETTERS[Ytest])
  
  output<-list(Xtest=Xtest, Ytest=as.factor(Ytest))
  
  
  return(output)
  
}
