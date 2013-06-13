moduleClass<- function (input)
{
  stopifnot("DataBase" %in% names(input) )
  stopifnot(all(c("Xtrain", "Ytrain","Xtest", "Ytest", "methodClass", "trainInd") %in% names(input)))
  
  DataBase <- input$DataBase

  output <- switch(DataBase,
                   "UNIMANshort" = class.gen(input),
                   "SanDiego"= class.gen(input),
                   "other"=class.gen(input),
                   "SanDiegoBatch" = SanDiegoBatch(input),
                   stop("Error in switch"))
  output <- c(input, output) 
  return(output)
}
  

class.gen <- function(input)
{
  ### input
  ###Indice para el conj. de entrenamiento
  trainInd<-input$trainInd
  ###Conjunto de entrenamiento
  Xtrain <- as.matrix(input$Xtrain)
  Ytrain <- as.factor(input$Ytrain)
  
  ###Conunto de validación
  Xtest<-as.matrix(input$Xtest)
  Ytest <- as.factor(input$Ytest)
  
  ###método usado para el clasificador
  methodClass <- input$methodClass
  stopifnot(class(Xtrain) == "matrix")
  stopifnot(class(Ytrain) == "factor")
  stopifnot(class(Xtest) == "matrix")
  stopifnot(class(Ytest) == "factor")
  stopifnot(class(methodClass) == "character")

  Xtrain <- as.matrix(Xtrain) # requirement by caret
  Xtest <- as.matrix(Xtest) # requirement by caret
  
  stopifnot(nrow(Xtrain) == length(Ytrain))
  
  ##ajuste de parámetros
  trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  #tuneGrid <- data.frame(.k = c( 2, 3, 4, 5, 7, 9))
  
  ## knn Fit, hallar K
  knnFit <- train(Xtrain, Ytrain, 
                  preProcess = c("center", "scale"),
                  method = methodClass,
                  trControl = trControl)
  knnFit
  plot(knnFit, main="Selección del K")
  
  kfit<-knnFit$bestTune
  kfit
  

  ### clasificaciòn usando knn
  
  Yp <- knn(Xtrain, Xtest, Ytrain, k=kfit, prob=TRUE)
   
  ### error de entrenamiento
  Yp2 <-  knn(Xtrain, Xtrain, Ytrain, k=kfit, prob=TRUE)
  
  ### train
  #model <- train(Xtrain, Ytrain, method = methodClass)
  
  ### evaluate the model
  testTab <- table(Yp, Ytest) # confusion matrix 
  testAcc <- sum(diag(testTab)) / sum(testTab)

  ##error de entrenamiento
  trainTab <- table(Yp2, Ytrain) # confusion matrix 
  trainAcc <- sum(diag(trainTab)) / sum(trainTab)
  
  ### return output
  output <- list(model = methodClass, trainTab=trainTab,
        trainAcc=trainAcc, testTab = testTab, testAcc = testAcc, knnFit=knnFit )
  stopifnot(!any(names(input) %in% names(output)))
  
  return(output)
}


SanDiegoBatch <- function(input)
  
{
  Xtrain<-input$Xtrain
  Xtest<-input$Xtest
  Ytrain<-input$Ytrain
  Ytest<-input$Ytest
  methodClass <- input$methodClass
  
  ##stopifnot(class(Xtrain) == "list")
  #stopifnot(class(Ytrain) == "list")
  #stopifnot(class(Xtest) == "list")
  #stopifnot(class(Ytest) == "list")
  #stopifnot(class(methodClass) == "character")
  accu1<-matrix(nrow=0, ncol=1)
  accu2<-matrix(nrow=0, ncol=1)
  for(i in 1:9)
  {
    Xtrain1<-Xtrain[[i]]
    Xtest1<-Xtest[[i]]
    Ytrain1<-Ytrain[[i]]
    Ytest1<-Ytest[[i]]
    stopifnot(nrow(Xtrain1) == length(Ytrain1))
    
    
    trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
    tuneGrid <- data.frame(.k = c( 2, 3, 4, 5, 7, 9))
    
    ## knn Fit, hallar K
    knnFit <- train(Xtrain1, Ytrain1, 
                    preProcess = c("center", "scale"),
                    method = methodClass,
                    trControl = trControl, tuneGrid=tuneGrid)
    knnFit
    plot(knnFit, main="Selección del K")
    
    kfit<-knnFit$bestTune
    kfit
    
    
    ### clasificaciòn usando knn
    
    Yp <- knn(Xtrain1, Xtest1, Ytrain1, k=kfit, prob=TRUE)
    
    ### error de entrenamiento
    Yp2 <-  knn(Xtrain1, Xtrain1, Ytrain1, k=kfit, prob=TRUE)
    
    ### train
    #model <- train(Xtrain, Ytrain, method = methodClass)
    
    ### evaluate the model
    testTab <- table(Yp, Ytest1) # confusion matrix 
    testAcc <- sum(diag(testTab)) / sum(testTab)
    
    ##error de entrenamiento
    trainTab <- table(Yp2, Ytrain1) # confusion matrix 
    trainAcc <- sum(diag(trainTab)) / sum(trainTab)
    
    
    accu1<-rbind(accu1, trainAcc)
    accu2<-rbind(accu2, testAcc)
  }
  
  row.names(accu1)<-NULL
  row.names(accu2)<-NULL
  
  ### return output
  output <- list(methodClass = methodClass, accu1=accu1, accu2=accu2)
  
  return(output)
}
