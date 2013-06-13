
moduleSplit <- function(input)

  {
  stopifnot(all(c("X", "Y", "trainInd","DataBase")%in% names(input)))
  X<-input$X
  Y<-input$Y              
  DataBase <- input$DataBase
  
  output <- switch(DataBase,
                   "UNIMANshort" = split.gen(input),
                   "SanDiegoBatch" = split.sanD(input),
                   "SanDiego" =split.gen(input),
                   "other"=split.gen(input),
                   stop("Error in switch"))
  
  output <- c(input, output) 
  return(output)
  
  }
 

### split by UnimanShort - otra base de datos -- SanDiego sin analizar por Batch

split.gen<-function(input)
  
  {
     
  trainInd<-input$trainInd
  
  if (class(trainInd)=="numeric")
      {output<-fixed.trainInd(input)}
  else{  
  output <- switch(trainInd,
                   "incremental" = incremental(input),
                   "slide" = slide.window(input),
                   stop("Error in switch"))
      }
  return(output)
  }

fixed.trainInd<-function(input)
  
  {
  stopifnot(all(c("X", "Y") %in% names(input)))
    
  ### do splitting
  trainInd <- input$trainInd
  X<-input$X
  Y<-input$Y   
  
  Xtrain<-X[1:trainInd,]
  Ytrain<-Y[1:trainInd]
  
  Xtest<-X[(trainInd+1):nrow(X),]
  Ytest<-Y[(trainInd+1):length(Y)]
  
  ### return output
  output <- list(Xtrain=Xtrain, Ytrain=Ytrain, Xtest=Xtest, Ytest=Ytest)
      
  return(output)
  }

 incremental<-function(input)
  {
  X<-input$X
  Y<-input$Y     
  n<-nrow(input$X)
  trainInd<-matrix(nrow=0, ncol=1)
  
  ###  Almaceno en un vector columna (trainInd) los trainInd desde el
  ### 10% hasta el 90% de los datos
  i=10
  while (i<=90)  
    {
    train<-(n*i)/100
    trainInd<-rbind(trainInd,train)
    i=i+10
    }
  
   #Listas vacías
  Xtrain<-list()
  Ytrain<-list()
  Xtest<-list()
  Ytest<-list()
  
  for(i in 1:nrow(trainInd))
    {
    Xtrain[[i]]<-X[1:trainInd[i],]
    Ytrain[[i]]<-Y[1:trainInd[i]]
    
    Xtest[[i]]<-X[(trainInd[i]+1):nrow(X),]
    Ytest[[i]]<-Y[(trainInd[i]+1):length(Y)]
    
    } 
  
  output<-list(Xtrain=Xtrain, Ytrain=Ytrain, Xtest=Xtest,
               Ytest=Ytest, trainInd=trainInd)
  
  return(output)
  
  }
  
  slide.window<-function(input)
  {
    trainInd <- input$trainInd
    X<-input$X
    Y<-input$Y   
    n<-nrow(X)
    part<-n*0.1
        
    ini=1
    fin=part
    Indini<-list()
    Indfin<-list()
    
    for(i in 1:10) 
    {
      Indini[[i]]<-ini
      Indfin[[i]]<-fin
      ini<-fin+1
      fin<-fin+part
    }
    
    #Listas vacías
    Xtrain<-list()
    Ytrain<-list()
    Xtest<-list()
    Ytest<-list()
    
    for(i in 1:length(Indfin))
    {
      Xtrain[[i]]<-X[(Indini[[i]]:Indfin[[i]]),]
      Ytrain[[i]]<-Y[(Indini[[i]]:Indfin[[i]])]
      
      Xtest[[i]]<-X[(Indini[[i]]:Indfin[[i]]),]
      Ytest[[i]]<-Y[(Indini[[i]]:Indfin[[i]])]
    } 
    
    output<-list(Xtrain=Xtrain, Ytrain=Ytrain, Xtest=Xtest,
                 Ytest=Ytest, Indini=Indini, Indfin=Indfin)
    return(output) 
    
  }
  
  
  
  
##################### split para San Diego

split.sanD<-function(input)
  
{
  
  ### input review
  stopifnot("trainIni" %in% names(input) )
  stopifnot("trainFin" %in% names(input) )
  stopifnot("DataBase" %in% names(input) )
  
  ## Utiliza una función para extraer el conjunto train y otra para test
 
  train<-train.SanDiego(input)
  test<-test.SanDiego(input)
  output<-list(train=train, test=test)
  return(output)
} 

###Función para obtener el conjunto train
###Devuelve una lista de dos elementos llamada train con Xtrain y Ytrain.

train.SanDiego<- function(input)

{
  fichero<-"C:/Users/SUSY/Documents/maestria/driftout/data/data bases UCI"
  files=file.path(fichero, list.files(fichero))
  
  ini<-input$trainIni
  fin<-input$trainFin
    
  ## matriz X, Y vacía
  Xtrain<- matrix(nrow=0, ncol=16)
  Ytrain<- matrix(nrow=0, ncol=1)
  
  ### leer los datos
  
  
  for(i in ini:fin)
  {
    data <- read.matrix.csr(files[i])
    
    X0<-(as.matrix(data$x))
    
    X1<-readate(X0)
        
    Xtrain<-rbind(Xtrain,X1)
    Y0<-as.matrix(data$y)
    Ytrain<-rbind(Ytrain,Y0)
  }
    
  Ytrain<-as.factor(Ytrain)
  Ytrain<-(LETTERS[Ytrain])
  row.names(Xtrain)<-NULL
  
  
  output<-list(Xtrain=Xtrain, Ytrain=as.factor(Ytrain))
 
  
  return(output)
  
}

###Función para obtener el conjunto test
###Devuelve una lista de dos elementos llamada test con Xtest[[i]] y Ytest[[i]]

test.SanDiego<- function(input)
  
{
  fichero<-"C:/Users/SUSY/Documents/maestria/driftout/data/data bases UCI"
  files=file.path(fichero, list.files(fichero))
  
  ini<-input$trainFin
  ini<-ini+1
  
  
  ## Listas vacías para Xtest y Ytest
  
  Xtest<-list( "Xtest1","Xtest2", "Xtest3", "Xtest4","Xtest5","Xtest6", "Xtest7", "Xtest8", "Xtest9", "Xtest10")
  Ytest<-list("Ytest1", "Ytest2", "Ytest3", "Ytest4","Ytest5", "Ytest6", "Ytest7", "Ytest8", "Ytest9", "Ytest10")
  
  
  ### leer los batch
  
  for(i in ini:10)
  {
    
    data <- read.matrix.csr(files[i])

    X0<-(as.matrix(data$x))
    X1<-readate(X0)
    ## guardo la matriz Xtest$i en la lista Xtest
    Xtest[[i]]<-X1
    
    Ytest[[i]]<-as.factor(data$y)
    Ytest[[i]]<-LETTERS[Ytest[[i]]]
    
  }
  
    row.names(Xtest)<-NULL
    output<-list(Xtest=Xtest, Ytest=Ytest)
  
  
  return(output)
  
}

readate<-function(X0)
  
{
  
  n<-as.numeric(nrow(X0))
  X1<-matrix(,nrow=n, ncol=16,
  dimnames =list(c(1:n),c(paste("S", 1:16, sep=""))))
  j=1

  for(i in 1:16)
    {           
      X1[,i]<-X0[,j] 
      j=j+8
    }

  return(X1)
}
