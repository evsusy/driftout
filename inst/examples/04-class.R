X<-as.matrix(X)
UNIMANshort<-load("C:/Users/SUSY/Documents/maestria/driftout/data/UNIMANshort.XY.RData")
input<-list(DataBase= "UNIMANshort", X=X, Y=Y , trainInd=40, methodClass="knn")

a<-moduleClass(input)
a



slide<-function(input)
{
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
  
  

accu<-matrix(nrow=0, ncol=1)

for(i in 1:nrow(trainInd))
    {
    Ind<-trainInd[i,1]
    input<-list(DataBase= "UNIMANshort", X=X, Y=Y , trainInd=Ind, methodClass="knn")
    a<-moduleClass(input)
    accu<-rbind(accu,a$trainAcc)
    
    }