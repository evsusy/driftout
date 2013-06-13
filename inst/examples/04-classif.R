# A . USANDO UNIMANshort

# 1. genero X, Y
rm(list=ls())
a<-list(DataBase="UNIMANshort")
A<-moduleGenXY(a)
summary(A)
X<-A$X
Y<-A$Y
# 2. Split Data
B<-list(DataBase="UNIMANshort", X=X, Y=Y , trainInd=150)
b<-moduleSplit(B)
summary(b)


Xtrain<-as.matrix(b$Xtrain)
dim(Xtrain)
Xtest<-as.matrix(b$Xtest)
dim(Xtest)
Ytrain<-as.factor(b$Ytrain)
length(Ytrain)
Ytest<-as.factor(b$Ytest)
length(Ytest)
trainInd<-b$trainInd

# 3. aplico clasificador

A<-list(DataBase="UNIMANshort",Xtrain=Xtrain, Ytrain=Ytrain,
        Xtest=Xtest,Ytest=Ytest, methodClass="knn", trainInd=trainInd)
a<-moduleClass(A)
a
#  B. USANDO SAN DIEGO SIN BATCH

rm(list=ls())
# 1. genero X, Y
a<-list(DataBase="SanDiego", ini=1, fin=9)
A<-moduleGenXY(a)
summary(A)
X<-A$X
Y<-A$Y
# 2. Split Data
A<-list(DataBase="SanDiego", X=X, Y=Y , trainInd=9000)
a<-moduleSplit(A)
summary(a)

Xtrain<-as.matrix(a$Xtrain)
dim(Xtrain)
Xtest<-as.matrix(a$Xtest)
dim(Xtest)
Ytrain<-as.factor(a$Ytrain)
length(Ytrain)
Ytest<-as.factor(a$Ytest)
length(Ytest)
trainInd<-a$trainInd

# 3. aplico clasificador
A<-list(DataBase="SanDiego",Xtrain=Xtrain, Ytrain=Ytrain,
        Xtest=Xtest,Ytest=Ytest, methodClass="knn", trainInd=trainInd)
a<-moduleClass(A)
summary(a)
a$testAcc
a$trainAcc


#  C. USANDO SAN DIEGO -- Leyendo los datos en forma Incremental 
# trainInd="incremental"
# recorro la base de datos de Sandiego en 9 grupos para train y validación
# en 9 grupos desde el 10% hasta el 90%


rm(list=ls())
# 1. genero X, Y
a<-list(DataBase="SanDiego", ini=1, fin=9)
A<-moduleGenXY(a)
summary(A)
X<-A$X
Y<-A$Y


# 2. Split Data
A<-list(DataBase="SanDiego", X=X, Y=Y, trainInd="incremental")
a<-moduleSplit(A)
summary(a)


Xtrain<-(a$Xtrain)
dim(Xtrain)
Xtest<-(a$Xtest)
dim(Xtest)
Ytrain<-a$Ytrain
length(Ytrain)
Ytest<-a$Ytest
length(Ytest)
trainInd<-a$trainInd

# 3. aplico clasificador
A<-list(DataBase="SanDiegoBatch",Xtrain=Xtrain, Ytrain=Ytrain,
        Xtest=Xtest,Ytest=Ytest, methodClass="knn", trainInd=trainInd)
a<-moduleClass(A)
summary(a)
a$accu1
a$accu2

