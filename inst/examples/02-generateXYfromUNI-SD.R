## example for read and generate X, Y from DataBases UNIMANshort y San Diego
#### función moduleGenXY(input), genera X , Y de las bases de datos 
####UNIMANshort y SanDiego, para el caso de SanDiego permite cargar toda la base
####de datos o sólo los batchs que nos interesen.
####input :  es una lista que puede contener lo siguiente
####    * DataBase=   "UNIMANshort"
####                  "SanDiego"
####                  "other"  en este caso genera solo un mensaje
####    * batch= se debe definir en la entrada solo si DataBase="SanDiego"
####                  "to"  se usa para leer desde el batch 1 hasta el batch="fin"
####                  "this" se usa si se requiere leer desde una batch="ini"
####                  hasta un batch="fin". Esto de no leer por batch.
####    * ini= es un número entre 1 y 10, se requiere solo si el batch="this"
####    * fin= es un número entre 1 y 10, se requiere solo si el batch="this"
####output: retorna una lista que contiene input, X, Y y conc en caso de haber
####    seleccionado la base de datos de UNIMANshort
####    PARA LEER POR BATCH y hacer SPLIT en SanDiego, defina DataBases y trainIni
####    trainFin



rm(A)
a<-list(DataBase="SanDiego", batch="this", ini=1, fin=10)
A<-moduleGenXY(a)
summary(A)

### for UNIMANshort, enter name DataBase

rm(A)
a<-list(DataBase="UNIMANshort")
a
A<-moduleGenXY(a)
moduleSplit.split(list(DataBase="UNIMANshort", X=A$X, Y=A$Y, trainSize=150))



### for split en SanDiego  

rm(A)
a<-list(DataBase="SanDiego", trainIni = 1, trainFin = 5)
A<-moduleSplit(a)
summary(A)
dim(A$train$Xtrain)
dim(A$test$Xtest)

###3 probe test.Sandiego(input)
rm(A)
a<-list(DataBase="SanDiego", trainIni = 1, trainFin = 3)
A<-test.SanDiego(a)
Y<-(A$Ytest)

