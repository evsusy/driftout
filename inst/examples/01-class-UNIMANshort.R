### par
sensors <- 1:2

### data
library(chemosensors)
data(UNIMANshort)

X <- UNIMANshort$dat[, sensors]
Y <- as.factor(apply(UNIMANshort$C, 1, function(x) LETTERS[which(x != 0)]))

out <- list(X = X, Y = Y, trainSize = 150)

### moduleSplit
out <- moduleSplit.split(out)

### moduleClass
out <- addList(out, list(method = "lda"))
out <- moduleClass.train(out)
