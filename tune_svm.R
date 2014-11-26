############## BUILD DATA SETS ##############################
start = proc.time()

library(e1071)



word.path = "Dropbox/STAT154_project/frequency.csv"
power.path = NULL

new.word.path = NULL
new.power.path = NULL


# Returns a DataFrame containing the word features of DF which appear in at least LOW and no more than HIGH different texts, as well as the class label
filterByCount = function(df, freq, low, high)
{
  return(df[, c(freq >= low & freq <= high, TRUE)])
}

# Returns a random sample of N words appearing in exactly FREQ texts size N 
words.by.freq = function(mtx, freq, n = 20) {
  words = names(mtx)[textsPerWord == freq]
  print(sample(words, size = n))
}

# Trains a linear SVM with COST using only TRAININD of DAT and returns predicted labels of test observations
assess.SVM = function(dat, cost, trainind) {
  # Used to filter out zero columns to allow scaling
  isNonconstant = apply(dat[trainind, 1:(length(dat) - 1)], 2, function(x) sum(x) > 0)
  mod = svm(classification ~ ., data = dat[isNonconstant], cost = cost, subset = trainind, kernel = "linear")
  predy = predict(mod, dat[-trainind,], type = "Classification")
  return(predy)
}

# Returns a vector with a random fold label (in [1, k]) for each row of mtx 
get.fold.vect = function(k, mtx) {
  fold = rep(1:k, ceiling(nrow(mtx)/k))
  fold = sample(fold, size = nrow(mtx), replace = FALSE)
  return(fold)
}

# Performs K-fold cross-validation on linear SVM with COST on FEATURE.MTX, returns a confusion matrix.
cross.val.confusion.mtx = function(feature.mtx, cost, k = 5) {
  # This partition keeps proportions of ham and spam in each fold similar
  fold = rep(0, nrow(feature.mtx))
  fold[feature.mtx$classification == "ham"] = get.fold.vect(k, feature.mtx[feature.mtx$classification == "ham", ])
  fold[feature.mtx$classification == "spam"] = get.fold.vect(k, feature.mtx[feature.mtx$classification == "spam",])
  
  thismodtime = proc.time()
  print(paste("Training", k, "SVM models with cost", cost,"...", sep = " "))
  predy = as.factor(unlist(sapply(1:k, function(x) assess.SVM(feature.mtx, cost, which(fold != x)))))
  y = as.factor(unlist(sapply(1:k, function(x) feature.mtx$classification[fold == x])))
  tab = table(predy, y)
  print("Finished.")
  print(proc.time() - thismodtime)
  
  print(tab)
  print(paste("Accuracy:", acc(tab), sep = " "))
  print(paste("ppv:", ppv(tab), sep = " "))
  print(paste("npv:", npv(tab), sep = " "))
  return(table(predy, y))
}

# Returns a list of confusion matrices for a range of cost values
tune.confusion.mtx = function(feature.mtx, costrange, k = 5) {
  return(lapply(costrange, function(cost) cross.val.confusion.mtx(filt.word.mtx, cost, k)))
}

# Calculates overall accuracy from confusion matrix TAB.
acc = function(tab) {
  return((tab["spam", "spam"] + tab["ham", "ham"])/sum(tab))
}

# Calculates Positive Predictive Value from confusion matrix TAB.
ppv = function(tab) {
  return(tab["spam", "spam"]/sum(tab["spam",]))
}

# Calculates Negative Predictive Value from confusion matrix TAB.
npv = function(tab) {
  return(tab["ham", "ham"]/sum(tab["ham",]))
}




### Read and format training for:
### WORD MATRIX
word.mtx = read.csv(word.path)
names(word.mtx)[length(word.mtx)] = "classification"

### POWER MATRIX
#power.mtx = read.csv(power.path)


############# UNSUPERVISED LEARNING ########################

### Calculate number of texts in which each word appears 
textsPerWord = apply(word.mtx[1:(length(word.mtx) - 1)], 2, function(x) sum(x > 0))

### Decide upper and lower bounds for number of texts
# number of total features as a function of min cutoff
mincut = 1:10
plot(mincut, sapply(mincut, function(x){return(sum(textsPerWord >= x))}))

# print random words that appear in exactly 5 texts 
words.by.freq(word.mtx, 5)

# print most common words
freq.order = order(textsPerWord)
tail(cbind(names(word.mtx)[freq.order], textsPerWord[freq.order]), 10)


### Filter columns with rare and too common words
filt.word.mtx = filterByCount(word.mtx, textsPerWord, 5, 500)

### Merge filtered WORD MATRIX and POWER MATRIX with inner join

############ TUNE MODELs ###################################
### tune cost for linear svm with cross-validation


confusions = tune.confusion.mtx(filt.word.mtx, 10 ^ (-4:4))
proc.time() -  start
par(mfrow = c(3, 1))
plot(-4:4, sapply(confusions, acc), main = "Overall Accuracy", xlab = "log cost")
plot(-4:4, sapply(confusions, ppv), main = "Positive Predictive Value", xlab = "log cost")
plot(-4:4, sapply(confusions, npv), main = "Negative Predictive Value", xlab = "log cost")
par(mfrow = c(1, 1))

### Fit best final model on whole data set
final.model = NULL

########### CLASSIFY NEW DATA #############################
### Read and format new data
### new WORD MATRIX


### new POWER MATRIX

### Use old filters

### Combine filtered WORD MATRIX and POWER MATRIX

### Classify new data with FINAL.MODEL
#yhat = predict(final.model, newdata = merged.new, type = "Classification")