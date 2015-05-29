#NOTE TO INSTRUCTORS: 
#This script includes an example of our cross validation code. If you run the
# whole script, it will take upwards of an hour due to the cross validation. However, you can
# skip the cross validation and go straight to Step 10, Validation Set, by 
# commenting out lines 247 - 258.

library(e1071)
library(ROCR)

word.matrix.train.path = "./group5_wordMatrix.csv" #unfiltered word matrix
word.matrix.test.path = "./test_msgs_unlb.csv" #unfiltered word matrix
power.matrix.train.path = "./power_features_train.csv"
power.matrix.test.path = "./power_features_test_labeled.csv"

###################### Define Functions ##############################

# Returns a DataFrame containing the word features of DF which appear in
# at least LOW and no more than HIGH different texts, as well as the class label
filterByCount = function(df, freq, low, high)
{
  return(df[, c(freq >= low & freq <= high, TRUE)])
}

# Returns a random sample of N words appearing in exactly FREQ texts size N 
words.by.freq = function(mtx, freq, n = 20) {
  words = names(mtx)[textsPerWord == freq]
  print(sample(words, size = n))
}

# Trains a linear SVM with COST using only TRAININD of DAT and returns predicted labels
# or decision values of test observations
assess.SVM = function(dat, cost, trainind, decision.values = FALSE) {
  # Used to filter out zero columns to allow scaling
  isNonconstant = c(apply(dat[trainind, 1:(length(dat) - 1)], 2, function(x) sum(x) > 0), TRUE)
  mod = svm(classification ~ ., data = dat[isNonconstant], cost = cost, subset = trainind, kernel = "linear")
  if (!decision.values) {
    return(predict(mod, dat[-trainind,], type = "Classification"))
  }
  return (attributes(predict(mod, dat[-trainind,],decision.values = TRUE))$decision.values)
}

# Returns a vector with a random fold label (in [1, k]) for each row of mtx 
get.fold.vect = function(k, mtx) {
  fold = rep(1:k, ceiling(nrow(mtx)/k))
  fold = sample(fold, size = nrow(mtx), replace = FALSE)
  return(fold)
}

# Performs K-fold cross-validation on linear SVM with COST on FEATURE.MTX, returns a confusion matrix.
# if decision.values == TRUE and decision.boundary not null, uses decision.boundary instead of 0.
# if decision.values == TRUE and decision.boundary not null, returns a ROCR plottable Performance object.
cross.val.confusion.mtx = function(feature.mtx, cost, k = 5, decision.values = FALSE, decision.boundary = NULL) {
  # This partition keeps proportions of ham and spam in each fold similar
  fold = rep(0, nrow(feature.mtx))
  fold[feature.mtx$classification == "ham"] = get.fold.vect(k, feature.mtx[feature.mtx$classification == "ham", ])
  fold[feature.mtx$classification == "spam"] = get.fold.vect(k, feature.mtx[feature.mtx$classification == "spam",])
  thismodtime = proc.time()
  print(paste("Training", k, "SVM models with cost", cost,"...", sep = " "))
  
  y = as.factor(unlist(sapply(1:k, function(x) feature.mtx$classification[fold == x])))
  
  if (!decision.values) {
    predy = as.factor(unlist(sapply(1:k, function(x) assess.SVM(feature.mtx, cost, which(fold != x)))))
    print(proc.time() - thismodtime)
    
    tab = table(predy, y)
    print(tab)
    print(paste("Accuracy:", acc(tab), sep = " "))
    print(paste("ppv:", ppv(tab), sep = " "))
    print(paste("npv:", npv(tab), sep = " "))
    return(tab)
  }
  ## if decision.values = TRUE, return PERFORMANCE OBJECT, which can be plotted by ROCR
  predy = unlist(sapply(1:k, function(x) assess.SVM(feature.mtx, cost, which(fold != x), decision.values = TRUE)))
  if (!is.null(decision.boundary)) {
    greater = predy > decision.boundary
    predy[greater] = "ham"
    predy[!greater] = "spam"
    tab = table(as.factor(predy), y)
    return(tab)
  }
  predictionObj = prediction(predy, y, label.ordering = c("spam", "ham"))
  performanceObj = performance(predictionObj, "tpr", "fpr")
  return(performanceObj)
  
}

# Returns a list of confusion matrices for a range of cost values
tune.confusion.mtx = function(feature.mtx, costrange, k = 5, decision.values = FALSE) {
  return(lapply(costrange, function(cost) cross.val.confusion.mtx(feature.mtx, cost, k, decision.values)))
}

# Calculates overall accuracy from confusion matrix TAB.
acc = function(tab) {
  if (length(tab) < 4) {
    return(tab["ham", "ham"]/sum(tab))
  }
  return((tab["spam", "spam"] + tab["ham", "ham"])/sum(tab))
}

# Calculates Positive Predictive Value from confusion matrix TAB.
ppv = function(tab) {
  if (all(row.names(tab) == "ham")) {
    return(0)
  }
  return(tab["spam", "spam"]/sum(tab["spam",]))
}

# Calculates Negative Predictive Value from confusion matrix TAB.
npv = function(tab) {
  if (all(row.names(tab) == "ham")) {
    return(1)
  }
  return(tab["ham", "ham"]/sum(tab["ham",]))
}

# When LAB is one of the two class labels, returns the other class label.
other = function(lab) {
  if (lab == "ham") {
    return("spam")
  }
  if (lab == "spam") {
    return("ham")
  }
  return(NULL)
}

# Sensitivity or specificity depending on which LAB is passed in.
# Passing in "spam" give pct of actual spam classed as spam (sensitivity)
general.sens.spec = function(tab, lab) {
  if (all(row.names(tab) == lab)) {
    return(1)
  }
  if (all(row.names(tab) == other(lab))) {
    return(0)
  }
  return(tab[lab, lab]/sum(tab[,lab]))
}

#Given a list of ROCR performance objects, plots on one plot
plotROCS = function(performanceObjects, ...) {
  plot(performanceObjects[[1]], colorize = TRUE, ...)
  for (i in 2:length(performanceObjects)) {
    plot(performanceObjects[[i]], add = TRUE, colorize = TRUE)
  }
}

#Plots ppv, npv and accuracy of different costs on same axis

accPlot = function(costs, conf, main, low = 0.7) {
  plot(costs, sapply(conf, acc), main = main, xlab = "log cost", ylab ="", ylim = c(low, 1), type="o", pch=1)
  points(costs, sapply(conf, ppv), col = "green", type="o", pch = "+")
  points(costs, sapply(conf, npv), col = "red", type="o", pch = 23, cex = 1.5)
  legend("right", legend = c("Acc", "PPV", "NPV"), 
         bg="white", pch =c(1, 3, 23), 
         pt.bg=c("black", "green", "red"),
         col=c("black", "green", "red"),
         inset=c(0.01, 0.02))
}

############# Read word feature training matrix ############
word.mtx = read.csv(word.matrix.train.path, fileEncoding="WINDOWS-1252")
names(word.mtx)[length(word.mtx)] = "classification"

############# Choose word count limits #####################
### Calculate number of texts in which each word appears 
textsPerWord = apply(word.mtx[1:(length(word.mtx) - 1)], 2, function(x) sum(x > 0))

# number of total features as a function of min cutoff
par(mfrow=c(1,1))
mincut = 1:10
plot(mincut, sapply(mincut, function(x){return(sum(textsPerWord >= x))}), main = "Effect of Lower Cutoff on Feature Count", ylab = "Number of Word Features", xlab = "Lower Cutoff")

# print random words that appear in exactly 5 texts 
words.by.freq(word.mtx, 5)

# print 10 most common words
freq.order = order(textsPerWord)
tail(cbind(names(word.mtx)[freq.order], textsPerWord[freq.order]), 10)

# chosen cutoffs
lower = 5
upper = 500

# filter matrix by word count
filt.word.mtx = filterByCount(word.mtx, textsPerWord, lower, upper)
# Number of Features
length(filt.word.mtx)
# Number of Observations
nrow(filt.word.mtx)

############# Read and process test word matrix ################### 
test.word.mtx = read.csv(word.matrix.test.path, fileEncoding="WINDOWS-1252")
# Add zero columns for all words from training set not present in test data
for (colName in names(filt.word.mtx)) {
  if (!(colName %in% names(test.word.mtx))) {
    test.word.mtx[[colName]] = rep(0, nrow(test.word.mtx))
  }
}

# Remove all columns that are not in the filtered list of words
test.cleaned = test.word.mtx
#dropCols = c("classification") #collection of columns to drop
dropCols = c()
for (colName in names(test.cleaned)){
  if(!(colName %in% names(filt.word.mtx))){
    dropCols = c(dropCols, colName)    
  }
}
test.cleaned = test.cleaned[, !(names(test.cleaned) %in% dropCols)]

# Save the filtered test matrix to a csv.
write.table(test.cleaned, file="word_filtered_test.csv", sep=",", 
            row.names=FALSE, col.names=names(test.cleaned),
            fileEncoding="WINDOWS-1252")

############# Combine Word and Power Feature Matrices ######################
power.train = read.csv(power.matrix.train.path, fileEncoding="WINDOWS-1252")
power.test = read.csv(power.matrix.test.path, fileEncoding="WINDOWS-1252")

# Add ID column to all data frames
filt.word.mtx$ID = seq.int(nrow(filt.word.mtx))
test.cleaned$ID = seq.int(nrow(test.cleaned))
power.train$ID = seq.int(nrow(power.train))
power.test$ID = seq.int(nrow(power.test))

# Merge on the ID column
combined.train = merge.data.frame(filt.word.mtx, power.train, by.x="ID", by.y="ID")
combined.test = merge.data.frame(test.cleaned, power.test, by.x="ID", by.y="ID")

# There are redundant "classification" columns now. Let's remove them.
dropCols = c('classification.x', 'classification.y', "ID") 
combined.train$classification = combined.train$classification.x
combined.train = combined.train[, !(names(combined.train) %in% dropCols)]
combined.test$classification = combined.test$classification.x
combined.test = combined.test[, !(names(combined.test) %in% dropCols)]

print("DIMENSIONS (NOT COUNTING HEADER AND LABEL COLUMN)")
print(paste("COMBINED TRAINING MATRIX DIMENSIONS: ",dim(combined.train)[1],
      " by ", dim(combined.train)[2] - 1, sep = ""))
print(paste("COMBINED TEST MATRIX DIMENSIONS: ",dim(combined.test)[1],
            " by ", dim(combined.test)[2] - 1, sep = ""))

############# Choose cost for linear svm ###################
# Create confusion matrices for costs from 10^-4 to 10^0
set.seed(5)
logcosts = seq(-4, -0, .50)

# Make ROC curves for all costs
performances.combined = tune.confusion.mtx(combined.train, 10 ^ logcosts, k = 10, decision.values = TRUE)
save(performances.combined, file='performances_combined.RData')
plotROCS(performances.combined, main = "Combined Feature Linear SVM with Various Costs")

confusions.cobmined = tune.confusion.mtx(combined.train, 10^logcosts, k = 10)
save(confusions.cobmined, file='confusios_combined.RData')
# Plot accuracies, ppv, npv
accPlot(logcosts, confusions.cobmined, main="Accuracy, PPV and NPV", low = 0.7)

# From inspecting the ROC curves, we determined the optimal parameters.
# Choose cost
cost = 10 ^ -3.0
# Choose decision.boundary. From the ROC plots, it appears that the optimal value is around -0.1
decision.boundary = -0.1

############## Fit model on entire training set ###################
final.mod = svm(classification ~ ., data = combined.train, cost = cost, kernel = "linear")

############# Predict and Produce ROC Plot ###################### 
predy = attributes(predict(final.mod, combined.test, decision.values = TRUE))$decision.values
# ROC Plot
y = power.test$classification
predictionObj = prediction(predy, y, label.ordering = c("spam", "ham"))
performanceObj = performance(predictionObj, "tpr", "fpr")
plot(performanceObj, colorize=T, main="Validation Set")

############# Confusion Table and Accuracy Metrics ###################### 
# Create list of predicted class labels based on decision value and our chosen decision boundary.
pred.classification = sapply(predy, FUN = function(x) if (x >decision.boundary){
  return("ham")} else {return("spam")})
tab=table(pred.classification,y)
print(tab)
ppv(tab)
npv(tab)
general.sens.spec(tab, "spam")
general.sens.spec(tab,"ham")

############# Print the Combined Feature Matrix Dimensions ###################### 
print("DIMENSIONS (NOT COUNTING HEADER AND LABEL COLUMN)")
print(paste("COMBINED TRAINING MATRIX DIMENSIONS: ",dim(combined.train)[1],
            " by ", dim(combined.train)[2] - 1, sep = ""))
print(paste("COMBINED TEST MATRIX DIMENSIONS: ",dim(combined.test)[1],
            " by ", dim(combined.test)[2] - 1, sep = ""))
