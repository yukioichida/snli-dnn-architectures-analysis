library(car)
library(DTK)
library(FSA)
library(dplyr)


customizedPlot <- function (x = "DTK.test output") {
  out = x[[2]]
  a = x[[1]]
  n = nrow(out)
  plot(c(max(out[, 3]), min(out[, 2])), c(1, n), type = "n", xlab = "Mean Difference", ylab = "Mean Comparison", yaxt = "n", las=1)
  
  axis(2, at = seq(1, n), labels = rownames(out), las=1)
  title(main = paste(paste(((1 - a) * 100), "%", sep = ""), "Confidence Intervals"))
  
  for (i in 1:n) {
    lines(x=c(-1e100,1e100),y=c(i,i),col="gray",lty=3)
    if(out[i,2]>0 || out[i,3]<0){
      lines(out[i, 2:3], y = c(i, i),col="red",lwd=2, type = "l")
      points(x = out[i, 1], y = i,col="red")
    } else{
      lines(out[i, 2:3], y = c(i, i), lwd=2)
      points(x = out[i, 1], y = i)
    }  
  }
}

df <- read.csv("dataset.csv")
df$category[df$architecture == "LSTM"] <- "A"
df$category[df$architecture == "BiLSTM"] <- "B"
df$category[df$architecture == "Tree-LSTM"] <- "C"
df$category[df$architecture == "Ensemble"] <- "D"
df$category[df$architecture == "CNN"] <- "E"

# Removing GRU
df <- df %>% filter(architecture != "GRU")

df$overfitting_score <- df$train.acc - df$test.acc

# Count of each architecture
table(df$architecture)
table(df$category)

Summarize(test.acc ~ architecture, data=df, digits=2)

# First analysis: Overfitting score (train.acc - test.acc)
DTK.result = DTK.test(x = df$overfitting_score, f = df$category)
Summarize(overfitting_score ~ architecture, data=df, digits=2)
customizedPlot(DTK.result)
ajuste = lm(data = df, overfitting_score ~ .)
anova(ajuste)

# Second analysys: Generalization capacity: test.accuracy
DTK.result = DTK.test(x = df$test.acc, f = df$category)
Summarize(test.acc ~ architecture, data=df, digits=2)
customizedPlot(DTK.result)


# Third Analysis: number of required parameters
df$m <- as.numeric(sub("m", "e6", df$parameters, fixed = TRUE))
df$k <- as.numeric(sub("k", "e3", df$parameters, fixed = TRUE))
df$parameters <- if_else(is.na(df$m), df$k/100000, df$m/100000) 

Summarize(parameters ~ architecture, data=df, digits=2)
DTK.result = DTK.test(x = df$parameters, f = df$category, a=0.05)
customizedPlot(DTK.result)

