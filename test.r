myplot <-
  function (x = "DTK.test output") {
    out = x[[2]]
    a = x[[1]]
    n = nrow(out)
    plot(c(max(out[, 3]), min(out[, 2])), c(1, n), type = "n", xlab = "Mean Difference", ylab = "Mean Comparison", yaxt = "n", las=1)
    
    axis(2, at = seq(1, n), labels = rownames(out), las=1)
    title(main = paste(paste(((1 - a) * 100), "%", sep = ""),
                       "Confidence Intervals"))
    
    
    
    for (i in 1:n) {
      lines(x=c(-1e100,1e100),y=c(i,i),col="gray",lty=2)
      if(out[i,2]>0 || out[i,3]<0){
        lines(out[i, 2:3], y = c(i, i),col="red",lwd=2)
        points(x = out[i, 1], y = i,col="red")
      } else{
        lines(out[i, 2:3], y = c(i, i))
        points(x = out[i, 1], y = i)
      }  
    }
  }

df <- read.csv("dataset.csv")
#df$architecture <- as.character(df$architecture)
#df$architecture[df$architecture == "BiLSTM"] <- "B"

df$category[df$architecture == "LSTM"] <- "A"
df$category[df$architecture == "GRU"] <- "B"
df$category[df$architecture == "BiLSTM"] <- "C"
df$category[df$architecture == "Tree-LSTM"] <- "D"
df$category[df$architecture == "Ensemble"] <- "E"
df$category[df$architecture == "CNN"] <- "F"

df$overfitting_score <- df$train.acc - df$test.acc


DTK.result = DTK.test(x = df$overfitting_score, f = df$category)

myplot(DTK.result)

DTK.result = DTK.test(x = df$test.acc, f = df$category)

myplot(DTK.result)
