
df <- read.csv("dataset.csv")

# Qaunti
table(df$architecture)

library(DTK)

DTK.test(x = df$test.acc, f = df$architecture)


DTK.result = DTK.test(x = df$test.acc, f = df$architecture)

DTK.plot(DTK.result)

plot(result , las=1 , col="brown" )

library(MultNonParam)

tukey.kruskal.test(df$test.acc, df$architecture)

model=lm( df$test.acc ~ df$architecture )
ANOVA=aov(model)

ANOVA

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'df$architecture', conf.level=0.95)
TUKEY

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )