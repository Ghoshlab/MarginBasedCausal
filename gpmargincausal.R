install.packages(c("randomForest","survey","data.table"))
library(randomForest)
library(e1071)


rhc <- fread('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.csv')


### Logistic regression estimate of propensity scores
svm1=svm(factor(swang1)~factor(cat1)+factor(ca)+factor(cardiohx)+factor(chfhx)
       +factor(dementhx)+factor(psychhx)+factor(chrpulhx)+factor(renalhx)
       +factor(liverhx)+factor(gibledhx)+factor(malighx)+factor(immunhx)
       +factor(transhx)+factor(amihx)+age+factor(sex)+edu+das2d3pc+aps1
       +scoma1+meanbp1+wblc1+hrt1+resp1+temp1+pafi1+alb1+hema1+bili1
       +crea1+sod1+pot1+paco21+ph1+wtkilo1+factor(dnr1)+factor(ninsclas)
       +factor(resp)+factor(card)+factor(neuro)+factor(gastr)+factor(renal)
       +factor(meta)+factor(hema)+factor(seps)+factor(trauma)+factor(ortho)
       +factor(race)+factor(income), gamma=0.0001, data=rhc)

# Identify margin and estimated effect
rhc$margin <- rep(0,5735)
rhc$margin[svm1$index] <- 1

lm1 <- lm(I(dth30 == "Yes")~factor(swang1),data=rhc,subset=(margin == T))

# explainer for Traskin-Small

library(rpart)
library(rpart.plot)
tree1 <- rpart(margin~.,data=rhc)
zp <- prune(tree1, cp = 0.1)
prp(zp, type = 4, extra = 101, leaf.round = 1, fallen.leaves = TRUE,
    varlen = 0, tweak = 0.8)
