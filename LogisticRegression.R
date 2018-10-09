library(ggplot2)
library(magrittr)
library(ROCR)


avocado <- read.csv(file.choose(), header = T)
str(avocado)
avocado$Date <- as.Date(avocado$Date)

avocadoUSA <- avocado[avocado$region == "TotalUS",] #Look at only total 
ggplot(avocadoUSA, aes(y = type, x = AveragePrice, color = type)) + geom_point() + theme_minimal()
ggplot(avocadoUSA, aes(x = type, y = AveragePrice, fill = type)) + geom_boxplot()



val <- ifelse( avocadoUSA$type== 'organic', 1 , 0)
dat <- data.frame(class = val , value = avocadoUSA$AveragePrice, type = avocadoUSA$type)

ggplot(dat, aes(x=value, y=class)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=F)  + theme_minimal()



#Split the data:
set.seed(100)
indx <- sample(
  1:length(avocadoUSA[,1]),
  length(avocadoUSA[,1])*.8,
  replace = F
)
train <- avocadoUSA[indx,]
test <- avocadoUSA[-indx,]
(dim(train)[1] + dim(test)[1]) == dim(avocadoUSA)[1] # Make sure we split the data correctly



# Model fitting

model <- glm(type ~AveragePrice,family=binomial(link='logit'),data=train)
summary(model) #Coefficient is significant from 0


#Evaluation
p <- predict(model, newdata=test, type="response") #Response gets probabilities
pr <- prediction(p, test$type)
prf <- performance(pr, measure = "tpr", x.measure = "fpr") #True Positive and False Positive
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #Very close to 1


