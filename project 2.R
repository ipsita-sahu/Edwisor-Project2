rm(list=ls(all=T))
#set the working directory
setwd("G:/edwisor_project2")
getwd()
#load the data
dataset=read.csv("day.csv",header = T,na.strings = c(" ","",NA))
str(dataset)
dim(dataset)


#factorization of data
factor=c("season","yr","mnth","holiday","weekday","workingday","weathersit")
for(i in factor){
  dataset[,i]=as.factor(dataset[,i])
}

#Dropping of casual and registered count
dataset=dataset[,-c(14,15)]

#Distributionn of data
library(ggplot2)

ggplot(data=dataset, aes_string(x = dataset$atemp, y = dataset$cnt)) + 
  geom_point(aes_string(colour = dataset$season),size = 2) +
  theme_bw()+ ylab("cnt") + xlab("atemp") + ggtitle("atemp vs count") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5))+
  scale_colour_discrete(name="season")
  

ggplot(data=dataset, aes_string(x = dataset$windspeed, y = dataset$cnt)) + 
  geom_point(aes_string(colour = dataset$season),size = 2) +
  theme_bw()+ ylab("cnt") + xlab("windspeed") + ggtitle("windspeed vs count") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5))+
  scale_colour_discrete(name="season")


ggplot(data=dataset, aes_string(x = dataset$temp, y = dataset$cnt)) + 
  geom_point(aes_string(colour = dataset$season),size = 2) +
  theme_bw()+ ylab("cnt") + xlab("temp") + ggtitle("temp vs count") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5))+
  scale_colour_discrete(name="season")

ggplot(data=dataset, aes_string(x = dataset$hum, y = dataset$cnt)) + 
  geom_point(aes_string(colour = dataset$season),size = 2) +
  theme_bw()+ ylab("cnt") + xlab("hum") + ggtitle("hum vs count") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5))+
  scale_colour_discrete(name="season")

#ggplot(dataset, aes(x=dataset$season)) +
  
#geom_bar(position="dodge")+theme_bw()+ggtitle("month vs working day")


ggplot(dataset, aes(x = atemp, y = cnt, color = weekday)) +
  
geom_smooth(method = "loess", fill = NA, size = 1) +
  
theme_light(base_size = 11) +
  
xlab("atemp") +
  
ylab("count of Bike Rentals") +
  
ggtitle("count VS atemp") +
  
  
  
scale_color_discrete(name = "Weekday:",
                       
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       
                       labels = c("sun","mon","tue","wed","thurs","fri","sat"))+
  
theme(plot.title = element_text(size = 11, face="bold"))

ggplot(dataset,aes(x=season,y=cnt,fill=season))+
geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("season vs count")





#checking of missing value
sum(is.na(dataset))

#saving numeric varible in cnames
numeric_index = sapply(dataset,is.numeric) 
numeric_data = dataset[,numeric_index]
cnames = colnames(numeric_data)
cnames

#saving categorical variable in cnames1
cnames1=c("temp","atemp","hum","windspeed")

#outlier plot
library(ggplot2)
for (i in 1:length(cnames1)) {
  assign(paste0("gn",i), ggplot(aes_string( y = (cnames1[i]), x= "cnt") , data = subset(dataset)) + 
           stat_boxplot(geom = "errorbar" , width = 0.5) +
           geom_boxplot(outlier.color = "red", fill = "grey", outlier.shape = 20, outlier.size = 1, notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cnames1[i], x= "cnt")+
           ggtitle(paste("Boxplot" , cnames1[i])))
  #print(i)
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=3,nrow=2)

#removing of outlier from dataset
for(i in cnames1){
val = dataset[,i][dataset[,i] %in% boxplot.stats(dataset[,i])$out]
print(length(val))
dataset=dataset[which(!dataset[,i] %in% val),]
}

#corrlation plotting
library(corrgram)
corrgram(dataset[,cnames1], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
attach(dataset)

#ANOVA Test
m1=aov(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit)
summary(m1)


#standardization
for(i in cnames1){
    print(i)
    dataset[,i] = (dataset[,i] - mean(dataset[,i]))/
                                    sd(dataset[,i])
}
View(dataset)
#Final dataset after dropping of certain features
dataset=subset(dataset,select=-c(instant,dteday,holiday,temp,weekday))

library(rpart)
library(MASS)
set.seed(123)
train_index = sample(1:nrow(dataset), 0.8 * nrow(dataset))
train = dataset[train_index,]
test = dataset[-train_index,]

# linear regression model
lm_mod=lm(cnt~.,train)
summary(lm_mod)
lm_prd=predict(lm_mod,test[,-9])
lm_prd
library(Metrics)
rmse(test[,9],lm_prd)


#kFold cross validation
library(caret)
custom <-trainControl(method="repeatedcv",
                      number=10,
                      repeats=5,
                      verboseIter=T)

set.seed(123)
lm <-train(cnt ~.,train,method ='lm',trControl=custom)
lm$results
lm
summary(lm)


#Ridge Regression
set.seed(123)

library(glmnet)
ridge <-train(cnt~.,
              train,
              method='glmnet',
              tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,2,length=10)),
              trControl=custom)
plot(ridge)
ridge
plot(varImp(ridge,scale = T))

#lasso regression
set.seed(123)
lasso <-train(cnt~.,
              train,
              method='glmnet',
              tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,0.2,length=5)),
              trControl=custom)
lasso
plot(varImp(lasso,scale = T))

#elastic 
set.seed(123)
en <-train(cnt~.,
              train,
              method='glmnet',
              tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,1,length=5)),
              trControl=custom)
en

model_list <-list(LinearModel=lm,Ridge=ridge,Lasso=lasso,ElasticNet=en)
res <-resamples(model_list)
summary(res)
xyplot(res,metric = 'RMSE')
en$bestTune







#Decision Tree
fit = rpart(cnt ~ ., data = train, method = "anova")
fit
predictions_DT = predict(fit, test[,-9])
predictions_DT
library(Metrics)
rmse(test[,9],predictions_DT)




#Random forest
library(randomForest)
library(ggplot2)
library(inTrees)
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)
RF_model
treeList = RF2List(RF_model)  
exec = extractRules(treeList, train[,-9])
exec[1:2,]
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
ruleMetric = getRuleMetric(exec, train[,-9], train$cnt)  
ruleMetric[1:2,]
rf_prd=predict(RF_model,test[,-9]) 
rf_prd
summary(rf_prd)
library(Metrics)
rmse(test[,9],rf_prd)


#xg boost

features=c("season","yr","mnth","weekday","workingday","weathersit","atemp","hum","windspeed")
xgb_mod <- xgboost(data = data.matrix(train[,features]),label = train$cnt,objective="reg:linear",eval_metric="rmse",max.depth=5,nround = 10)                      
y_pred = predict(xgb_mod, newdata = data.matrix(test[,features]))
y_pred
library(Metrics)
rmse(test$cnt,y_pred)





