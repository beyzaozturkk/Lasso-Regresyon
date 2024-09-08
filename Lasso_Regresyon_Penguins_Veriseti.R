####LASSO REGRESYON###
penguins<-read.csv('C:/Users/Burak/OneDrive/Documents/Verisetleri/penguins.csv' , header = TRUE , sep = "," , dec = ".")
View(penguins)
names(penguins)
nrow(penguins)

penguins$sex[penguins$sex %in% c("", "unknown", "?")] <- NA
library(caret)
library(glmnet)
library(tidyverse)
library(VIM)
library(mice)
md.pattern(penguins)

fig <- aggr(penguins , col = c("orange" , "red") , labels = names(penguins),
            numbers = TRUE , sortVars = TRUE, cex.axis = 0.6 , 
            ylab(c("Histogram of Missing Values" , "Pattern"))
)
fig
class(penguins$sex)
penguins$sex<-as.factor(penguins$sex)
?mice
imputedData <- mice(data = penguins , m = 3 , maxit = 3 , 
                method = NULL , 
                defaultMethod = c("pmm","logreg","polyreg","lda"))
summary(imputedData)
names(imputedData)
imputedData$m
imputedData$imp
imputedData$imp$bill_length_mm
imputedData$imp$sex


## 3. imputation degerleri ile veri setini doldur. 
completed <- complete(imputedData , 1)
View(completed)

modelData <- completed %>% 
  mutate( species = as.factor(species),island=as.factor(island),sex=as.factor(sex)) %>%
  select(bill_length_mm , bill_depth_mm , flipper_length_mm ,body_mass_g,species,island,sex)

nrow(modelData)

## One Hot Encoding Dummy De??isken

#numerik degi??kenlerde bir degisiklik olmaz

modelData1<- model.matrix(body_mass_g ~.  , data  = modelData)
head(modelData1)


#train set ve test set bolme

set.seed(145)
train_test_set<- sample(1:nrow(modelData1),size=0.80*nrow(modelData1))

trainsetx<-modelData1[train_test_set,]
testsetx<-modelData1[-train_test_set,]

trainsety<- modelData$body_mass_g[train_test_set]
testsety<- modelData$body_mass_g[-train_test_set]

#lasso regresyonda alpha degeri 1 olur 
model<-glmnet(trainsetx,trainsety,alpha=1,lambda =10^seq(from=2,to=-2,by=-0.01))
model

plot(model , xvar = "lambda")
legend("bottomright" , lwd = 1 , col = 1:nrow(trainsetx) , legend = colnames(trainsetx) )

## Cross Validation
#uygun lambda degerini bulmak icin cross validation yapariz.

nrow(trainsetx)
modelcv <- cv.glmnet(trainsetx , trainsety , alpha = 1 , lambda = 10^seq(from=2,to=-2,by=-0.01) , nfolds = 10 )
plot(modelcv)

best_lambda <- modelcv$lambda.min
best_lambda # katsayilarimizi nasil g??ncelleyecegimizi g??sterir. 
#modelin kompleksitisi azaltilip test veri setinde daha iyi performan gostermesi saglanir

### Model Tahmin Performans Degerlendirmesi 


Lasso <- glmnet(trainsetx , trainsety, alpha = 1 , lambda = best_lambda) 

Lasso $beta #elde ettigimiz en iyi modelin katsayilari
Lasso 

predictions <- predict(Lasso , testsetx)
predictions

R2(predictions , testsety)
MAE(predictions , testsety)
RMSE(predictions , testsety)

##Normal Lineer modelde kuruldugunda sonuclar ne olur?

#lambda sifir olarak girildiginde hicbir etkisi olmayacak ve lm() fonksiyonu ile kurulmus model gibi ols ile katsayilar bulunmus olur.

modelOLS<- glmnet(trainsetx , trainsety , alpha = 1 , lambda = 0)
modelOLS$beta


predictionsOLS <- predict(modelOLS , testsetx)

R2(predictionsOLS , testsety)
MAE(predictionsOLS , testsety)
RMSE(predictionsOLS , testsety)

data_predict<- data.frame("predictions"=predictions,
                          "actuals"=testsety)
library(formattable)
formattable(data_predict)
