library(readxl)
library(xlsx)

# get data

imdb_data1 <- read_excel("final project data.xlsx", sheet=1, col_names=T)
imdb_data2 <- imdb_data1[,-c(1,2,7,10,11,12,16,19,20,21,47,49,50,51,52)]
imdb_data <- imdb_data2
imdb_data <- na.omit(imdb_data)

#Linear regression
fit <- lm(imdb_score ~ Twitter_Rating, data=imdb_data)
summary(fit)
library(ggplot2)
ggplot(imdb_data, aes(x=Twitter_Rating, y=imdb_score)) +geom_point(shape=1) + geom_smooth(method=lm)

#Create training and testing datasets
sample.ind = sample(2,  
                    nrow(imdb_data),
                    replace = T,
                    prob = c(0.7,0.3))
data.train = imdb_data[sample.ind==1,]  
data.test = imdb_data[sample.ind==2,]  

# Original Data distibution of oscar nominated movies 
table(imdb_data$oscar_nomination)/nrow(imdb_data) 

# Training Data
table(data.train$oscar_nomination)/nrow(data.train)  

# Testing Data
table(data.test$oscar_nomination)/nrow(data.test) 

#Fit Random Forest Model
#install.packages("randomForest")
 
library(randomForest)
data.train$oscar_nomination = factor(data.train$oscar_nomination)

rf_data = randomForest(oscar_nomination ~ .,  
                  ntree = 800,
                  data = data.train[,-c(20)])
plot(rf_data)
print(rf_data)

# Variable Importance
varImpPlot(rf_data,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")

#Variable Importance
var.imp = data.frame(importance(rf_data,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

rf = randomForest(oscar_nomination ~ .,  
                       ntree = 800,
                       data = data.train[,c(6,7,8,9,11,12,13,15,37)])

# Predicting response variable
data.train$predicted.response = predict(rf , data.train[,c(6,7,8,9,11,12,13,15,37)])

# Create Confusion Matrix
#install.packages('e1071', dependencies=TRUE) 
library(caret)
print(  
  confusionMatrix(data = data.train$predicted.response,  
                  reference = data.train$oscar_nomination))
   #               positive = "oscarnomination"))

# Predicting response variable
data.test$predicted.response <- predict(rf ,data.test)

# Create Confusion Matrix
print(  
  confusionMatrix(data=data.test$predicted.response,  
                  reference=data.test$oscar_nomination))
                  #positive='oscar_nomination'))
##########################################################################################
#Apply prediction on full data

imdb_data$predicted.response <- predict(rf ,imdb_data)

# Create Confusion Matrix
print(  
  confusionMatrix(data=imdb_data$predicted.response,  
                  reference=imdb_data$oscar_nomination))
                  #positive='className')) 

write.xlsx(imdb_data, "c:\\Users\\achaina\\Desktop\\MS BAIM\\Big Data Analytics\\final project\\predicted_data.xlsx")

