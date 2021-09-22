library(ggplot2) # Data visualization
library(plotly) # Interactive data visualizations
library(psych) # Will be used for correlation visualizations
library(rattle) # Graphing decision trees
library(caret) # Machine learning

data("iris")
head(iris)
summary(iris)
pairs.panels(
  iris[,1:4], # Our data.
  scale = TRUE, # Changes size of correlation value lables based on strength.
  hist.col = 'grey85', # Histogram color.  
  bg = c("mediumseagreen","orange2","mediumpurple1")[iris$Species], # Colors of the Species levels.
  pch = 21, # The plot characters shape and size.
  main = 'Correlation matrix of Iris data')  # Title. 

#The upper part of the correlataion matrix tells us the correlation between the varaibles and there is a moderate to strong correlation between
#all variables except for sepal length and sepal width.The bottom half of matrix gives scatter plots
#which have have been divided by species using color


#3D plot
plot_ly(data = iris,x=~Sepal.Length, y = ~Petal.Length, z = ~Petal.Width,
        color=~Species,
        type = "scatter3d",
        mode="markers"
        )%>%
  layout(
    scene=list(xaxis=list(title = 'Sepal length'), # Assign axes names
               yaxis = list(title = 'Petal length'),
               zaxis = list(title = 'Petal width'))
  )


#Boxplot of sepal width:
ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Width,fill=Species))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_light()+
  labs(title =  'Box plot of sepal width for each species', 
       x = 'Species', y = 'Sepal width')
# Boxplot of sepal length:

ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Length,fill=Species))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_light()+
  labs(title =  'Box plot of sepal Length for each species', 
       x = 'Species', y = 'Sepal Length')

# Boxplot of Petal width
ggplot(data = iris,mapping = aes(x=Species,y=Petal.Width,fill=Species))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_light()+
  labs(title =  'Box plot of Petal.Width for each species', 
       x = 'Species', y = 'Petal.Width')
# Boxplot of Petal length
ggplot(data = iris,mapping = aes(x=Species,y=Petal.Length,fill=Species))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_light()+
  labs(title =  'Box plot of Petal.Length for each species', 
       x = 'Species', y = 'Petal.Width')

#Data Partationing
set.seed(222)

train_index<-createDataPartition(y=iris$Species,p=.7,list = FALSE ,# Set results into matrix form.
                                times=1)
#Splitting into test and train data
train_data<-iris[train_index,]
test_data<-iris[-train_index]

#Machine Learning
#Decision tree
fitControl<-trainControl(method="cv",number = 10,savePredictions = TRUE)

#Creating the model
dt_model<-train(Species~.,data=train_data,method="rpart",#specify rf model
                trControl=fitControl)

#Check the predicted accuracy of our decision tree model by running it on resamples of our training data
confusionMatrix(dt_model)
#Results here tell us that our average accuracy is 90.48%
#Checking the importance of each feature in our model
dt_importance <-varImp(dt_model)
dt_importance
#Create plot of importance variables
ggplot(data = dt_importance,mapping = aes(x=dt_importance[,1]))+
  geom_boxplot()+
  labs(title = "Variable Importnace :Decision Tree model")+
  theme_light()
#Petal langth was the most important variable

#Plotting the decision tree
fancyRpartPlot(dt_model$finalModel,sub = '')
#The decision tree tells that if  sepal length is less than 2.6,the species prediction is setosa
#if petal length is between 2.6 and 4.8 the prediction is versicolor
#if petal length is greater than 4.8 the prediction is virginica

#Prediction for decision Tree
prediction_dt<-predict(dt_model,test_data)
#Check proportion of predictions which are accurate
table(predict_dt,test_data$Species)%>%prop.table()%>%round(2)

#Random Forest

fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
# Create model
rf_model <- train(
  Species ~ ., 
  method = 'rf',  
  trControl = fitControl, 
  data = train_data) 

# Create object of importance of our variables 
rf_importance <- varImp(rf_model) 

# Create box plot of importance of variables
ggplot(data = rf_importance, mapping = aes(x = rf_importance[,1])) + 
  geom_boxplot() + 
  labs(title = "Variable importance: Random forest model") +
  theme_light() 

#This plot tells us that petal length and width are the most important variable for prediction in our model.

#Testing for accuracy
confusionMatrix(rf_model)

#The output tells us that the predicted accuracy of our model is 94.29%.

#Prediction
prediction_rf <- predict(rf_model, test_data)

#Check the accuracy of our random forest model on our test data.
table(prediction_rf, test_data$Species) %>%  
  prop.table() %>% 
  round(2) 

#Naive Bayes
fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
# Create model
library(klaR) #fOr nb
nb_model <- train(Species ~ .,
                  data = train_data,
                  method = 'nb', 
                  trControl = fitControl) 

#Test for Accuracy
confusionMatrix(nb_model)

#The results here tell us that our average accuracy is 95.24% when testing our data on resamples of our training data.
# Create object of importance of our variables 
nb_importance <- varImp(nb_model) 

# Create box plot of importance of variables
ggplot(data = nb_importance, mapping = aes(x = nb_importance[,1])) + 
  geom_boxplot() + 
  labs(title = "Variable importance: Naive Bayes model") + # Title
  theme_light() # Theme

# We can see that petal width and length are the two most important variables for predicting each species. 


#Support Vector Machine (SVM)
itControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
#Model creation
svm_model <- train(Species ~ ., 
                   data = train_data, 
                   method = 'svmLinear', 
                   trControl = fitControl) 


#Check the predicted accuracy of our naive Bayes model by running it on resamples
confusionMatrix(svm_model)

# Create object of importance of our variables 
svm_importance <- varImp(svm_model)

# Create box plot of importance
ggplot(data = svm_importance, mapping = aes(x = svm_importance[,1])) + 
  geom_boxplot() + 
  labs(title = "Variable importance: Support vector machine model") + 
  theme_light() 

#  petal length and petal width are the two most important variables for predicting each species. 

# PREDICTION
prediction_svm <- predict(svm_model, test_data)

#Checking  proportion of the predictions which were accurate.
table(prediction_svm, test_data$Species) %>% 
  prop.table() %>% 
  round(2)
























