# SVR

# setwd()

# import data
dataset <- read.csv('Position_Salaries.csv')

# Splitting the dataset into the Training set and Test Set
# #install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = .8) # replace dep_var
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling
training_set <- scale(training_set)
test_set <- scale(test_set)


# Fitting the SVR to the dataset
# install.packages('e1071')
library(e1071)
regressor <- svm(formula = Salary ~ ., 
                data = dataset,
                type = 'eps-regression')

# predicting a new result
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualise against data
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$indep_var, y = dataset$dep_var),
             colour = 'red') +
  geom_line(aes(x = dataset$indep_var, y = predict(regressor, newdata = dataset)),
            colour = 'blue') + 
  ggtitle('Support Vector Regression') +
  xlab('indep_var') +
  ylab('dep_var')



 