# Simple Linear Regression #


# set working directory
dataset <- read.csv('Salary_Data.csv')
head(dataset)

# simply note which variable is dependent on which independent variable 

# install.packages(CaTools) for random sampling 
library(caTools)
set.seed(123) # this makes R remember the randomisation, so you get the same results each time 

split = sample.split(dataset$dep, # simply reaplce dep with the column name of the dependent variable
                     SplitRatio = 2/3) # tells it 2/3 one side as TRUE

head(split) # view the split


# divide data into subsets for training and testing the algorithm
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
str(training_set)
str(test_set)


# now to fit a regressor (this determines the model using the given variables)
regressor = lm(formula = dep ~ indep, # replace dep with the column name of your 
                                      # previously noted variable
                                      # and the same for the indep
               data = training_set)
summary(regressor) # take note of the Pr(>|t|) value (which is the p-value) for each variable


# predict the test set results using the training set
y_pred = predict(regressor, newdata = test_set)
y_pred
# this predicts the dependent using the trained linear model against test values


# now to visualise the training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$indep, y = training_set$dep),
            colour = 'red') +
  geom_line(aes(x = training_set$indep, 
            y = predict(regressor, newdata = training_set)),
            colour = 'blue') + 
  ggtitle('Training Set with Linear Regression)') + 
  xlab('x') + 
  ylab('y')

# if the model shows a relation, visulise the same line, but against the test set
ggplot() +
  geom_point(aes(x = test_set$indep, y = test_set$dep),
             colour = 'red') +
  geom_line(aes(x = training_set$indep, 
            y = predict(regressor, newdata = training_set)),
            colour = 'blue') + 
  ggtitle('Test set against Algorithm') + 
  xlab('x') + 
  ylab('y')

# does the given line still hold true?
