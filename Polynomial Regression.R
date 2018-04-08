# Polynomial Regression #

# setwd()

dataset <- read.csv('filename.csv')

# no need to split

# fit a linear regression model first
lin_reg = lm(formula = dep ~ level, # replace dep with dependent variable, replace level with independent 
             data = dataset)
lin_reg


# Polynomial Regression
# create new columns of your indepedent variable to different powers
dataset$level2 = dataset$level^2
dataset$level3 = dataset$level^3
poly_reg = lm(formula = dep ~ level, # replace and dep and level again
              data = dataset)
summary(poly_reg)
# summary shows decrease in p value for each additional level


# Visualise Linear Regression
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$level, y = dataset$dep),
             colour = 'red') +
  geom_line(aes(x = dataset$level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') + 
  ggtitle('Linear Regression ggplot2') +
  xlab('level') +
  ylab('dep')


#Visualise POLYNOMIAL Regression
# don't change geom_point, as we want the REAL data points not the predictions
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$dep),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') + 
  ggtitle('Polynomial Regression ggplot2') +
  xlab('level') +
  ylab('dep')


# predicting a new result with linear
y_pred = predict(lin_reg, data.frame(Level = z)) #replace z with given prediction
y_pred


# predicting a new result with with even higher polynomials
# use the predict function to show the incremental decrease in p value between levels
y_pred = predict(poly_reg, data.frame(Level = z, # replace z with the value or level you are predicting
                                      Level2 = z^2,
                                      Level3 = z^3,
                                      Level4 = z^4))
y_pred

