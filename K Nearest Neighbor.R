# K-Nearest Neighbors

# Classification Template

# Import
dataset <- read.csv('Social_Network_Ads.csv')
# View(dataset)
# Process, only want Age, Salary and Purchased
dataset = dataset[3:5]

# Split
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

# Feature Scaling
training_set[1:2] = scale(training_set[1:2])
test_set[1:2] = scale(test_set[1:2])

# Fitting K-NN to the Training Set and then predict Test results
# Create Classifier here
# install.packages('class')
library(class)
y_pred = knn(train = training_set[, -3], 
             test = test_set[, -3],
             cl = training_set[, 3],
             k = 5) 

# Making the Confusion Matrix (to test predictor)
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
# install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], 
               test = grid_set,
               cl = training_set[, 3],
               k = 5)
plot(set[, -3],
     main = 'K-NN (Training Set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1 , 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Now Visulaise on Test Set
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], 
                          test = grid_set,
                          cl = training_set[, 3],
                          k = 5)
plot(set[, -3],
     main = 'K-NN (Test Set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1 , 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))




