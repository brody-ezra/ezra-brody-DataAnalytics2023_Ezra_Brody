#PCA practice with the iris dataset
#create new dataset from iris that doesnt have the species type (column 5)
Iris <- iris[-c(5)]
head(Iris)
#pincipal components analysis command
help(princomp)
principal_components <- princomp(Iris, cor = TRUE, score = TRUE)
  #using corelation matrix since there are no constant variables?
  #TRUE for calculating score for each PC
summary(principal_components)
  #component 1 accounts for .7296 of variance
  #component 2 accounts for .2285 of variance
  #component 3 accounts for .0367 of variance
  #component 4 accounts for .0052 of variance
  #compomentns 1 and 2 account for ~95% of variance
#using plot() to visualize the components
plot(principal_components)
  #variances are not by proportion? what does y-axis measure?
#using line plot
plot(principal_components, type = "l")
#using biplot to plot the multiple components
help(biplot)
biplot(principal_components)
  #petal length and petal width arrows are close
  #sepal length then sepal width are farther
  #does this determine which variables are most of the variance?