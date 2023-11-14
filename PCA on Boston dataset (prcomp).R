#PCA from Boston dataset
library(MASS)
data(Boston, package="MASS")
help(Boston)

#using prcomp() function instead of princomp()
#whats the difference? why different for this dataset than iris?
#maybe that it allows for scalling? which we will use
help(prcomp)
pca_out <- prcomp(Boston, scale. = T)
summary(pca_out)
plot(pca_out)

#using biplot
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
#boston_pc has the same rows and columns as original, but contains
#the pca value for each one
head(boston_pc, 1)
summary(boston_pc)
