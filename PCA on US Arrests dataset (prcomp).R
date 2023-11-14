#PCA on US arrests dataset on base R studio
#"USArrests" dataset has 50 rows for 50 states in alphabetical order
data("USArrests")
help("USArrests")

#get the states
states = row.names(USArrests)

#explore data
names(USArrests)
summary(USArrests)
#very different means and general summary statistics
#apply() function lets us apply mean() to each row or col.
  #rows input is 1, columns input is 2
apply(USArrests, 2, mean)
#Murder  Assault UrbanPop     Rape 
#7.788  170.760   65.540   21.232 
#examine variance through apply function
apply(USArrests, 2, var)
#way different variances, really high for assault

#we need to scale the data when applying the PCA model
#the urbanpop variable is not directly compareable to the number
#of rapes in a state per 100,000 people
#if we dont scale before, the prinicple components we get will
#be driven up from assaults variable due to its high mean and
#variance values
#standardize variables to have mean = 0 and sd = 1 before PCA

#using prcomp() function, we can set this standardization
#the default function sets variable means to be 0
#scale=TRUE option sets sd = 1
pr.out = prcomp(USArrests, scale = TRUE)
#look at what the function gave us
names(pr.out)
#center = means, scale = SD for variables used for scaling
#prior to implementing PCA
pr.out$scale
pr.out$center
  #matches with calculated means from before
#rotation matrix = principle component loadings
  #each column of pr.out$rotation contains the corresponding
  #PC loading vector
pr.out$rotation
  #4 distinct principal components

#using prcomp() function, we dont have to multiple the data
#by each PC loading vectors to obtain PC score vectors.
#the 50 x 4 matrix x has its columns as the PC score vectors
#i.e. the kth column is the kth PC score vector
dim(pr.out$x)

#plot the first 2 PC
biplot(pr.out, scale = 0)
  #scale = 0 ensures arrows are scaled to represent loadings

#sdev measure from the prcomp() function give stdev for each PC
pr.out$sdev

#variance explained by each principal component is obtained by
#squaring stdev
pr.var = pr.out$sdev^2

#to compute proportion of variance explained by each PC
#divide variance explained by each PC by total variance from all 4
pve = pr.var/sum(pr.var)
pve
  #first PC accounts for 62% of variance
  #second accounts for 24.7%