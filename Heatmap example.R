# creating a matrix data with random numbers
# and plotting the matrix using the image() function
# you will see there, it does not have a real pattern in the plot.

set.seed(12345)
help(par)

# par can be used to set or query graphical parameters.
# Parameters can be set by specifying them as arguments
# to par in tag = value form, or by passing them as a list of tagged values

par(mar = rep(0.2,4))
data_Matrix<-matrix(rnorm(400),nrow=40)
data_Matrix
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

help(heatmap)
help(rep)
par(mar=rep(0.2,4))
heatmap(data_Matrix)
# When we run the heatmap() here, we get the dendrograms printed on the
# both columns and the rows and still there is no real immerging pattern that is interesting to us,
#it is because there is no real interesting pattern underlying in the data we generated


#adding pattern to the data by doing a ranom coin flip, using rbinom() function
help(rbinom)

set.seed(678910)
for(i in 1:40){
  #flipping coin and getting the data
  coin_flip<-rbinom(1,size=1,prob=0.5)
  #if heads, add common pattern to that row
  if(coin_flip){
    data_Matrix[i,]<-data_Matrix[i,]+rep(c(0,3),each=5)
  }
}
#above code:
  #looped through every row and selected random row to flip coin
  #if coin flip was heads (TRUE), pattern was added that the five...
  #columns would have a mean of 0, and other would have mean of 3
  #WHICH 5 COLUMNS MEAN OF 0???

#Plot will show right side yellow, with higher values, and left side...
#darker red with lower values. Because some rows have mean of 3 on right..
#side and some have mean of 0; pattern was introduced.
par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

heatmap(data_Matrix)

#Dendrogram clearly splits heatmap into 10 clusters, 5 on each side
#analyze patterns closer by looking at marginal means of rows and columns
#10 column means and 40 row means
hh<-hclust(dist(data_Matrix))
data_Matrix_Ordered<-data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1,xlab="The Row Mean",ylab="Row",pch=19)
plot(colMeans(data_Matrix_Ordered),xlab="The Column Mean",ylab="Column",pch=19)

#Interpretation