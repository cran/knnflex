`knn.dist` <-
function(x, dist.meth="euclidean", p=2) {
#create a distance matrix using all values in the data
d<-as.matrix(dist(x,dist.meth,p))
#fix for some small high persision errors
round(d,digits=15)
}

