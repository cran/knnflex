`knn.probability` <-
function(train, test, y, dist.matrix, k=1, ties.meth="min") {

#number of predictions to make
n<-length(test)

#sort the indexes for the training and test sets
if (is.unsorted(train)) train<-sort(train)
if (is.unsorted(test)) test<-sort(test)

#only need the rows for the test data and columns
#for the training data
d<-dist.matrix[test,train]

#ensure y is a factor
y<-as.factor(y)

#only need the responses for the training data
if (length(y)>length(train)) y<-y[train]

#calculate closest neighbors and
#return aggregate response for the k closest neighbors
if (n==1) {
  d<-rank(d, ties.method = ties.meth)
  x<-classprob(y[d <= k])
  x<-data.frame(x)
  names(x)<-test
  row.names(x)<-levels(y)
  return(x)
  }
else {
  d<-t(apply(d,1,function(x) rank(x,ties.method=ties.meth)))
  x<-apply(d,1,function(x) classprob(y[x<=k]))
  row.names(x)<-levels(y)
  return(x)
  }
}
