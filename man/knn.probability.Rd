\name{knn.probability}
\alias{knn.probability}
\title{ KNN prediction probability routine using pre-calculated distances }
\description{
  K-Nearest Neighbor prediction probability method which uses the distances calculated by
  \code{\link{knn.dist}}.  For predictions (not probabilities) see \code{\link{knn.predict}}.
}
\usage{
knn.probability(train, test, y, dist.matrix, k = 1, ties.meth = "min")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{ indexes which specify the rows of \emph{x} provided to \code{\link{knn.dist}}
                to be used in making the predictions }
  \item{test}{ indexes which specify the rows of \emph{x} provided to \code{\link{knn.dist}}
                to make predictions for }
  \item{y}{ responses, see details below }
  \item{dist.matrix}{ the output from a call to \code{\link{knn.dist}} }
  \item{k}{ the number of nearest neighbors to consider }
  \item{ties.meth}{ method to handle ties for the kth neighbor, the default is "min" which
                    uses all ties, alternatives include "max" which uses none if there are ties
                    for the k-th nearest neighbor, "random" which
                    selects among the ties randomly and "first" which uses the ties in their
                    order in the data }
}
\details{
Prediction probabilities are calculated for each test case by aggregating the responses of the
k-nearest neighbors among the training cases and using the \code{\link{classprob}}.
\code{k} may be specified to be any positive
integer less than the number of training cases, but is generally between 1 and 10.

The indexes for the training and test cases are in reference to the order of the entire
data set as it was passed to \code{\link{knn.dist}}.

Only responses for the training cases are used.  The responses provided in y may be those
for the entire data set (test and training cases), or just for the training cases.

The ties are handled using the \code{\link{rank}} function.  Further information may be found
by examining the \code{ties.method} there.  
}
\value{
a matirx of prediction probabilities whose number of columns is the number of test cases and
the number of rows is the number of levels in the responses.  
}
\author{ Atina Dunlap Brooks }
\note{
For the traditional scenario, classification using the Euclidean distance on a fixed set
of training cases and a fixed set of test cases, the method \code{\link[class]{knn}} is ideal.
The functions \code{\link{knn.dist}} and \code{\link{knn.predict}} are intend to be
used when something beyond the traditional case is desired.  For example, prediction on 
a continuous y (non-classification), cross-validation for the selection of k,
or the use of an alternate distance method are well handled.
}
\seealso{ \code{\link{knn.dist}}, \code{\link{knn.predict}}, \code{\link[class]{knn}} }
\examples{
# the iris example used by knn(class)
library(class)
data(iris3)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
# how to get predictions from knn(class)
pred <- knn(train, test, cl, k = 3, prob=TRUE)
# display the confusion matrix
table(pred,cl)
# view probabilities (only the highest probability is returned)
attr(pred,"prob")

# how to get predictions with knn.dist and knn.predict
x <- rbind(train,test)
kdist <- knn.dist(x)
pred <- knn.predict(1:75, 76:150, cl, kdist, k=3)
# display the confusion matrix
table(pred,cl)
# view probabilities (all class probabilities are returned)
knn.probability(1:75, 76:150, cl, kdist, k=3)

# to compare probabilites, rounding done for display purposes
p1 <- knn(train, test, cl, k = 3, prob=TRUE)
p2 <- round(knn.probability(1:75, 76:150, cl, kdist, k=3), digits=2)
table( round(attr(p1,"prob"), digits=2), apply(p2,2,max) )

# note any small differences in predictions are a result of
# both methods breaking ties in majority class randomly

}
\keyword{ methods }
