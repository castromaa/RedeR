\name{mergeNodes}
\alias{mergeNodes}
\alias{mergeNodes,RedPort-method}

\title{Merge nodes.}

\description{
Merge nodes in an active RedeR session and build a new group.  
}

\usage{
mergeNodes(obj, nodes)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodes}{Node sequence <array of strings>}
}

\value{
Add/change graph objects.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
# Initialize igraph
library(igraph)

nodes<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)
  addNodes(rdp, nodes)
  mergeNodes(rdp,c("n2","n3","n4"))
  updateGraph(rdp)
}  
}

\keyword{graph}
