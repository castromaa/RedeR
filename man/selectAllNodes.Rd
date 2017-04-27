\name{selectAllNodes}
\alias{selectAllNodes}
\alias{selectAllNodes,RedPort-method}

\title{selectAllNodes}

\description{
Mark all nodes in an active RedeR application. 
}

\usage{
selectAllNodes(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Mark nodes.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:deleteSelectedNodes]{deleteSelectedNodes}}
}

\examples{
# Initialize igraph
library(igraph)

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)  
  addEdges(rdp, edges)
  selectAllNodes(rdp)
  updateGraph(rdp)
}  
}

\keyword{graph}
