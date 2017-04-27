\name{deleteNodes}
\alias{deleteNodes}
\alias{deleteNodes,RedPort-method}

\title{Remove nodes from RedeR graphs.}

\description{
Method to remove nodes from an active RedeR session.  
}

\usage{
deleteNodes(obj, nodes)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodes}{Node sequence as an array <array of strings> } 
}

\value{
Remove graph objects.
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

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)
  addEdges(rdp, edges)
  deleteNodes(rdp, c("n1","n3") )
  updateGraph(rdp)
}  
}

\keyword{graph}

