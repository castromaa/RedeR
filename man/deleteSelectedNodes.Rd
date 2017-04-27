\name{deleteSelectedNodes}
\alias{deleteSelectedNodes}
\alias{deleteSelectedNodes,RedPort-method}

\title{Delete selected nodes in RedeR graphs.}

\description{
Remove all selected nodes from an active RedeR session.    
}

\usage{
deleteSelectedNodes(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Remove graph objects.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
  \code{\link[RedeR:RedPort]{RedPort}},
  \code{\link[RedeR:selectAllNodes]{selectAllNodes}}, 
  \code{\link[RedeR:selectNodes]{selectNodes}},
  \code{\link[RedeR:selectNodes]{deSelectNodes}}
}

\examples{
# Initialize igraph
library(igraph)

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)  
  addEdges(rdp, edges)
  selectNodes(rdp,c("n3","n4"))
  deleteSelectedNodes(rdp)
  updateGraph(rdp)
}  
}

\keyword{graph}

