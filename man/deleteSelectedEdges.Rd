\name{deleteSelectedEdges}
\alias{deleteSelectedEdges}
\alias{deleteSelectedEdges,RedPort-method}

\title{Delete selected edges in RedeR graphs.}

\description{
Remove all edges selected in an active RedeR session.    
}

\usage{
deleteSelectedEdges(obj)
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
  \code{\link[RedeR:selectAllEdges]{selectAllEdges}}, 
  \code{\link[RedeR:selectEdges]{selectEdges}},
  \code{\link[RedeR:selectEdges]{deSelectEdges}}
}

\examples{
# Initialize igraph
library(igraph)

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)  
  addEdges(rdp, edges)
  selectEdges(rdp,"n1","n3")
  deleteSelectedEdges(rdp)
  updateGraph(rdp)
}  
}

\keyword{graph}

