\name{deSelectGraph}
\alias{deSelectGraph}
\alias{deSelectGraph,RedPort-method}

\title{Unmark selected graph objects. }

\description{
Unmark all selected objects in an active RedeR session. 
}

\usage{
deSelectGraph(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Unmark graph.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
  \code{\link[RedeR:RedPort]{RedPort}},
  \code{\link[RedeR:selectNodes]{selectNodes}}, 
  \code{\link[RedeR:selectEdges]{selectEdges}},
  \code{\link[RedeR:selectGraph]{selectGraph}}  
}

\examples{
# Initialize igraph
library(igraph)

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)  
  addEdges(rdp, edges)
  selectGraph(rdp)
  deSelectGraph(rdp)
  updateGraph(rdp)
}  
}

\keyword{graph}
