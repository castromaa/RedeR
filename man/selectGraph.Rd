\name{selectGraph}
\alias{selectGraph}
\alias{selectGraph,RedPort-method}

\title{Select graph.}

\description{
Method to mark all objects in an active RedeR application. Selected objects are put available for other methods. It can be done interactively as well.
}

\usage{
selectGraph(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Mark graph.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
  \code{\link[RedeR:RedPort]{RedPort}},
  \code{\link[RedeR:deleteSelectedNodes]{deleteSelectedNodes}}, 
  \code{\link[RedeR:deleteSelectedEdges]{deleteSelectedEdges}},
  \code{\link[RedeR:deSelectGraph]{deSelectGraph}}  
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
  updateGraph(rdp)
}  
}

\keyword{graph}
