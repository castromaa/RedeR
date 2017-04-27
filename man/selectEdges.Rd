\name{selectEdges}
\alias{selectEdges}
\alias{selectEdges,RedPort-method}

\title{selectEdges}

\description{
Select edges in an active RedeR application.  
}

\usage{
selectEdges(obj, nodeA, nodeB)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodeA}{<string>}
  \item{nodeB}{<string>}
}

\value{
Mark edges -- which can be handled by other methods.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
  \code{\link[RedeR:RedPort]{RedPort}},
  \code{\link[RedeR:deleteSelectedEdges]{deleteSelectedEdges}}
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
  updateGraph(rdp)
}  
}

\keyword{graph}


