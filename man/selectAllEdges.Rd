\name{selectAllEdges}
\alias{selectAllEdges}
\alias{selectAllEdges,RedPort-method}

\title{Select all edges.}

\description{
Method to mark all edges in an active RedeR application. Selected objects are put available for other methods. It can be done interactively as well.
}

\usage{
selectAllEdges(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Mark edges.
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
  selectAllEdges(rdp)
  updateGraph(rdp)
}  
}

\keyword{graph}
