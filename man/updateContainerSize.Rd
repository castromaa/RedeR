\name{updateContainerSize}
\alias{updateContainerSize}
\alias{updateContainerSize,RedPort-method}

\title{Update container size.}

\description{
Updates the size of all containers in an active RedeR session.  
}

\usage{
updateContainerSize(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Updates RedeR's container objects.
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
  nestNodes( rdp, c("n2","n3") )
  updateContainerSize(rdp)  
  updateGraph(rdp)
}  
}

\keyword{graph}
