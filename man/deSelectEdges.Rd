\name{deSelectEdges}
\alias{deSelectEdges}
\alias{deSelectEdges,RedPort-method}

\title{Unmark selected edges.}

\description{
Unmark all selected edges in an active RedeR session.  
}

\usage{
deSelectEdges(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Unmark edges.
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

  calld(rdp)  
  addEdges(rdp, edges)
  selectEdges(rdp,"n1","n3")
  deSelectEdges(rdp)
  updateGraph(rdp)
}  
}

\keyword{graph}
