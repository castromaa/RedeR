\name{deSelectNodes}
\alias{deSelectNodes}
\alias{deSelectNodes,RedPort-method}

\title{Unmark selected nodes.}

\description{
Unmark all selected nodes in an active RedeR session. 
}

\usage{
deSelectNodes(obj)
}

\arguments{
 \item{obj}{Object of RedPort Class. }
}

\value{
Unmark nodes.
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
  selectNodes(rdp,c("n3","n4","n5"))
  deSelectNodes(rdp)
  updateGraph(rdp)
}  
}

\keyword{graph}
