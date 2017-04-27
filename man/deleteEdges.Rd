\name{deleteEdges}
\alias{deleteEdges}
\alias{deleteEdges,RedPort-method}

\title{Remove edges from RedeR graphs.}

\description{
Method to remove edges between nodes in an active RedeR session.  
}

\usage{
deleteEdges(obj, edges)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{edges}{Edge sequence as an array <array of strings> }
}

\value{
Removes the specified edges from the graph.
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
  deleteEdges(rdp, c("n1","n3","n1","n7") )
  updateGraph(rdp)
}  
}

\keyword{graph}

