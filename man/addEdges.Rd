\name{addEdges}
\alias{addEdges}
\alias{addEdges,RedPort-method}

\title{Add edges to RedeR graphs.}

\description{
Add edges to an active RedeR session.
}

\usage{
addEdges(obj, edges)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{edges}{Edge sequence as an array <array of strings>. }    
}

\value{
Adds the specified edges to the graph.
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
  updateGraph(rdp)
}  
}

\keyword{graph}

