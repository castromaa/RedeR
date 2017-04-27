\name{getSourceEdgeIDs}
\alias{getSourceEdgeIDs}
\alias{getSourceEdgeIDs,RedPort-method}

\title{Get source-edge IDs.}

\description{
Method to get IDs of all 'source' edges from an active RedeR session.   
}

\usage{
getSourceEdgeIDs(obj, ...)
}

\arguments{                                 
  \item{obj}{Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. } 
}

\details{
Additional arguments:
\describe{
  \item{type}{Filter options. Valid arguments: <'node'>, <'container'> or <'all'>. Default='node'. } 
  \item{status}{Filter options. Valid arguments:  <'selected'>, <'nonselected'> or <'all'>. Default='all'} 
}
}

\value{
Returns 'source' edges <array of integers>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
\code{\link[RedeR:getGraph]{getGraph}}
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
  getSourceEdgeIDs(rdp)
}  
}

\keyword{graph}

