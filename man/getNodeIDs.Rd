\name{getNodeIDs}
\alias{getNodeIDs}
\alias{getNodeIDs,RedPort-method}

\title{
Get node IDs.
}

\description{
Method to get node attributes 'node IDs' from an active RedeR session. 
}

\usage{
getNodeIDs(obj, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class.}
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
Returns node attributes <array of numerics>
}

\author{
Mauro Castro
}

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
  getNodeIDs(rdp)
}  
}

\keyword{graph}
