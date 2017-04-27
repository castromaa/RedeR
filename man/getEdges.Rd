\name{getEdges}
\alias{getEdges}
\alias{getEdges,RedPort-method}

\title{Get edges.}

\description{
Method to get all edges from an active RedeR application.  
}

\usage{
getEdges(obj, ...)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. }
}

\details{
Additional arguments:
\describe{
  \item{status}{Filter options. Valid arguments:  <'selected'>, <'nonselected'> or <'all'>. Default='selected'} 
  \item{type}{Filter options. Valid arguments: <'node'>, <'container'> or <'all'>. Default='node'. } 
}
}

\value{
Returns edges <array of strings>
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
  getEdges(rdp)
}  
}

\keyword{graph}

