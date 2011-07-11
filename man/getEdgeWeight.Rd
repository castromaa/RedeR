\name{getEdgeWeight}
\alias{getEdgeWeight}
\alias{getEdgeWeight,RedPort-method}

\title{Get edge weight.}

\description{
Method to get edge attributes 'weight' from an active RedeR session.
}

\usage{
getEdgeWeight(obj, ...)
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
Returns edge attributes <array of strings>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:setEdgeWeight]{setEdgeWeight}}
\code{\link[RedeR:getGraph]{getGraph}}
}

\examples{

rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  updateGraph(rdp)
  getEdgeWeight(rdp)
}  
}

\keyword{graph}

