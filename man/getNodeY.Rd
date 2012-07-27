\name{getNodeY}
\alias{getNodeY}
\alias{getNodeY,RedPort-method}

\title{
Get node y coordinate.
}

\description{
Method to get node attributes 'node Y position' from an active RedeR session. 
}

\usage{
getNodeY(obj, ...)
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
\code{\link[RedeR:getGraph]{getGraph}}
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeX]{getNodeX}, \link[RedeR:setNodeXY]{setNodeXY}}
}

\examples{

rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  updateGraph(rdp)
  getNodeY(rdp)
}  
}

\keyword{graph}

