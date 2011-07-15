\name{setNodeXY}
\alias{setNodeXY}
\alias{setNodeXY,RedPort-method}

\title{Set node x,y coordinates.}

\description{
Method to set node attribute 'XY position' in active RedeR sessions.
}

\usage{
setNodeXY(obj, node, x, y)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Node name<string>}
  \item{x}{x coord. <numeric>}
  \item{y}{y coord. <numeric>}
}

\value{
Sets node attribute <numeric>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeX]{getNodeX}, \link[RedeR:getNodeY]{getNodeY}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeXY(rdp, "n1", 200.0, 200.0)
  setNodeXY(rdp, "n2", 100.0, 100.0)
  setNodeXY(rdp, "n3", 300.0, 300.0)
  updateGraph(rdp)
}  
}

\keyword{graph}

