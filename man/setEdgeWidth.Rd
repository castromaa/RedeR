\name{setEdgeWidth}
\alias{setEdgeWidth}
\alias{setEdgeWidth,RedPort-method}

\title{Set edge width.}

\description{
Method to set edge attribute 'width' in active RedeR sessions.
}

\usage{
setEdgeWidth(obj, nodeA, nodeB, width)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodeA}{Node name <string>}
  \item{nodeB}{Node name <string>}
  \item{width}{Options: > 0.0 <numeric>}
}

\value{
Sets edge attribute <numeric>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getEdgeWidth]{getEdgeWidth}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setEdgeWidth(rdp, "n1", "n2", 4.0)
  updateGraph(rdp)
}  
}

\keyword{graph}

