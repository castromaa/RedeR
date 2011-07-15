\name{setNodeLineWidth}
\alias{setNodeLineWidth}
\alias{setNodeLineWidth,RedPort-method}

\title{Set node-line width.}

\description{
Method to set node attribute 'line width' in active RedeR sessions.
}

\usage{
setNodeLineWidth(obj, node, width)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Name <string>}
  \item{width}{Options: >= 0 <numeric>}
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
\code{\link[RedeR:getNodeLineWidth]{getNodeLineWidth}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeLineWidth(rdp, "n1", 5.5)
  updateGraph(rdp)
}  
}

\keyword{graph}
