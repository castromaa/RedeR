\name{setNodeSize}
\alias{setNodeSize}
\alias{setNodeSize,RedPort-method}

\title{Set node size.}

\description{
Method to set node attribute 'size' in active RedeR sessions.
}

\usage{
setNodeSize(obj, node, size)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Name <string>}
  \item{size}{Options: >= 0 <numeric>}
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
\code{\link[RedeR:getNodeSize]{getNodeSize}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeSize(rdp, "n1", 40.0)
  updateGraph(rdp)
}  
}

\keyword{graph}
