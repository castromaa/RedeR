\name{setEdgeType}
\alias{setEdgeType}
\alias{setEdgeType,RedPort-method}

\title{Set edge type.}

\description{
Method to set edge attribute 'type' in active RedeR sessions. 
}

\usage{
setEdgeType(obj, nodeA, nodeB, type)
}

\arguments{
 \item{obj}{Object of RedPort Class. }
  \item{nodeA}{Name <string>}
  \item{nodeB}{Name <string>}
  \item{type}{Options: 'SOLID, 'DOTTED, 'DOTTED_SHORT','LONG_DASH' <string> }
}

\value{
Sets edge attribute <string>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getEdgeType]{getEdgeType}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setEdgeType(rdp, "n1", "n2", "DOTTED")
  updateGraph(rdp)
}  
}

\keyword{graph}

