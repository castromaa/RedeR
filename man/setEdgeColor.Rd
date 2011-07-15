\name{setEdgeColor}
\alias{setEdgeColor}
\alias{setEdgeColor,RedPort-method}

\title{Set edge color.}

\description{
Method to set edge attribute 'color' in active RedeR sessions. 
}

\usage{
setEdgeColor(obj, nodeA, nodeB, color)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodeA}{Name <string>}
  \item{nodeB}{Name <string>}
  \item{color}{e.g. "#ff0000" for red <hexadecimal>}
}

\value{
Sets edge attribute <hexadecimal>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getEdgeColor]{getEdgeColor}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setEdgeColor(rdp, "n1", "n2", "#ff0000")
  updateGraph(rdp)
}  
}

\keyword{graph}

