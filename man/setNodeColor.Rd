\name{setNodeColor}
\alias{setNodeColor}
\alias{setNodeColor,RedPort-method}

\title{Set node color.}

\description{
Method to set node attribute 'color' in active RedeR sessions.
}

\usage{
setNodeColor(obj, node, color)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Name <string>}
  \item{color}{e.g. "#ff0000" for red <hexadecimal>}
}

\value{
Sets node attribute <hexadecimal>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeColor]{getNodeColor}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeColor(rdp, "n1", "#ff0000")
  updateGraph(rdp)
}  
}

\keyword{graph}
