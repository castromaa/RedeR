\name{setNodeLineColor}
\alias{setNodeLineColor}
\alias{setNodeLineColor,RedPort-method}

\title{Set node-line color.}

\description{
Method to set node attribute 'line color' in active RedeR sessions.
}

\usage{
setNodeLineColor(obj, node, color)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Name <string>}
  \item{color}{e.g. "#ff0000" for red <hexadecimal> }
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
\code{\link[RedeR:getNodeLineColor]{getNodeLineColor}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeLineColor(rdp, "n1", "#3333ff")
  updateGraph(rdp)
}  
}

\keyword{graph}
