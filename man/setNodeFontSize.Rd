\name{setNodeFontSize}
\alias{setNodeFontSize}
\alias{setNodeFontSize,RedPort-method}

\title{Set font size.}

\description{
Method to set node attribute 'font size' in active RedeR sessions.
}

\usage{
setNodeFontSize(obj, node, size)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Name <string>}
  \item{size}{Options: >= 0 <integer>}
}

\value{
Sets node attribute <integer>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeFontSize]{getNodeFontSize}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeFontSize(rdp, "n1", 50)
  updateGraph(rdp)
}  
}

\keyword{graph}
