\name{setNodeWeight}
\alias{setNodeWeight}
\alias{setNodeWeight,RedPort-method}

\title{Set node weight.}

\description{
Method to set node attribute 'weight' in active RedeR sessions.
}

\usage{
setNodeWeight(obj, node, weight)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Node name <string>}
  \item{weight}{Node weight <numeric>}
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
\code{\link[RedeR:getNodeWeight]{getNodeWeight}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  updateGraph(rdp)
  setNodeWeight(rdp, "n1", 2.0)
  getNodeWeight(rdp)
  #Available to internal RedeR methods (e.g. transfer att to node color or size) 
}  
}

\keyword{graph}

