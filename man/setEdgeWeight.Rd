\name{setEdgeWeight}
\alias{setEdgeWeight}
\alias{setEdgeWeight,RedPort-method}

\title{Set edge weight.}

\description{
Method to set edge attribute 'weight' in active RedeR sessions.
}

\usage{
setEdgeWeight(obj, nodeA, nodeB, weight)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodeA}{Node name <string>}
  \item{nodeB}{Node name <string>}
  \item{weight}{Edge weight <numeric>}
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
\code{\link[RedeR:getEdgeWeight]{getEdgeWeight}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  updateGraph(rdp)
  setEdgeWeight(rdp, "n1", "n2", 0.5)
  #Check attribs in RedeR app and run 'relax'! 
}  
}

\keyword{graph}
