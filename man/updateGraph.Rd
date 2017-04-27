\name{updateGraph}
\alias{updateGraph}
\alias{updateGraph,RedPort-method}

\title{
Update RedeR graphs.
}

\description{
Updates an active RedeR application session. 
}

\usage{
updateGraph(obj)
}

\arguments{
  \item{obj}{ Object of RedPort Class.}
}

\value{
Updates RedeR graph.
}

\author{
Mauro Castro
}

\note{
Prior calling this method make sure that there is an active RedeR session.
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
# Initialize igraph
library(igraph)

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)
  addEdges(rdp, edges)  
  updateGraph(rdp)
}  
}

\keyword{graph}
