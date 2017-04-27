\name{setArrowDirection}
\alias{setArrowDirection}
\alias{setArrowDirection,RedPort-method}

\title{Set arrow direction.}

\description{
Method to set edge attribute 'arrow direction' in active RedeR sessions.
}

\usage{
setArrowDirection(obj, nodeA, nodeB, direction)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodeA}{Name <string>}
  \item{nodeB}{Name <string>}
  \item{direction}{Options: 0 (A-B), 1 (A->B), 2 (A<-B) or 3 (A<->B)  <integer>}
}

\value{
Sets edge attribute <integer>
}

\author{Mauro Castro}

\note{
The direction is set according to the edge order in the app (i.e. the edge list available inside RedeR). So, if a request for direction "1" places nodeA='B' and nodeB='A', then the direction will appear as A->B in the app.
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
  setArrowDirection(rdp, "n1", "n2", 2)
  updateGraph(rdp)
}  
}

\keyword{graph}

