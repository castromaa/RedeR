\name{addEdgeBetweenContainers}
\alias{addEdgeBetweenContainers}
\alias{addEdgeBetweenContainers,RedPort-method}

\title{Add edges between containers.}

\description{
Method to add edges between RedeR containers. This method adds non-nested assignments, in contrast to the default behavior that builds nested associations to-and-from containers.
}

\usage{
addEdgeBetweenContainers(obj, containerA, containerB )
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{containerA}{<string>}
  \item{containerB}{<string>}  
}

\value{
Add graph objects.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
# Initialize igraph
library(igraph)

el<-matrix(c('n1','n2','n3','n4'), ncol=2, byrow=TRUE)
g <- graph.edgelist(el)

\dontrun{

  rdp <- RedPort()
  calld(rdp)
  
  addGraph( rdp, g, layout.kamada.kawai(g) )
  nestNodes( rdp, c('n1','n2') )
  nestNodes( rdp, c("n3","n4") ) 
  addEdgeBetweenContainers(rdp, "N0", "N1")
  updateGraph(rdp)
}  
}

\keyword{graph}

