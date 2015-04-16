\name{getContainerComponets}
\alias{getContainerComponets}
\alias{getContainerComponets,RedPort-method}

\title{Get container components.}

\description{
Method to get components (nested objects) of a specific container from an active RedeR session.
}

\usage{
getContainerComponets(obj, container)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{container}{Name of the container in the graph <string>}
}

\value{
Returns all nested objects assigned to a container <array of strings>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{

rdp <- RedPort('MyPort') 
el<-matrix(c('n1','n2','n3','n4'), ncol=2, byrow=TRUE)
g <- graph.edgelist(el)

\dontrun{

  calld(rdp)
  addGraph( rdp, g, layout.kamada.kawai(g) )
  nestNodes( rdp, c('n1','n2') )
  nestNodes( rdp, c("n3","n4") ) 
  updateGraph(rdp)
  getContainerComponets(rdp, "N0")
}  
}

\keyword{graph}

