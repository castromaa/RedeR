\name{selectNodes}
\alias{selectNodes}
\alias{selectNodes,RedPort-method}

\title{Select nodes.}

\description{
Method to select nodes in an active RedeR application. Selected objects are put available for other methods. It can be done interactively as well.
}

\usage{
selectNodes(obj, nodes, nt=NULL)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodes}{Names of nodes (or containers) <string or array of strings>}
  \item{nt}{Optional for nested nodes: to restrict searching to a specific container <string>}
}

\value{
Mark nodes -- which can be handled by other methods.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:deleteSelectedNodes]{deleteSelectedNodes}}
}


\examples{
# Initialize igraph
library(igraph)

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)  
  addEdges(rdp, edges)
  selectNodes(rdp,c("n3","n4","n5"))
  updateGraph(rdp)
}  
}

\keyword{graph}
