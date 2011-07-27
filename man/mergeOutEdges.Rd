\name{mergeOutEdges}
\alias{mergeOutEdges}
\alias{mergeOutEdges,RedPort-method}

\title{Merge out-edges between connoted nests. }

\description{
Method to assign out-edges to containers in an active RedeR session. This method transfers edges from nodes to the respective containers.
}

\usage{
mergeOutEdges(obj,...)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. } 
}

\details{       
Additional arguments: 
\describe{
   \item{setNorm}{Logical value. Whether to normalize out-edge width by the total possible links between connoted nests; if false, it will run a simple sum (default=TRUE). }
   \item{lb}{Lower bound used to set the normalized edge width (default=NULL). }
   \item{ub}{Upper bound used to set the normalized edge width (default=NULL). }
}
}

\value{
Add/change container attributes.
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
el<-matrix(c("n1","n2","n1","n3","n1","n4","n2","n5","n2","n6","n2","n7"), ncol=2, byrow=TRUE)
g <- graph.edgelist(el)

\dontrun{

  calld(rdp)
  addGraph( rdp, g, layout.kamada.kawai(g) )
  nestNodes( rdp, c("n1","n2") )
  mergeOutEdges(rdp)  
  updateGraph(rdp)
}  
}

\keyword{graph}
