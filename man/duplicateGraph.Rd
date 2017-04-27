\name{duplicateGraph}
\alias{duplicateGraph}
\alias{duplicateGraph,RedPort-method}

\title{Duplicate graphs in RedeR application.}

\description{
Method to duplicate graphs and subgraphs of a network.
}

\usage{
duplicateGraph(obj, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{       
Additional arguments: 
\describe{
  \item{isToCopyEdges}{Logical value, whether to include edges to the copy (default = TRUE). }
  \item{isDefaultCopy}{Logical value, whether to duplicate the complete network or to copy only the original graph (default = TRUE). } 
  \item{nodes}{Optional: nodes to be duplicated <array of strings> (p.s. in this case, isDefaultCopy=TRUE). }
}
}

\value{
Duplicates graphs in RedeR app. 
}


\author{Mauro Castro}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
}


\examples{
# Initialize igraph
library(igraph)

\dontrun{

rdp <- RedPort() 
calld(rdp)

###

g1 <- graph.lattice(c(3,3,3))
addGraph( rdp, g1, layout.kamada.kawai(g1) )
duplicateGraph(rdp)
}
}

\keyword{graph}
