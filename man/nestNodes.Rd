\name{nestNodes}
\alias{nestNodes}
\alias{nestNodes,RedPort-method}

\title{Nest nodes to containers.}

\description{
Method to nest nodes in an active RedeR session.  
}

\usage{
nestNodes(obj, nodes, ...)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{nodes}{<array of strings>}
  \item{...}{ Additional arguments passed to RedeR application. } 
}

\details{       
Additional arguments: 
\describe{
  \item{nestImage}{Status of the container on the screen: <'plain'>, <'transparent'>, or <'hide'> (default = 'plain'). } 
  \item{isAssign}{Logical value, whether to assign the container name to the nested nodes (default = TRUE). }
  \item{isAnchor}{Logical value, whether is to anchor the container in dynamic layouts (default = FALSE). }
  \item{gscale}{Expansion factor of the nest area related to a parent nest -- or related to the app panel (default = 40) <numerics>. } 
  \item{gcoord}{Sets the nest c(x,y) center related to the parent center. Coords between 0 and 100 are set to the inner area (default = NULL) <numeric vector>. } 
  \item{parent}{Nest ID of a parent nest. Must be used with 'isAssign=TRUE' (default = NULL). } 
  \item{gatt}{A list with graph attributes. See nest attribute syntax in \code{\link[RedeR:addGraph]{addGraph}} }
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5', 'tm6' <string>. Alternatively, it can be a list with customized attributes. } 
}
}

\value{
Add/change graph objects.
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
}  
}

\keyword{graph}

