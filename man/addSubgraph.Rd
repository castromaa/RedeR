\name{addSubgraph}
\alias{addSubgraph}
\alias{addSubgraph,RedPort-method}

\title{Add subgraphs to RedeR application.}

\description{
Method to send subgraph to RedeR app.
}

\usage{
addSubgraph(obj, g, nodes, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{g}{ An igraph object. }  
  \item{nodes}{Nodes of the subgraph <array of strings>}
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{       
Additional arguments: 
\describe{
  \item{gatt}{A list of graph attributes. See attribute syntax in \code{\link[RedeR:addGraph]{addGraph}} } 
  \item{gscale}{Expansion factor of the graph area related to the app panel (default = 75) <numerics>. } 
  \item{gcoord}{Sets the graph x,y center. Coords between 0 and 100 are set to the visible area of the app panel (default = c(75,75)) <numeric vector>. } 
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5'} 
}
}

\value{
Extracts subgraphs from 'igraph' objects and sends the result to the RedeR app. 
}


\author{Mauro Castro}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
\code{\link[RedeR:addSubgraph.list]{addSubgraph.list}}
}

\examples{
# Initialize igraph
library(igraph)

\dontrun{

rdp <- RedPort()
calld(rdp)

g <- graph.lattice(c(5,5,5))

#..extracts a subgraph from g and sends to RedeR:
addSubgraph( rdp, g, nodes=c(1:10) )

#..sets some attributes on g prior to extraction!
g$isNest<-TRUE
g$nestColor="#ff0000"
g$scale=50
addSubgraph( rdp, g, nodes=c(1:10) )

#..alternatively, sets an independent list of attributes:
att <-list()
att$isNest<-TRUE
att$nestColor="#0000ff"
att$scale=50
att$coordX=25
att$coordY=25
addSubgraph( rdp, g, nodes=c(20:30), gatt=att )

#..for further attributes see 'addGraph' function!
}
}

\keyword{graph}
