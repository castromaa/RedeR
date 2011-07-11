\name{nestSubgraph}
\alias{nestSubgraph}
\alias{nestSubgraph,RedPort-method}

\title{Show subgraphs in RedeR main panel.}

\description{
Method to show graphs and subgraphs in a network.
}

\usage{
nestSubgraph(obj, nodes, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{nodes}{ Nodes to be nested. }   
  \item{...}{ Additional arguments passed to RedeR application. }  
}


\details{       
Additional arguments: 
\describe{
  \item{mode}{ Character string, either 'in' or 'out'. If 'out', the subgraph will be nested apart from the main network. If 'in' the subgraph will be nested in the original network (default='out'). }
  \item{gscale}{ Expansion factor of the graph area in the app panel (default = 100) <numeric> -- p.s. 'NULL' will keep coordinates unchanged. }    
  \item{gcoord}{ Sets the subgraph x,y center. Coords between 0 and 100 are set to the visible area of the app panel (default = c(75,75)) <numeric vector>.}  
  \item{gatt}{A list of subgraph attributes. See attribute syntax in \code{\link[RedeR:addGraph]{addGraph}} } 
}
}

\value{
Shows subgraphs in RedeR app and returns the corresponding container IDs.
}


\author{Mauro Castro}

\seealso{
\code{\link[RedeR:nestNodes]{nestNodes}}
\code{\link[RedeR:addGraph]{addGraph}}
\code{\link[RedeR:addSubgraph]{addSubgraph}}
}


\examples{

rdp <- RedPort('MyPort')

\dontrun{

calld(rdp)

###

g1 <- graph.lattice(c(3,3,3))
addGraph( rdp, g1, layout.kamada.kawai(g1) )
nestSubgraph(rdp, nodes=c(1:5))

#..alternatively, a list of attributes can be set to the nest:
att<-list()
att$nestColor<-"#ff0000"
att$scale<-50
nestSubgraph(rdp, nodes=c(6:10), gatt=att)

}
}

\keyword{graph}
