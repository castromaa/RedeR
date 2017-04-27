\name{addSeries}
\alias{addSeries}
\alias{addSeries,RedPort-method}

\title{Add series to RedeR application.}

\description{
Method to send series of graphs to RedeR app.
}

\usage{
addSeries(obj, g, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{g}{ An igraph object. }  
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{       
Additional arguments: 
\describe{
  \item{setnodes}{Logical value, whether to update node attributes in the new item of the series (default = TRUE). } 
  \item{setedges}{Logical value, whether to add edges and update attributes in the new item of the series (default = TRUE). }
}
}

\value{
Submits series of R graphs to RedeR app. 
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
V(g1)$nodeColor<-heat.colors(vcount(g1))
addSeries( rdp, g1)
}
}

\keyword{graph}
