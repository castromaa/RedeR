\name{subg}
\alias{subg}

\title{Subgraph of a graph.}

\description{
Creates a subgraph containing only nodes specified from a data frame, including all edges among neighbors.
}

\usage{
subg(g, dat, refcol=1, maincomp=TRUE, connected=TRUE, transdat=TRUE)
}

\arguments{
  \item{g}{ An igraph object. }
  \item{dat}{ A data frame with node ids and attributes to be mapped to 'g'. }
  \item{refcol}{ The reference column  (node ids) in the 'dat' object. }
  \item{maincomp}{ Logical value, whether to return only the main component of the subgraph. }
  \item{connected}{ Logical value, whether to return only connected nodes. }
  \item{transdat}{ Logical value, whether to transfer node attributes from the 'dat' object to the subgraph. }
}

\value{
Returns a igraph object. 
}

\author{Mauro Castro}

\examples{
data(hs.inter)
data(ER.deg)
subnet <- subg(g=hs.inter, dat=ER.deg$dat, refcol=1)

}

\keyword{subgraph}
