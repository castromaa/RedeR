\name{nesthc}
\alias{nesthc}
\alias{nesthc,RedPort-method}

\title{Nest hclust objects to containers.}

\description{
Method to nest nodes in an active RedeR session.  
}

\usage{
nesthc(obj, hc, ...)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{hc}{Object of hclust Class.}
  \item{...}{ Additional arguments passed to RedeR application. } 
}

\details{       
Additional arguments: 
\describe{
  \item{cutlevel}{Cut level of the hclust object (default = 2) <numeric>. The distance is related to the option 'metric'. For "rootdist" and "leafdist", the cut level is related to the n steps required to get to the root's level or to the leaf's level, respectively (n>=1). For 'height', the cut is related to the corresponding dendrogram height (range: 0.0 to 1.0).}
  \item{metric}{Metric used to cut the hclust object at the top level (Options: "rootdist", "leafdist" or "height"; default="rootdist") <string>.}
  \item{ncomp}{Minimum number of components of a nest (default = 3) <numeric>.}
  \item{nlev}{Maximum number of levels of a nested sequence (default=2) <numeric>.}
  \item{grid}{Number of rows and cols to lay out graphs in the panel (default = c(2,3)) <numeric>. }
  \item{gridScale}{Expansion factor of the grid area in the app panel. Options: 0.0 to 100 (default = 75) <numeric>. }
  \item{gscale}{Expansion factor to set the nest area related to the parents -- or related to the app panel. Provided as a vector with three numbers, c(n1,n2,n3):  n1 is related to nests at the first level of the hierarchy (i.e. nests rooted to the panel); n2 is related to nests from single branches, and n3 nests from double branches (default = c(30,75,45)) <numeric>.}
  \item{gatt}{A list with graph attributes. See nest attribute syntax in \code{\link[RedeR:addGraph]{addGraph}} }
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5' (default: 'tm5'). }
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
\code{\link[RedeR:nestNodes]{nestNodes}}
\code{\link[RedeR:gtoy.rm]{gtoy.rm}}
}

\examples{

g <- gtoy.rm()
hc<-hclust(dist(get.adjacency(g)))
#plot(hc)

rdp <- RedPort('MyPort') 

\dontrun{

  calld(rdp)
  addGraph(rdp,g)
  nesthc(rdp, hc)

}  
}

\keyword{graph}

