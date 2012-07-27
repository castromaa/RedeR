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
  \item{hc}{Either an object of hclust of pvclust class.}
  \item{...}{ Additional arguments passed to RedeR application; if a "pvclust" object, it also passes arguments for "pvpick" function (e.g. to set the p-value threshold). } 
}

\details{       
Additional arguments: 
\describe{
  \item{cutlevel}{Numeric value indicating the point where the hclust object should be cut (default = 2). The distance is related to the option 'metric'. For "rootdist" and "leafdist", the cut level is related to the n steps required to get to the root's level or to the leaf's level, respectively (n>=1). For 'height', the cut is related to the corresponding dendrogram height <numeric>.}
  \item{metric}{Metric used to cut the hclust object at the top level (Options: "rootdist", "leafdist" or "height"; default="rootdist") <string>.}
  \item{nmemb}{Minimum number of members for a nest (>=2) <numeric>.}
  \item{nlev}{Maximum number of levels of a nested sequence (default=2) <numeric>.}
  \item{grid}{Number of rows and cols to lay out graphs in the panel (default = c(2,3)) <numeric>. }
  \item{gridScale}{Expansion factor of the grid area in the app panel. Options: 0.0 to 100 (default = 75) <numeric>. }
  \item{gscale}{Expansion factor to set the nest area related to the parents -- or related to the app panel. Provided as a vector with three numbers, c(n1,n2,n3):  n1 is related to nests at the first level of the hierarchy (i.e. nests rooted to the panel); n2 is related to nests from single branches, and n3 nests from double branches (default = c(30,75,45)) <numeric>.}
  \item{isAnchor}{Logical value; it sets whether to anchor containers in dynamic layouts. }
  \item{isAssign}{Logical value; it sets whether to assign container names to nested nodes. }
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5', 'tm6' (default: 'tm6') <string>. Alternatively, it can be a list with customized attributes.}
  \item{nlinewidth}{Line width of a nested series containers.}
  \item{nfontsz}{Label font size a nested series containers.}
  \item{plothc}{Logical value; whether to plot the corresponding hclust object (i.e. dendrogram). }
  \item{col}{A color vector; it is used to color labels in both containers and corresponding hclust object (i.e. dendrogram nodes). }
  \item{cex}{Numeric character expansion factor of dendrogram text and labels. }
  \item{xlab}{A label for the dendrogram x axis. }
  \item{ylab}{A label for the dendrogram y axis. }
}
}

\value{
Add/change graph objects and plot corresponding hclust object.
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

#g <- gtoy.rm()
#hc<-hclust(dist(get.adjacency(g)))
#plot(hc)

#rdp <- RedPort('MyPort') 

\dontrun{

  calld(rdp)
  addGraph(rdp,g)
  nesthc(rdp, hc)

}  
}

\keyword{graph}

