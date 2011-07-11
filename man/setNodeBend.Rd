\name{setNodeBend}
\alias{setNodeBend}
\alias{setNodeBend,RedPort-method}

\title{Set node bend.}

\description{
Method to set node attribute 'bend' in active RedeR sessions.
}

\usage{
setNodeBend(obj, node, bend)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Name <string>}
  \item{bend}{Options: 0-100\% <numeric>}
}

\details{
This is an arbitrary argument that produces different forms using the same basic node shape (e.g. to change the eccentricity of an ellipse)
}

\value{
Sets node attribute <numeric>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeBend]{getNodeBend}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeBend(rdp, "n1", 2.5)
  updateGraph(rdp)
}  
}

\keyword{graph}
