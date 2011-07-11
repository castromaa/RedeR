\name{setNodeShape}
\alias{setNodeShape}
\alias{setNodeShape,RedPort-method}

\title{Set node shape.}

\description{
Method to set node attribute 'shape' in active RedeR sessions.
}

\usage{
setNodeShape(obj, node, shape)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Name <string>}
  \item{shape}{Options: 'ELLIPSE', 'RECTANGLE','ROUNDED_RECTANGLE','TRIANGLE','DIAMOND' <string>}
}

\value{
Sets node attribute <string>
}

\details{
See 'setNodeBend' to produce different shapes using the same basic node form (e.g. to change the eccentricity of an ellipse).
}
\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeShape]{getNodeShape}}
\code{\link[RedeR:setNodeBend]{setNodeBend}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeShape(rdp, "n1", "RECTANGLE")
  updateGraph(rdp)
}  
}

\keyword{graph}
