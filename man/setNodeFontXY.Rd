\name{setNodeFontXY}
\alias{setNodeFontXY}
\alias{setNodeFontXY,RedPort-method}

\title{Set font x,y coordinates.}

\description{
Method to set node attribute 'font XY position' in active RedeR sessions.
}

\usage{
setNodeFontXY(obj, node, x, y)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Node name<string>}
  \item{x}{relative x coord. (related to x node position) <numeric>}
  \item{y}{relative y coord. (related to y node position)<numeric>}
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
\code{\link[RedeR:getNodeFontX]{getNodeFontX}, \link[RedeR:getNodeFontY]{getNodeFontY} }
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeFontXY(rdp, "n1", 50.0, 50.0)
  updateGraph(rdp)
}  
}

\keyword{graph}

