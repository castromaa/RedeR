\name{setNodeFontStyle}
\alias{setNodeFontStyle}
\alias{setNodeFontStyle,RedPort-method}

\title{Set font style.}

\description{
Method to set node attribute 'font style' in active RedeR sessions.
}


\usage{
setNodeFontStyle(obj, node, style)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Node name <string>}
  \item{style}{Options: 0 (PLAIN), 1 (BOLD), 2 (ITALIC), or 3 (BOLD+ITALIC) <integer>}
}

\value{
Sets node attribute <integer>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeFontStyle]{getNodeFontStyle}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeFontStyle(rdp, "n1", 1)
  updateGraph(rdp)
}  
}

\keyword{graph}
