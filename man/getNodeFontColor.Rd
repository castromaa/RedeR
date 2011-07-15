\name{getNodeFontColor}
\alias{getNodeFontColor}
\alias{getNodeFontColor,RedPort-method}

\title{
Get font color.
}

\description{
Method to get node attributes 'font color' from an active RedeR session. 
}

\usage{
getNodeFontColor(obj, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class.}
  \item{...}{ Additional arguments passed to RedeR application. }
}

\details{
Additional arguments:
\describe{
  \item{type}{Filter options. Valid arguments: <'node'>, <'container'> or <'all'>. Default='node'. } 
  \item{status}{Filter options. Valid arguments:  <'selected'>, <'nonselected'> or <'all'>. Default='all'} 
}
}

\value{
Returns node attributes <array of hexadecimal color codes>
}

\author{
Mauro Castro
}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:setNodeFontColor]{setNodeFontColor}}
\code{\link[RedeR:getGraph]{getGraph}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  updateGraph(rdp)
  getNodeFontColor(rdp)
}  
}

\keyword{graph}

