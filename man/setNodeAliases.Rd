\name{setNodeAlias}
\alias{setNodeAlias}
\alias{setNodeAlias,RedPort-method}

\title{
Set node alias.
}

\description{
Method to set node attributes 'alias' in active RedeR sessions.
}

\usage{
setNodeAlias(obj, node, alias)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Node name <string>}
  \item{alias}{Node alias <string>}
}

\value{
Sets node attribute <string>
}

\author{
Mauro Castro
}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
\code{\link[RedeR:getNodeAliases]{getNodeAliases}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
  setNodeAlias(rdp, "n1", "Node1")
  updateGraph(rdp)
}  
}

\keyword{graph}

