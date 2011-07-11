\name{setNodeFontName}
\alias{setNodeFontName}
\alias{setNodeFontName,RedPort-method}

\title{Set font name.}

\description{
Method to set node attribute 'font name' in active RedeR sessions.
}

\usage{
setNodeFontName(obj, node, name)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{node}{Node name <string>}
  \item{name}{Font name (options are JRE or system dependent) <string>}
}

\value{
Sets node attribute <string>
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}},
\code{\link[RedeR:getNodeFontName]{getNodeFontName}}
}

\examples{
rdp <- RedPort('MyPort') 
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  calld(rdp)
  addEdges(rdp, edges)
	#Note: font names are system dependent!
  setNodeFontName(rdp, "n1", "Arial")
  updateGraph(rdp)
}  
}

\keyword{graph}
