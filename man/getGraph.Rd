\name{getGraph}
\alias{getGraph}
\alias{getGraph,RedPort-method}

\title{Get RedeR graph.}

\description{
Method to get and wrap up RedeR graphs into R objects. 
}

\usage{
getGraph(obj, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{
Additional arguments:
\describe{
  \item{status}{Filter options for RedeR graph status. Valid arguments:  <'selected'>, <'nonselected'> or <'all'> (default='all'). } 
  \item{type}{Filter options for RedeR graph objects. Valid arguments: <'node'>, <'container'> or <'all'> (default='node'). }
  \item{attribs}{Filter options for RedeR graph attributes. Valid arguments:  <'plain'>, <'minimal'> or <'all'> (default='plain'). } 
}
}

\value{
Returns igraph objects. 
}

\author{Mauro Castro}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{

rdp <- RedPort('MyPort')

\dontrun{

calld(rdp) 
#ps. first add a graph (e.g. see samples in RedeR or 'addGraph' method)!
g <- getGraph(rdp)
}
}

\keyword{graph}
