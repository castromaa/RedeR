\name{dynwin}
\alias{dynwin}
\alias{dynwin,RedPort-method}

\title{Wrap R graphics.}

\description{
Method to wrap R graphics in RedeR Java classes (deprecated). 
}

\usage{
dynwin(obj, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{
Other arguments:
\describe{
  \item{width}{Initial width of the window <integer>} 
  \item{height}{Initial height of the window <integer>}
  \item{ps}{Initial point size <integer>}  
}
}

\value{
Returns R graphics as canvas. 
}

\author{Mauro Castro}

\seealso{
\code{\link[RedeR:PluginBuilder]{PluginBuilder}}
\code{\link[RedeR:RedPort]{RedPort}}
}

\note{
This method should be called only from RedeR plugins!
}

\examples{
#.Deprecated
}

\keyword{graph}
