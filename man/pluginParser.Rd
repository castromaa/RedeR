\name{pluginParser}
\alias{pluginParser}

\title{RedeR plugin parser.}

\description{
Function to parse R expressions to RedeR plugins. 
}

\usage{
pluginParser(dyname, dyncode, args=FALSE)
}

\arguments{
  \item{dyname}{Name of the expression to display in RedeR application.}
  \item{dyncode}{R code (e.g. function) to be parsed.}  
  \item{args}{Option to parse formal arguments.} 
}

\value{
Returns parsed but unevaluated expressions. 
}

\note{
This function is used internally by the 'PluginBuilder' to parse plug-in methods and add-ons.
}

\author{Mauro Castro}

\seealso{
\code{\link[RedeR:PluginBuilder]{PluginBuilder}}
}

\examples{
x<-function(){...}
pluginParser('MyMethod', x, args=TRUE)
}

\keyword{graph}
