\name{version}
\alias{version}
\alias{version,RedPort-method}

\title{Version}

\description{
Check RedeR application version.  
}

\usage{
version(obj)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
}

\value{
  Returns the version of the current RedeR application that is listening a specified XML-RPC port.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
rdp <- RedPort('MyPort')

\dontrun{

  calld(rdp) 
  version(rdp)
}
}

\keyword{graph}
