\name{calld}
\alias{calld}
\alias{calld,RedPort-method}

\title{Call RedeR app from R.}

\description{ 
Method to invoke RedeR application from R.
}

\usage{
calld(obj, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. }
}

\details{
Other arguments can be passed to the system in order to open the application.
\describe{
  \item{filepath}{Path to 'reder.jar' file <string>} 
  \item{maxlag}{Max acceptable lag time for the R-Java callback confirmation (default=20 s) <numeric>}  
  \item{checkcalls}{Reports eventual errors from the R-Java callback (default=FALSE) <logical>}
}
}

\value{
Systems call to open RedeR application and XML-RPC server.
}

\author{Mauro Castro}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
\code{\link[RedeR:addGraph]{addGraph}}
}

\examples{

rdp <- RedPort('MyPort')

\dontrun{

    calld(rdp)
}
}

\keyword{graph}
