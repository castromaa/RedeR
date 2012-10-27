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
Other arguments can be passed to the system in order to open the application or set additional environment variables. 
This maybe required when using the callback functions available in RedeR main panel (i.e. 'RCall'). 
If this is the case, try to pass the location of R home and the path to the dynamic library directory.
\describe{
  \item{filepath}{Path to 'reder.jar' file <string>} 
  \item{ADDPATH}{Add any additional path to RedeR shell <string>} 
  \item{checks}{Paths' security checks. Option: 'lock' (default) or 'unlock' <string>} 	
  \item{maxlag}{Max acceptable lag time for the R-Java callback confirmation (default=300 ms) <numeric>}  
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
