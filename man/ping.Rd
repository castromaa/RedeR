\name{ping}
\alias{ping}
\alias{ping,RedPort-method}

\title{Test RedeR R-to-Java interface. }

\description{
Test R interface and the connection to an active RedeR session.  
}

\usage{
ping(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
  "R interface is ready to use!"
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
ping (rdp)
}  
 
}

\keyword{graph}
