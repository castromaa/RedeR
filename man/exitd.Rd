\name{exitd}
\alias{exitd}
\alias{exitd,RedPort-method}

\title{Exit RedeR R-to-Java interface. }

\description{
Exit R interface and close the active RedeR session.  
}

\usage{
exitd(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Exit software.
}

\author{Mauro Castro}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{

rdp <- RedPort()

\dontrun{

  calld(rdp)
  exitd(rdp)

}  
}

\keyword{graph}
