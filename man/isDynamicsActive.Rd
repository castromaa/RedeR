\name{isDynamicsActive}
\alias{isDynamicsActive}
\alias{isDynamicsActive,RedPort-method}

\title{Inquires about RedeR current state. }

\description{
Inquires whether 'dynamics' algorithm is active in RedeR application.
}

\usage{
isDynamicsActive(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Returns 1<integer> if true, 0<integer> otherwise.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
rdp <- RedPort() 

\dontrun{

  calld(rdp)
  isDynamicsActive(rdp)
    # 1 or 0
}  
}

\keyword{graph}

