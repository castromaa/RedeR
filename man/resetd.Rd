\name{resetd}
\alias{resetd}
\alias{resetd,RedPort-method}

\title{Reset RedeR app. }

\description{
Reset the active RedeR session.  
}

\usage{
resetd(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Reset the software panel.
}

\author{Mauro Castro}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
rdp <- RedPort('MyPort') 

\dontrun{

  calld(rdp)
  resetd(rdp)
}  
}

\keyword{graph}
