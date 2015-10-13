\name{RedPort}
\alias{RedPort}

\title{
The constructor for the RedPort class.
}

\description{
Constructor to build RedeR interface via XML-RPC (remote procedure call) server.
}

\usage{
RedPort(title = 'default', host = '127.0.0.1', port = 9091)
}

\arguments{
  \item{title}{
    A character string representing the XML-RPC port.
}
  \item{host}{
    The domain name of the machine that is running the RedeR XML-RPC server.
}
  \item{port}{
    An integer specifying the port on which the XML-RPC server should listen.
}
}
\value{
  An object of the RedPort Class.
}

\seealso{
\code{\link[RedeR:calld]{calld}}
}

\author{
  Mauro Castro
}

\examples{
  rdp <- RedPort('MyPort') 
}

\keyword{ graph }

