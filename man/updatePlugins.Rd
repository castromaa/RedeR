\name{updatePlugins}
\alias{updatePlugins}
\alias{updatePlugins,RedPort-method}

\title{
Update RedeR plugins.
}

\description{
Method to call RedeR application in order to update plugins sent by R command.
}

\usage{
updatePlugins(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Updates new plugins.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:PluginBuilder]{PluginBuilder}}
\code{\link[RedeR:submitPlugin]{submitPlugin}}
\code{\link[RedeR:deletePlugin]{deletePlugin}}
\code{\link[RedeR:pluginParser]{pluginParser}}
}

\examples{
rdp <- RedPort('MyPort') 
                           
\dontrun{
  calld(rdp)  
  updatePlugins(rdp)
}  
}

\keyword{graph}
