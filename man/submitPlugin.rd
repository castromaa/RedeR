\name{submitPlugin}
\alias{submitPlugin}
\alias{submitPlugin,RedPort-method}

\title{
Submit plugins to RedeR application.
}

\description{
Method to send R-code to RedeR app in order to upload a new plugin (deprecated).
}

\usage{
submitPlugin(obj, plugin)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{plugin}{Object of PluginBuilder Class. } 
}

\value{
Send a new plugin to RedeR application.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:PluginBuilder]{PluginBuilder}}
\code{\link[RedeR:updatePlugins]{updatePlugins}}
\code{\link[RedeR:deletePlugin]{deletePlugin}}
\code{\link[RedeR:pluginParser]{pluginParser}}
}


\examples{
                           
#.Deprecated

}


\keyword{graph}

