\name{deletePlugin}
\alias{deletePlugin}
\alias{deletePlugin,RedPort-method}

\title{
Remove plugins from RedeR application.
}
\description{
Method to remove plugins from the RedeR application, including the main menu.
}
\usage{
deletePlugin(obj, pluginName)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{pluginName}{Plugin name that is available at RedeR application <string>}
}

\details{
Need description!
}

\value{
Delete a plugin from RedeR application.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:PluginBuilder]{PluginBuilder}}
\code{\link[RedeR:submitPlugin]{submitPlugin}}
\code{\link[RedeR:deletePlugin]{updatePlugins}}
\code{\link[RedeR:pluginParser]{pluginParser}}
}

\examples{

#Prior calling this method build "MyPlugin" using the PluginBuilder!

rdp <- RedPort('MyPort') 
  
\dontrun{

  calld(rdp) 
  deletePlugin(rdp, "MyPlugin")
  updatePlugins(rdp)
  #Remove the plugin "MyPlugin" from RedeR
}  
}
