\name{PluginBuilder}
\alias{PluginBuilder}

\title{
The constructor for the PluginBuilder class.
}

\description{
Constructor to build RedeR plugins.
}

\usage{
PluginBuilder(title='plugin', allMethods, allAddons=NULL)
}

\arguments{
  \item{title}{
    A character string representing the plugin name.
}
  \item{allMethods}{
    List of all plugin methods wrapped as R functions (does not accept arguments).
}  
  \item{allAddons}{
    List of all additional expressions wrapped as R functions (accept arguments).
}  
 
}

\details{
RedeR plug-ins have two main sections: methods and add-ons. The 'methods'section can be regarded as the plug-in trigger. When installed in the app, this trigger is usedto start a given analysis by unfolding the R expressions wrapped in the methods. Add-onsuse the same strategy, but remains hidden in the app -- and it is optional.
}

\value{
Build a new plugin.
}

\author{Mauro Castro}

\note{
The 'allMethods' section does not accept arguments. Formal functions can be passed to add-ons as additional arguments.
}

\seealso{
\code{\link[RedeR:submitPlugin]{submitPlugin}}
\code{\link[RedeR:updatePlugins]{updatePlugins}}
\code{\link[RedeR:deletePlugin]{deletePlugin}}
\code{\link[RedeR:pluginParser]{pluginParser}}
\code{\link[RedeR:dynwin]{dynwin}}
}


\examples{

#Wrap up a new method into a function
mt1 <- function() 
{
  rdp   <- RedPort('MyPort')
  g     <- getGraph(rdp)
  dg    <- degree.distribution(g)
  dynwin(rdp)                              #creates a RedeR java graphics device 
  plot(dg, xlab = "k", ylab = "P(k)")
}

#Initiate the plugin skeleton
plugin <- PluginBuilder(title="MyPlugin", allMethods=list(mt1=mt1))
                             
#Invoke RedeR and submit the new plugin

rdp <- RedPort('MyPort') 
                                         
\dontrun{
  calld(rdp)
  submitPlugin(rdp, plugin)
  updatePlugins(rdp)
}

  
}


\keyword{graph}

