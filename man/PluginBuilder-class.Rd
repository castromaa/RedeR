\name{PluginBuilder-class}
\docType{class}
\alias{PluginBuilder-class}

\title{Class 'PluginBuilder' }

\description{
A class providing methods to build plugins to RedeR application.
}

\section{Slots}{
  \describe{
    \item{\code{title}:}{A character string representing the plugin name.}
    \item{\code{allMethods}:}{List of all plugin methods wrapped as R functions.}
    \item{\code{allAddons}:}{List of all additional R expression wrapped as R functions.} 
  }
}


\author{Mauro Castro}

\examples{

    #Initiates a new method, say mt1
    mt1<-function(){"#Your code here!"}
    #Initiates a plugin skeleton
    plugin <- PluginBuilder ( title = "MyPlugin", allMethods = list(mt1=mt1) )
    
}
\keyword{graphs}
\keyword{classes}

