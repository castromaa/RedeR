\name{addLegend}
\alias{addLegend.color}
\alias{addLegend.size}
\alias{addLegend.shape}
\alias{addLegend.color,RedPort-method}
\alias{addLegend.size,RedPort-method}
\alias{addLegend.shape,RedPort-method}

\title{Add graph legends to RedeR application.}

\description{
Methods to send legends to RedeR app.
}

\usage{
addLegend.color(obj, colvec,  ...)
addLegend.size(obj, sizevec, ...)
addLegend.shape(obj, shapevec, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{colvec}{ Vector with legend colors, either hexadecimal or valid R color names. }
  \item{sizevec}{ Vector with legend node size, options: > 0 <numeric>. }
  \item{shapevec}{ Vector with valid shape names: 'ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND'. }
  \item{...}{ Additional arguments passed to RedeR application. }    
}


\details{       
Additional arguments: 
\describe{
  \item{type}{ Legend type. Options: "node" or "edge" (default: "node") <character>. }
  \item{labvec}{ Vector with legend labels <character>. }
  \item{position}{ Position of the legend in RedeR panel. Options: 'topleft','topright','bottomleft','bottomright' (default: addLegend.color "topright", addLegend.size "bottomleft", and addLegend.shape "bottomright" )<character>.}
  \item{dxborder}{ Distance (in pixel) from panel border (default: 5 ) <numeric>. }
  \item{dyborder}{ Distance (in pixel) from panel border (default: 5 ) <numeric>. }
  \item{vertical}{ Logical value, set vertical/horizontal position of the legend in the app panel (default: TRUE for addLegend.color and addLegend.size and FALSE for addLegend.shape. }
  \item{ftsize}{ Font size (in pixel) (default: 8) <numeric>. }
  \item{title}{ Legend title <string>. }
  \item{dxtitle}{ Distance (in pixel) from legend labels to the main axis (default: 30) <numeric>. }
  \item{size}{ Legend size; only for addLegend.color and addLegend.shape methods (default: 30) <numeric>. }
  \item{bend}{ Legend width/height ratio; only for addLegend.color method (default: 0.85) <numeric>. }
  \item{col}{ Legend color; only for addLegend.size and addLegend.shape methods (default: "#000000") <either hexadecimal or valid color name>.}
  \item{intersp}{ Legend inter space (only for addLegend.size and addLegend.shape methods) (default: 0) <numeric>. }
}
}

\value{
Send legend objects to RedeR app.
}


\author{Mauro Castro}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
}


\examples{

rdp <- RedPort('MyPort')

\dontrun{

calld(rdp)

cols<-colorRampPalette(colors=c('red','blue'))(14)	
addLegend.color(rdp,cols)
addLegend.color(rdp,cols,type="edge")

size<-c(10,20,30,40,50)
addLegend.size(rdp,size)

size<-c(1:10)
addLegend.size(rdp,size,type="edge")

shape<-c('ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND')
addLegend.shape(rdp,shape)

shape<-c('SOLID','DOTTED','DOTTED_SHORT','LONG_DASH')
addLegend.shape(rdp,shape,type="edge")


}
}

\keyword{legend}
