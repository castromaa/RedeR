\name{att}
\alias{att.setv}
\alias{att.sete}
\alias{att.mapv}
\alias{att.mape}

\title{Map and set edge and vertex attributes to RedeR application.}

\description{
These functions map data frames containing edge/vertex attributes to an igraph object and set attributes to RedeR.
}

\usage{
att.setv(g, from, to='nodeColor', pal=1, cols=NULL, na.col=grey(0.7), xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, isrev=FALSE, getleg=TRUE, roundleg=1, title=NULL)
att.sete(g, from, to='edgeColor', pal=1, cols=NULL, na.col=grey(0.7), xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, isrev=FALSE, getleg=TRUE, roundleg=1, title=NULL)
att.mapv(g, dat, refcol=1)
att.mape(g, dat, refcol=c(1,2))
}

\arguments{
  \item{g}{An igraph object. }
  \item{from}{An attribute name available in 'g' <string>. }
  \item{to}{A valid RedeR attribute name (see \code{\link[RedeR:addGraph]{addGraph}} or type 'att.setv()' or 'att.sete()'). }
  \item{breaks}{A numeric vector of two or more breakpoints to be applied to the attribute values. }
  \item{pal}{Default color palette. Options:  1 or 2. }
  \item{xlim}{A numeric vector with three boundaries: c(<lower boundary>, <upper boundary>, <NA>). It corresponds to boundary values to be apply to numeric attributes (e.g. nodeSize). Default: c(20,100,1).  }
  \item{cols}{Vector of colors (either hexadecimals or valid R color names). }
  \item{na.col}{A color representing eventual NAs. Default: grey(0.7) }
  \item{shapes}{A string vector with valid RedeR shapes (see \code{\link[RedeR:addGraph]{addGraph}} or type 'att.setv()' or 'att.sete()').  }
  \item{categvec}{Optional: levels to encode attributes as a factor <vector>. }
  \item{nquant}{Optional: number of breakpoints to split attribute values by quantiles <integer>. }  
  \item{isrev}{Optional: reversed version of attribute values <logical>. } 
  \item{getleg}{Optional: return legend values <logical>. } 
  \item{dat}{A data frame with the attributes to be mapped to 'g'. } 
  \item{refcol}{The reference columns in the 'data' object with either node ids (one column <integer>) or edge ids (two columns <vector of two integers>). } 
  \item{roundleg}{Integer indicating the number of decimal places (round) in the legend of numerical attributes}.
  \item{title}{Optional: legend title}.  
}


\value{
Map/set RedeR attributes to igraph objects.
}


\author{Mauro Castro}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
}


\examples{
data(ER.deg)

sg <- ER.deg$ceg  # an igraph object
dt <- ER.deg$dat  # a data frame object

# maps the data frame to the igraph object
sg <- att.mapv(g=sg, dat=dt, refcol=1)

# Sets graph attributes to RedeR!

# sets gene symbol do nodeAlias attribute
sg <- att.setv(sg, from="Symbol", to="nodeAlias")

# sets numerical value to nodeColor attribute
sg <- att.setv(sg, from="logFC.t3", to="nodeColor", breaks=seq(-1,1,0.2), pal=2)

# sets numerical value to nodeSize attribute
sg <- att.setv(sg, from="ERbdist", to="nodeSize", nquant=10, isrev=TRUE, xlim=c(5,40,1))

}

\keyword{attributes}
