\name{RedeR-package}
\alias{RedeR-package}
\alias{RedeR}
\docType{package}
\title{
RedeR: bridging the gap between network analysis and visualization.
}
\description{
RedeR is an R-based package combined with a stand-alone Java application for dynamic network visualization and manipulation. It implements a callback engine by using a low-level R-to-Java interface to build and run common plugins. In this sense, RedeR takes advantage of R to run robust statistics, while the R-to-Java interface bridges the gap between network analysis and visualization: for R Developers, it allows the development of Java plug-ins exclusively using R codes; for Java Users, it runs R methods implemented in a stand-alone application, and for R Users RedeR interactively displays R graphs using a robust Java graphic engine embedded in this package.
}

\details{
\tabular{ll}{
Package: \tab RedeR \cr
Type: \tab Package\cr
License: \tab GPL \cr
LazyLoad: \tab yes\cr
}
}

\seealso{
\code{\link[RedeR:RedPort-class]{RedPort-class}}
}

\author{
Mauro Castro <mauro.a.castro@gmail.com>
}
\references{
Castro, M. et al. \emph{RedeR: R/Bioconductor package for representing modular structures, nested networks and multiple levels of hierarchical associations}. Genome Biology 13(4):R29, 2012.
}
\keyword{ package }
