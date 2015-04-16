\name{gtoy.rm}
\alias{gtoy.rm}

\title{Random graphs and modules.}

\description{
A very simple function to generate random graphs with modular structures. 
}

\usage{
gtoy.rm(m=3, nmax=30, nmin=3, p1=0.5, p2=0.05, p3=0.9)
}

\arguments{
  \item{m}{ Number of modules. }
  \item{nmax}{ The maximum number of vertices in each module. }  
  \item{nmin}{ The minimum number of vertices in each module. }  
  \item{p1}{ Probability for adding new vertices to a module. }  
  \item{p2}{ Probability for drawing an edge between modules. }  
  \item{p3}{ Probability for drawing an edge within modules. }   
}

\value{
Returns a igraph object. 
}

\author{Mauro Castro}

\examples{

#g<-gtoy.rm()

}

\keyword{graph}
