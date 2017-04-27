\name{relax}
\alias{relax}
\alias{relax,RedPort-method}

\title{relax}

\description{
This function starts the dynamic layout and sets the force-directed options available in RedeR app.
}

\usage{
relax(obj,p1=100,p2=100,p3=100,p4=100,p5=100,p6=100,p7=10,p8=10,ps=FALSE)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{p1}{Edge target length (in pixels; >= 1 ) <numeric>. }
  \item{p2}{Edge stiffness (arbitrary unities; >= 1 ) <numeric>. }
  \item{p3}{Node repel factor (arbitrary unities; >= 1 ) <numeric>. }
  \item{p4}{Node perimeter effect (in pixels; >= 1 ) <numeric>. }
  \item{p5}{Node speed limit (arbitrary unities; >= 1 ) <numeric>. }
  \item{p6}{Nest-nest edge target length, i.e., edge target between linked containers (in pixels; >= 1 ) <numeric>. }
  \item{p7}{Nest-node repel factor, i.e., repulsion among containers and out-nodes (arbitrary unities; >= 1 ) <numeric>.  }
  \item{p8}{Repulsion radius, i.e., this parameter limits the repel factor range (given in p1 unites; >= 1 ) <numeric>.  }
  \item{ps}{Panel settings: logical value, whether to start interactive panel. }
}

\details{
One of the most versatile features of RedeR is the ability to deal with nested network objects using dynamic simulation, which makes it possible to represent, for example, subnetworks and time-series onto the same graph in a user-friendly routine. The simulation uses force-directed algorithms as described elsewhere (Brandes 2001; Fruchterman and Reingold 1991). Here we adapted the method to deal with nested networks. In force-directed graphs, each edge can be regarded as a spring - with a given target length - and can either exert a repulsive or attractive force on the connected nodes, while nodes are analogous to mutually repulsive charged particles that move according to the applied forces. In RedeR, the simulation is additionally constrained by the hierarchical structure. For example, a nested node is constrained to its parent-node by opposing forces applied by the nest, which is regarded as a special node whose nested objects can reach a local equilibrium independently from other network levels. The simulation is adjusted by global options and evolves iteratively (and interactively) until the system reaches the equilibrium state. The parameters controlling the dynamics are arbitrarily set to layout sparse networks with a few nodes (e.g. 10-100 nodes). For large and dense networks better results can be achieved interactively by tuning one or more parameters.
}

\references{

Brandes U. Drawing graphs: methods and models. In: Lecture notes in computer science. Kaufmann M. and Wagner D. (Ed), vol. 2025. Heidelberg: Springer; 2001: 71-86.

Fruchterman TMJ, Reingold EM. Graph drawing by force-directed placement. Software: Practice and Experience 1991, 21(11):1129-1164.

}

\author{Mauro Castro}

\examples{
# Initialize igraph
library(igraph)

g <- graph.lattice(c(5,5,5))

\dontrun{

  rdp <- RedPort()
  calld(rdp)
  addGraph( rdp, g, layout.random(g) )
  relax(rdp)
} 


}

\keyword{layout}
