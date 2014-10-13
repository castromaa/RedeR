\name{addGraph}
\alias{addGraph}
\alias{addGraph,RedPort-method}

\title{Add graphs to RedeR application.}

\description{
Method to wrap R graphs into RedeR objects and send it to RedeR app.
}

\usage{
addGraph(obj, g, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{g}{ An igraph object. }  
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{       
Additional arguments: 
\describe{
  \item{layout}{Vertex coordinates (graph layout). Accepts matrix with 2 cols (i.e. x and y coords) <matrix>. } 
  \item{gscale}{Expansion factor of the graph area related to the app panel area (default = 75) <numeric>. } 
  \item{zoom}{ Sets the zoom scale for the app panel (range: 0.0 to 100.0; default = 100.0) <numerics>. } 
  \item{gcoord}{Sets the graph x,y center. Coords between 0 and 100 are set to the visible area of the app panel (default = c(50,50)) <numeric vector>. } 
  \item{isNest}{Logical value, whether to nest all nodes into a new container (default = FALSE). See additional args in \code{\link[RedeR:nestNodes]{nestNodes}}} 
  \item{isAnchor}{If isNest=TRUE, this logical value sets whether to anchor the container in dynamic layouts (default = TRUE). }
  \item{isAssign}{If isNest=TRUE, this logical value sets whether to assign the container name to the nested nodes (default = FALSE). }
  \item{loadEdges}{Logical value, whether to send edges to RedeR app (default = TRUE).} 
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5','tm6' <string>. Alternatively, it can be a list with customized attributes. }
  \item{ntransform}{Logical value, whether to transform nodes in containers (default = FALSE).}
  \item{parent}{ID of a container already available in the app <string>. Nodes from g will be nested to this container. } 
}
}

\value{
Submits R graphs to RedeR app. 
}


\section{Attributes passed by the igraph object}{

\subsection{Graph attributes}{
  \describe{	
    \item{bgColor}{ Sets the background color of the app panel <hexadecimal>. } 
    \item{zoom}{ Sets the zoom scale for the app panel (range: 0.0 to 100.0) (Default=100) <numerics>. } 
    \item{gscale}{ Expansion factor of the graph area related to the app panel (range: 0.0 to 100.0) (Default=100) <numerics> (PS. alternative entry to the 'gscale' argument above). }
    \item{coordX}{ Sets the graph x center; x between 0 and 100 sets to visible area <numeric> (PS. alternative entry to the 'gcoord' argument above). }
    \item{coordY}{ Sets the graph y center; y between 0 and 100 sets to visible area <numeric> (PS. alternative entry to the 'gcoord' argument above). }
    \item{loadEdges}{ Logical value, whether to send edges to RedeR app (Default=TRUE) (PS. alternative entry to the 'loadEdges' argument above).  }
    \item{isNest}{ Logical value, whether to nest all nodes into a new container (Default=FALSE) (PS. alternative entry to the 'nest' argument above). }
    \item{isAnchor}{If isNest=TRUE, this logical value sets whether to anchor the container in dynamic layouts (Default=FALSE). }
    \item{isAssign}{If isNest=TRUE, this logical value sets whether to assign the container name to the nested nodes (Default=FALSE). }
    \item{nestColor}{ If isNest=TRUE, this attribute sets the 'color' of the new container <hexadecimal>. }
    \item{nestAlias}{ If isNest=TRUE, this attribute sets the label of the new container <string>.  }
    \item{nestFontSize}{ If isNest=TRUE, this attribute sets the size of the container label (Default=12). <numerics>.}
    \item{nestFontColor}{ If isNest=TRUE, this attribute sets the 'color' of the container label <hexadecimal>. }
    \item{nestFontX}{ If isNest=TRUE, this attribute sets the x position of the label related to the container (Default=-8) <numerics>.}
    \item{nestFontY}{ If isNest=TRUE, this attribute sets the y position of the label related to the container (Default=-8) <numerics>.}
    \item{nestShape}{ If isNest=TRUE, this attribute sets the shape of the container, options: <'ELLIPSE'> and <'ROUNDED_RECTANGLE'> (Default= ELLIPSE).}
    \item{nestSize}{ If isNest=TRUE, this attribute sets the size of the container (Default=NULL) <numerics>.}
    \item{nestLineWidth}{ If isNest=TRUE, this attribute sets the line width of the container, options: >= 0 (Default=1.0) <numerics>.}
    \item{nestLineColor}{ If isNest=TRUE, this attribute sets the line color of the container <hexadecimal>.}
    \item{nestImage}{If isNest=TRUE, sets the status of the container on the screen: <'plain'>, <'transparent'>, or <'hide'> (Default= plain). } 
    \item{nestLineType}{ If isNest=TRUE, this attribute sets the line type of the container: <'SOLID'>, <'DOTTED'>, <'DOTTED_SHORT'>, <'LONG_DASH'> (Default='SOLID').}
  }
}

\subsection{Vertex attributes}{
  \describe{
    \item{name}{ Node attribute 'name' <string>. }
    \item{nodeAlias}{ Node attribute 'alias' <string>. }
    \item{nodeBend}{ Node attribute 'bend', options: 0-100\% (Default=50) <numeric>. }
    \item{coordX}{ Node attribute 'x coord' (Default=random coord) <numeric>. }
    \item{coordY}{ Node attribute 'y coord'(Default=random coord) <numeric>. }
    \item{nodeSize}{ Node attribute 'size', options: > 0 (Default=20) <numeric>. }
    \item{nodeShape}{ Node attribute 'shape', options: 'ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND' (Default= ELLIPSE) <string>. }
    \item{nodeColor}{ Node attribute 'color', e.g. "#ff0000" for red <hexadecimal>. }
    \item{nodeWeight}{ Node attribute 'weight', options: >= 0 (Default=0) <numeric>. }
    \item{nodeLineWidth}{ Node attribute 'line width', options: >= 0 (Default=1) <numeric>. }
    \item{nodeLineColor}{ Node attribute 'line color', e.g. "#ff0000" for red <hexadecimal>. }
    \item{nodeFontSize}{ Node attribute 'font size', options: >= 0 (Default=12) <integer>. }
    \item{nodeFontColor}{ Node attribute 'font color', e.g. "#ff0000" for red <hexadecimal>. }	
  }
}

\subsection{Edge attributes}{
  \describe{
    \item{arrowDirection}{
    Edge attribute 'arrow direction', used to set mixed associations in undirected graphs.
    Options: 0 (A-B), 1 (A-> B), -1 (A-| B), 2 (A <-B), -2 (A |-B), 3 (A <-> B), -3 (A |-| B), 
    4 (A |-> B) and  -4 (A <-| B) (Default=0) <integer>.}
    \item{arrowType}{ Edge attribute 'arrow type', used to set the association mode in directed graphs. 
    Options: -1, 0 and 1 (Default=1) <integer>. }
    \item{edgeWeight}{ Edge attribute 'weight', options: >= 0 (Default=0.0) <numeric>. } 
    \item{edgeWidth}{ Edge attribute 'width', options: >=0 (Default=1.0) <numeric>. } 	
    \item{edgeColor}{ Edge attribute 'color', e.g. "#ff0000" for red <hexadecimal>. } 
    \item{edgeType}{ Edge attribute 'color', options: 'SOLID', 'DOTTED, 'DOTTED_SHORT', 'LONG_DASH' (Default='SOLID'). } 
    \item{arrowLength}{ Edge arrow attribute 'length', options: > 0 (Default=10) <numeric>. } 
    \item{arrowAngle}{ Edge arrow attribute 'angle', options: 0-90 (Default=45) <numeric>. } 
    \item{linkType}{ Set assignment type either between nodes and containers or containers and containers.  Options: 'nested' and 'notnested' (Default='nested') <string>. } 
  }
}

}

\note{
In 'igraph' package, vertex and edge attributes can be assigned as arbitrary R objects. In order 
to pass these extensible features to RedeR the attributes must be provided in a valid syntax (see above). 
Only UNIQUE edges are accepted. If present, mutual/multiple edges will be collapsed to unique edges. 
In this cases, source-target information is transferred to 'arrowDirection' attribute; other attributes will be related to the first edge from the edge list.
}

\author{Mauro Castro}

\seealso{
\code{\link[RedeR:getGraph]{getGraph}}
\code{\link[RedeR:addLegend]{addLegend}}
\code{\link[RedeR:nesthc]{nesthc}}
\code{\link[RedeR:nestNodes]{nestNodes}}
\code{\link[RedeR:mergeOutEdges]{mergeOutEdges}}
\code{\link[RedeR:relax]{relax}}
\code{\link[RedeR:selectNodes]{selectNodes}}
\code{\link[RedeR:att]{att}}
}




\examples{

rdp <- RedPort('MyPort')

\dontrun{

calld(rdp)

###

g1 <- graph.empty(n=10, directed=FALSE)
addGraph( rdp, g1, layout.random(g1) )

resetd(rdp)

###

g2 <- graph.lattice(c(5,5,5))
addGraph( rdp, g2, layout.kamada.kawai(g2) )

resetd(rdp)

###

g <- barabasi.game(10)
V(g)$name<-letters[1:10]
V(g)$nodeSize<-c(100,rep(30,9))
addGraph( rdp,  g, ntransform=TRUE )

sg <- barabasi.game(3)
addGraph( rdp, sg, parent="a" )

resetd(rdp)

###...to check loading an interactome!

data(hs.inter)
system.time( addGraph(rdp, hs.inter, layout=NULL) )


}
}

\keyword{graph}
