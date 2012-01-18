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
  \item{isAnchor}{If isNest=TRUE, this logical value sets whether to anchor the container in dynamic layouts. }
  \item{isAssign}{If isNest=TRUE, this logical value sets whether to assign the container name to the nested nodes. }
  \item{loadEdges}{Logical value, whether to send edges to RedeR app (default = TRUE).} 
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5','tm6'. } 
}
}

\value{
Submits R graphs to RedeR app. 
}


\section{Attributes passed by the igraph object}{

\subsection{Graph attributes}{
  \describe{	
    \item{bgColor}{ Sets the background color of the app panel <hexadecimal>. } 
    \item{zoom}{ Sets the zoom scale for the app panel (range: 0.0 to 100.0) <numerics>. } 
    \item{gscale}{ Expansion factor of the graph area in the app panel <numerics> (PS. alternative entry to the 'gscale' argument above). }
    \item{coordX}{ Sets the graph x center; x between 0 and 100 sets to visible area <numeric> (PS. alternative entry to the 'gcoord' argument above). }
    \item{coordY}{ Sets the graph y center; y between 0 and 100 sets to visible area <numeric> (PS. alternative entry to the 'gcoord' argument above). }
    \item{loadEdges}{ Logical value, whether to send edges to RedeR app (PS. alternative entry to the 'loadEdges' argument above).  }
    \item{isNest}{ Logical value, whether to nest all nodes into a new container (PS. alternative entry to the 'nest' argument above). }
    \item{isAnchor}{If isNest=TRUE, this logical value sets whether to anchor the container in dynamic layouts. }
    \item{isAssign}{If isNest=TRUE, this logical value sets whether to assign the container name to the nested nodes. }
    \item{nestColor}{ If isNest=TRUE, this attribute sets the 'color' of the new container <hexadecimal>. }
    \item{nestAlias}{ If isNest=TRUE, this attribute sets the label of the new container <string>.  }
    \item{nestFontSize}{ If isNest=TRUE, this attribute sets the size of the container label <numerics>.}
    \item{nestFontColor}{ If isNest=TRUE, this attribute sets the 'color' of the container label <hexadecimal>. }
    \item{nestFontX}{ If isNest=TRUE, this attribute sets the x position of the label related to the container <numerics>.}
    \item{nestFontY}{ If isNest=TRUE, this attribute sets the y position of the label related to the container <numerics>.}
    \item{nestShape}{ If isNest=TRUE, this attribute sets the shape of the container, options: <'ELLIPSE'> and <'ROUNDED_RECTANGLE'>.}
    \item{nestSize}{ If isNest=TRUE, this attribute sets the size of the container <numerics>.}
    \item{nestLineWidth}{ If isNest=TRUE, this attribute sets the line width of the container, options: >= 0 <numerics>.}
    \item{nestLineColor}{ If isNest=TRUE, this attribute sets the line color of the container <hexadecimal>.}
    \item{nestImage}{If isNest=TRUE, sets the status of the container on the screen: <'plain'>, <'transparent'>, or <'hide'>. } 
    \item{nestLineType}{ If isNest=TRUE, this attribute sets the line type of the container: <'SOLID'>, <'DOTTED'>, <'DOTTED_SHORT'>, <'LONG_DASH'>}
  }
}

\subsection{Vertex attributes}{
  \describe{
    \item{name}{ Node attribute 'name' <string>} 
    \item{nodeAlias}{ Node attribute 'alias' <string>} 
    \item{nodeBend}{ Node attribute 'bend', options: 0-100\% <numeric>}	
    \item{coordX}{ Node attribute 'x coord' <numeric>} 
    \item{coordY}{ Node attribute 'y coord' <numeric>}
    \item{nodeSize}{ Node attribute 'size', options: > 0 <numeric>}
    \item{nodeShape}{ Node attribute 'shape', options: 'ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND' <string>}
    \item{nodeColor}{ Node attribute 'color', e.g. "#ff0000" for red <hexadecimal>}
    \item{nodeWeight}{ Node attribute 'weight', options: <numeric>}
    \item{nodeLineWidth}{ Node attribute 'line width', options: >= 0 <numeric>}
    \item{nodeLineColor}{ Node attribute 'line color', e.g. "#ff0000" for red <hexadecimal>}
    \item{nodeFontSize}{ Node attribute 'font size', options: >= 0 <integer>}
    \item{nodeFontColor}{ Node attribute 'font color', e.g. "#ff0000" for red <hexadecimal>}	
  }
}

\subsection{Edge attributes}{
  \describe{
    \item{arrowDirection}{ Edge attribute 'arrow direction', Options: 0 (A-B), 1 (A->B), 2 (A<-B) or 3 (A<->B) <integer>} 
    \item{edgeWeight}{ Edge attribute 'weight', options: >= 0 <numeric>} 
    \item{edgeWidth}{ Edge attribute 'width', options: <numeric>} 	
    \item{edgeColor}{ Edge attribute 'color', e.g. "#ff0000" for red <hexadecimal>} 
    \item{edgeType}{ Edge attribute 'color', options: 'SOLID, 'DOTTED, 'DOTTED_SHORT', 'LONG_DASH'} 
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
\code{\link[RedeR:addSubgraph]{addSubgraph}}
\code{\link[RedeR:addSubgraph.list]{addSubgraph.list}}
\code{\link[RedeR:setArrowDirection]{setArrowDirection}}
\code{\link[RedeR:setEdgeWeight]{setEdgeWeight}}
\code{\link[RedeR:setEdgeWidth]{setEdgeWidth}}
\code{\link[RedeR:setEdgeColor]{setEdgeColor}}
\code{\link[RedeR:setEdgeType]{setEdgeType}}
\code{\link[RedeR:setNodeBend]{setNodeBend}}
\code{\link[RedeR:setNodeAliases]{setNodeAliases}}
\code{\link[RedeR:setNodeFontXY]{setNodeFontXY}}
\code{\link[RedeR:setNodeSize]{setNodeSize}}
\code{\link[RedeR:setNodeShape]{setNodeShape}}
\code{\link[RedeR:setNodeColor]{setNodeColor}}
\code{\link[RedeR:setNodeWeight]{setNodeWeight}}
\code{\link[RedeR:setNodeLineWidth]{setNodeLineWidth}}
\code{\link[RedeR:setNodeLineColor]{setNodeLineColor}}
\code{\link[RedeR:setNodeFontSize]{setNodeFontSize}}
\code{\link[RedeR:setNodeFontColor]{setNodeFontColor}}
\code{\link[RedeR:RedPort]{RedPort}}
}




\examples{

rdp <- RedPort('MyPort')

\dontrun{

calld(rdp)

###

g1 <- graph.empty(n=10, directed=FALSE)
addGraph( rdp, g1, layout.random(g1) )

###

g2 <- graph.lattice(c(5,5,5))
addGraph( rdp, g2, layout.kamada.kawai(g2) )

###...to check loading speed of an interactome!

data(hs.inter)system.time( addGraph(rdp, hs.inter, layout=NULL) )

}
}

\keyword{graph}
