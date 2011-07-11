\name{RedPort-class}
\docType{class}
\alias{RedPort-class}

\title{Class "RedPort" }

\description{A class providing access to the RedeR application. }

\section{Slots}{
  \describe{
    \item{\code{title}:}{The name of the XML-RPC port.}
    \item{\code{uri}:}{The uri to the XML-RPC server.}
    \item{\code{port}:}{The port number to the XML-RPC server.} 
    \item{\code{jclass}:}{The RedeR Java class that should wrap up R graphics.}    
  }
}

\section{Methods}{ 

    \subsection{Get node attributes from a RedeR session}{
        \describe{
            \item{\link[RedeR:getNodes]{getNodes}}{}
            \item{\link[RedeR:getNodeIDs]{getNodeIDs}}{}
            \item{\link[RedeR:getNodeAliases]{getNodeAliases}}{}
            \item{\link[RedeR:getNodeX]{getNodeX}}{}
            \item{\link[RedeR:getNodeY]{getNodeY}}{}
            \item{\link[RedeR:getNodeW]{getNodeW}}{}
            \item{\link[RedeR:getNodeH]{getNodeH}}{}
            \item{\link[RedeR:getNodeBend]{getNodeBend}}{}
            \item{\link[RedeR:getNodeSize]{getNodeSize}}{}
            \item{\link[RedeR:getNodeShape]{getNodeShape}}{}
            \item{\link[RedeR:getNodeColor]{getNodeColor}}{}
            \item{\link[RedeR:getNodeLineWidth]{getNodeLineWidth}}{}
            \item{\link[RedeR:getNodeLineColor]{getNodeLineColor}}{}
            \item{\link[RedeR:getNodeFontName]{getNodeFontName}}{}
            \item{\link[RedeR:getNodeFontStyle]{getNodeFontStyle}}{}
            \item{\link[RedeR:getNodeFontSize]{getNodeFontSize}}{}
            \item{\link[RedeR:getNodeFontColor]{getNodeFontColor}}{}
            \item{\link[RedeR:getNodeFontX]{getNodeFontX}}{}
            \item{\link[RedeR:getNodeFontY]{getNodeFontY}}{}
            \item{\link[RedeR:getNodeWeight]{getNodeWeight}}{}
        }      
    }
    
    \subsection{Set node attributes from a RedeR session}{
        \describe{
          \item{\link[RedeR:setNodeXY]{setNodeXY}}{}
          \item{\link[RedeR:setNodeBend]{setNodeBend}}{}
          \item{\link[RedeR:setNodeSize]{setNodeSize}}{}
          \item{\link[RedeR:setNodeShape]{setNodeShape}}{}
          \item{\link[RedeR:setNodeColor]{setNodeColor}}{}
          \item{\link[RedeR:setNodeLineWidth]{setNodeLineWidth}}{}
          \item{\link[RedeR:setNodeLineColor]{setNodeLineColor}}{}
          \item{\link[RedeR:setNodeFontName]{setNodeFontName}}{}
          \item{\link[RedeR:setNodeFontStyle]{setNodeFontStyle}}{}
          \item{\link[RedeR:setNodeFontSize]{setNodeFontSize}}{}
          \item{\link[RedeR:setNodeFontColor]{setNodeFontColor}}{}
          \item{\link[RedeR:setNodeFontXY]{setNodeFontXY}}{}
          \item{\link[RedeR:setNodeWeight]{setNodeWeight}}{}
        }      
    }
    
  \subsection{Get edge attributes from a RedeR session}{  
    \describe{
      \item{\link[RedeR:getEdges]{getEdges}}{}
      \item{\link[RedeR:getSourceEdges]{getSourceEdges}}{}
      \item{\link[RedeR:getTargetEdges]{getTargetEdges}}{}
      \item{\link[RedeR:getEdgeIDs]{getEdgeIDs}}{}
      \item{\link[RedeR:getSourceEdgeIDs]{getSourceEdgeIDs}}{}
      \item{\link[RedeR:getTargetEdgeIDs]{getTargetEdgeIDs}}{}
      \item{\link[RedeR:getArrowDirection]{getArrowDirection}}{}
      \item{\link[RedeR:getEdgeWidth]{getEdgeWidth}}{}
      \item{\link[RedeR:getEdgeColor]{getEdgeColor}}{}
      \item{\link[RedeR:getEdgeType]{getEdgeType}}{}
      \item{\link[RedeR:getEdgeWeight]{getEdgeWeight}}{}
    }
  } 
  
  \subsection{Set edge attributes from a RedeR session}{  
    \describe{
      \item{\link[RedeR:setArrowDirection]{setArrowDirection}}{}
      \item{\link[RedeR:setEdgeWidth]{setEdgeWidth}}{}
      \item{\link[RedeR:setEdgeColor]{setEdgeColor}}{}
      \item{\link[RedeR:setEdgeType]{setEdgeType}}{}
      \item{\link[RedeR:setEdgeWeight]{setEdgeWeight}}{}
    }
  }
  
  \subsection{Methods that change graph structure}{  
    \describe{
        \item{\link[RedeR:addGraph]{addGraph}}{}
        \item{\link[RedeR:getGraph]{getGraph}}{}            
        \item{\link[RedeR:addNodes]{addNodes}}{}
        \item{\link[RedeR:deleteNodes]{deleteNodes}}{}
        \item{\link[RedeR:nestNodes]{nestNodes}}{}
        \item{\link[RedeR:updateContainerSize]{updateContainerSize}}{}
        \item{\link[RedeR:mergeOutEdges]{mergeOutEdges}}{}
        \item{\link[RedeR:getContainerComponets]{getContainerComponets}}{}
        \item{\link[RedeR:mergeNodes]{mergeNodes}}{}
        \item{\link[RedeR:addEdges]{addEdges}}{}
        \item{\link[RedeR:addEdgeBetweenContainers]{addEdgeBetweenContainers}}{}
        \item{\link[RedeR:deleteEdges]{deleteEdges}}{}
    }
  }

  \subsection{Methods to wrap up attributes and add/get graphs to/from RedeR}{  
    \describe{
        \item{\link[RedeR:addGraph]{addGraph}}{}
        \item{\link[RedeR:getGraph]{getGraph}}{} 
	\item{\link[RedeR:addSubgraph]{addSubgraph}}{}
	\item{\link[RedeR:addSeries]{addSeries}}{}
	\item{\link[RedeR:duplicateGraph]{duplicateGraph}}{}
    }
  }        
          
  \subsection{Other methods to manipulate RedeR graphs}{  
    \describe{
        \item{\link[RedeR:updateGraph]{updateGraph}}{}
        \item{\link[RedeR:selectEdges]{selectEdges}}{}
        \item{\link[RedeR:selectNodes]{selectNodes}}{}
        \item{\link[RedeR:selectAllEdges]{selectAllEdges}}{}
        \item{\link[RedeR:selectAllNodes]{selectAllNodes}}{}
        \item{\link[RedeR:selectGraph]{selectGraph}}{}
        \item{\link[RedeR:deSelectEdges]{deSelectEdges}}{}
        \item{\link[RedeR:deSelectNodes]{deSelectNodes}}{}
        \item{\link[RedeR:deSelectGraph]{deSelectGraph}}{}
        \item{\link[RedeR:deleteSelectedEdges]{deleteSelectedEdges}}{}
        \item{\link[RedeR:deleteSelectedNodes]{deleteSelectedNodes}}{}
        \item{\link[RedeR:isDynamicsActive]{isDynamicsActive}}{}
    }
  }  
  
  \subsection{Methods to establish RedeR server connection}{  
    \describe{
        \item{\link[RedeR:ping]{ping}}{}
        \item{\link[RedeR:version]{version}}{}
        \item{\link[RedeR:calld]{calld}}{} 
        \item{\link[RedeR:exitd]{exitd}}{}   
        \item{\link[RedeR:resetd]{resetd}}{} 
    }
  } 
  
  \subsection{Methods to build RedeR plugins}{  
    \describe{
        \item{\link[RedeR:PluginBuilder]{PluginBuilder}}{}
	\item{\link[RedeR:submitPlugin]{submitPlugin}}{}
        \item{\link[RedeR:deletePlugin]{deletePlugin}}{}
        \item{\link[RedeR:updatePlugins]{updatePlugins}}{}
        \item{\link[RedeR:pluginParser]{pluginParser}}{} 
        \item{\link[RedeR:dynwin]{dynwin}}{}         
        
    }
  }   
    
}

\section{Details}{

RedPort methods invoke RedeR application via XML-RPC (remote procedure call) server. 
For each R method listed above there is a Java mirror that executes a callback procedure. 
Therefore, the Java callback engine must be initialized before any callback from RedeR (i.e. start the Java application).

}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\author{Mauro Castro}

\examples{
    
    # Creates a RedeR object by calling the constructor
    rdp <- RedPort('MyPort')       

}
\keyword{graphs}
\keyword{classes}

