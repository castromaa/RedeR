\name{RedPort-class}
\docType{class}
\alias{RedPort-class}

\title{Class "RedPort" }

\description{A class providing access to the RedeR application. }

\section{Slots}{
  \describe{
    \item{\code{title}:}{The name of the XML-RPC port.}
    \item{\code{uri}:}{The uri to the XML-RPC server.}
    \item{\code{host}:}{The host to the XML-RPC server.}  
    \item{\code{port}:}{The port number to the XML-RPC server.}
  }
}

\section{Methods}{ 

    \subsection{Get node attributes from a RedeR session}{
        \describe{
            \item{\link[RedeR:getNodes]{getNodes}}{}
            \item{\link[RedeR:getNodeIDs]{getNodeIDs}}{}
        }      
    }
    
  \subsection{Get edge attributes from a RedeR session}{  
    \describe{
      \item{\link[RedeR:getEdges]{getEdges}}{}
      \item{\link[RedeR:getEdgeIDs]{getEdgeIDs}}{}
      \item{\link[RedeR:getSourceEdgeIDs]{getSourceEdgeIDs}}{}
      \item{\link[RedeR:getTargetEdgeIDs]{getTargetEdgeIDs}}{}
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
        \item{\link[RedeR:setArrowDirection]{setArrowDirection}}{}
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

