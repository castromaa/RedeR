#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('ping',               signature='obj', function (obj)                 
            standardGeneric ('ping'),          package='RedeR')
setGeneric ('version',            signature='obj', function (obj)                 
            standardGeneric ('version'),       package='RedeR')
setGeneric ('calld',              signature='obj', function (obj,...)            
            standardGeneric ('calld'),         package='RedeR')
setGeneric ('updateGraph',        signature='obj', function (obj)                 
            standardGeneric ('updateGraph'),   package='RedeR')
setGeneric ('getGraph',           signature='obj', function (obj, ...)            
            standardGeneric ('getGraph'),      package='RedeR')   
setGeneric ('addGraph',           signature='obj', function (obj, g, ...)            
            standardGeneric ('addGraph'),      package='RedeR')          
setGeneric ('addSeries',  	      signature='obj', function (obj, g, ...)            
            standardGeneric ('addSeries'),     package='RedeR') 
setGeneric ('duplicateGraph',     signature='obj', function (obj, ...)            
            standardGeneric ('duplicateGraph'), package='RedeR')
setGeneric ('addSubgraph',        signature='obj', function (obj, g, nodes, ...)            
            standardGeneric ('addSubgraph'),    package='RedeR')  
setGeneric ('addSubgraph.list',   signature='obj', function (obj, g, nodeList, ...)            
            standardGeneric ('addSubgraph.list'), package='RedeR')   
setGeneric ('exitd',            signature='obj', function (obj)                 
            standardGeneric ('exitd'),         package='RedeR') 
setGeneric ('resetd',           signature='obj', function (obj)                 
            standardGeneric ('resetd'),        package='RedeR') 
setGeneric ('addLegend.color',   signature='obj', function (obj, colvec, ...)            
            standardGeneric ('addLegend.color'), package='RedeR')
setGeneric ('addLegend.size',   signature='obj', function (obj, sizevec, ...)            
            standardGeneric ('addLegend.size'), package='RedeR')            
setGeneric ('addLegend.shape',   signature='obj', function (obj, shapevec, ...)            
            standardGeneric ('addLegend.shape'), package='RedeR')                                                                                            
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('getNodes',           signature='obj', function (obj, ...) 
            standardGeneric ('getNodes'),          package='RedeR')
setGeneric ('getNodeIDs',         signature='obj', function (obj, ...) 
            standardGeneric ('getNodeIDs'),        package='RedeR')
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('addNodes',           signature='obj', function (obj, nodes)      
            standardGeneric ('addNodes'),     package='RedeR')
setGeneric ('deleteNodes',        signature='obj', function (obj, nodes)      
            standardGeneric ('deleteNodes'),  package='RedeR')
setGeneric ('nestNodes',          signature='obj', function (obj, nodes,...)      
            standardGeneric ('nestNodes'),    package='RedeR')
setGeneric ('nesthc',          signature='obj', function (obj, hc,...)      
            standardGeneric ('nesthc'),    package='RedeR')            
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('updateContainerSize',    signature='obj', function (obj)             
            standardGeneric ('updateContainerSize'),    package='RedeR')
setGeneric ('mergeOutEdges', signature='obj', function (obj,...)             
            standardGeneric ('mergeOutEdges'), package='RedeR')
setGeneric ('getContainerComponets',  signature='obj', function (obj, container)  
            standardGeneric ('getContainerComponets'),  package='RedeR')
setGeneric ('mergeNodes',             signature='obj', function (obj, nodes)      
            standardGeneric ('mergeNodes'),             package='RedeR')
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('getEdges',           signature='obj', function (obj,...) 
            standardGeneric ('getEdges'),       package='RedeR')
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('getEdgeIDs',         signature='obj', function (obj,...) 
            standardGeneric ('getEdgeIDs'),       package='RedeR')
setGeneric ('getSourceEdgeIDs',   signature='obj', function (obj,...) 
            standardGeneric ('getSourceEdgeIDs'), package='RedeR')
setGeneric ('getTargetEdgeIDs',   signature='obj', function (obj,...) 
            standardGeneric ('getTargetEdgeIDs'), package='RedeR')
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
setGeneric ('setArrowDirection',  signature='obj', function (obj, nodeA, nodeB, direction)    
            standardGeneric ('setArrowDirection'), package='RedeR')
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
setGeneric ('addEdges',                 signature='obj', function (obj,edges)                      
            standardGeneric ('addEdges'),                 package='RedeR')
setGeneric ('deleteEdges',              signature='obj', function (obj,edges)                      
            standardGeneric ('deleteEdges'),              package='RedeR')
setGeneric ('addEdgeBetweenContainers', signature='obj', function (obj,containerA,containerB)  
            standardGeneric ('addEdgeBetweenContainers'), package='RedeR')
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
setGeneric ('selectEdges',        signature='obj', function (obj, nodeA, nodeB)    
            standardGeneric ('selectEdges'), package='RedeR')
setGeneric ('selectNodes',        signature='obj', function (obj, nodes, nt=NULL)           
            standardGeneric ('selectNodes'), package='RedeR')
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('selectAllEdges',     signature='obj', function (obj)     
            standardGeneric ('selectAllEdges'), package='RedeR')
setGeneric ('selectAllNodes',     signature='obj', function (obj)     
            standardGeneric ('selectAllNodes'), package='RedeR')
setGeneric ('selectGraph',        signature='obj', function (obj)     
            standardGeneric ('selectGraph'),    package='RedeR')
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('deSelectEdges',      signature='obj', function (obj)     
            standardGeneric ('deSelectEdges'), package='RedeR')
setGeneric ('deSelectNodes',      signature='obj', function (obj)     
            standardGeneric ('deSelectNodes'), package='RedeR')
setGeneric ('deSelectGraph',      signature='obj', function (obj)     
            standardGeneric ('deSelectGraph'), package='RedeR')
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('deleteSelectedEdges',  signature='obj', function (obj)   
            standardGeneric ('deleteSelectedEdges'), package='RedeR')
setGeneric ('deleteSelectedNodes',  signature='obj', function (obj)   
            standardGeneric ('deleteSelectedNodes'), package='RedeR')
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setGeneric ('isDynamicsActive',     signature='obj', function (obj)   
            standardGeneric ('isDynamicsActive'), package='RedeR')
setGeneric ('relax',     signature='obj', 
function (obj,p1=100,p2=100,p3=100,p4=100, p5=100,p6=100,p7=10,p8=10,ps=FALSE)
            standardGeneric ('relax'), package='RedeR') 

