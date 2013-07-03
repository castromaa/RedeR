
#-------------------------------------------------------------------------------
setMethod ('ping', 'RedPort', 
           function (obj) { 
             #Check if the port is not in use by the app------------------------
             calltest=try(rederexpresspost(obj@uri,'RedHandler.ping'), TRUE) 
             if(is.character(calltest) && length(calltest)==1){
               if(calltest=="1"){
                 return(1)
               } else {
                 return(0)
               }
             } else {
               return(0)
             }
           }
)

#-------------------------------------------------------------------------------
setMethod ('exitd', 'RedPort', 
           function (obj) { 
             if(ping(obj)==0)return(invisible())
             Sys.sleep(0.5)
             invisible(rederexpresspost(obj@uri, 'RedHandler.exit'))
             Sys.sleep(0.5)
           }
)

#-------------------------------------------------------------------------------
setMethod ('resetd', 'RedPort', 
           function (obj) {
             if(ping(obj)==0)return(invisible())
             invisible(rederexpresspost(obj@uri, 'RedHandler.reset'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('version', 'RedPort', 
           function (obj) { 
             if(ping(obj)==0)return(invisible())
             return (rederexpresspost (obj@uri, 'RedHandler.version'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('calld', 'RedPort',
           
           function (obj, filepath='default', ADDPATH='', checks='lock', maxlag=300) {
             
             #Check if the port is not in use by the app---------------------------
             if(ping(obj)==1){
               tx=paste("R interface is already in use! Port: ", obj@port, sep='') 
               return(tx)
             }
             if(!is.numeric(maxlag))maxlag=300
             
             #Set local paths and call RedeR app:----------------------------------
             
             #(1)Check rJava package, get path to JRI if present!------------------
             jriPath=system.file("jri",package="rJava")
             if(nchar(jriPath)>2){
               if(nchar(ADDPATH)>2){
                 ADDPATH=paste(ADDPATH, jriPath, sep=.Platform$path.sep)
               } else {
                 ADDPATH = jriPath
               }
             }   
             
             #(2) get path to the 'reder.jar' file---------------------------------     
             if(filepath=="default"){
               filepath = system.file(package = "RedeR", "java/reder.jar")
             }
             
             #(3) get adjusted paths to OS type--------------------------------      
             os=regexpr("win", .Platform$OS.type, ignore.case=TRUE)
             if(os[1]>=0){os="win"} else {os="unix.alikes"}
             R_HOME=R.home()
             if(os=="unix.alikes"){ 
               shtype="sh"    
               libPath=R.home(component="lib")
               srdir=Sys.getenv("R_SHARE_DIR")
               indir=Sys.getenv("R_INCLUDE_DIR")
               dcdir=Sys.getenv("R_DOC_DIR")
               userlbdir=Sys.getenv("R_LIBS_USER") 
               myPaths=paste(R_HOME, libPath, ADDPATH, indir, srdir, dcdir, userlbdir, sep=.Platform$path.sep)                                                   
             } else { ##...wind.
               shtype="cmd"      
               libPath=R.home(component="bin")
               srdir=R.home(component="share")
               indir=R.home(component="include")
               dcdir=R.home(component="doc")
               userlbdir=R.home(component="R_LIBS_USER")
               myPaths=paste(R_HOME, libPath, ADDPATH, indir, srdir, dcdir, userlbdir, sep=.Platform$path.sep)
               myPaths=gsub("/","\\\\", myPaths)                                                                           
             }
             
             #(4) add myPaths to system paths--------------------------------------
             PATH=paste(Sys.getenv("PATH"), myPaths, sep=.Platform$path.sep)
             Sys.setenv(PATH=PATH)  
             
             #(5) quote paths to be passed to RedeR shell
             filepath=shQuote(filepath, type=shtype)
             R_HOME =shQuote(R_HOME, type=shtype) 
             libPath=shQuote(libPath, type=shtype) 
             ADDPATH =shQuote(ADDPATH, type=shtype) 
             indir =shQuote(indir, type=shtype) 
             srdir =shQuote(srdir, type=shtype)
             dcdir =shQuote(dcdir, type=shtype)
             userlbdir =shQuote(userlbdir, type=shtype) 
             myPaths=paste(R_HOME, libPath, ADDPATH, indir, srdir, dcdir, userlbdir, sep=' ')
             
             #(6)Execute 'calld' and update app settings at RedeR preferences:-----               
             argm    = paste('openshellDcall', obj@port, myPaths, checks, sep=' ')
             command = paste('java -jar',      filepath, argm,    sep=' ')
             system(command, ignore.stdout = TRUE, ignore.stderr = TRUE, wait=FALSE) 
             
             #(7) Wait response from the app (implement a short-delay)-------------
             status="OFF"
             tdelta=0
             t0=proc.time()[2] #...used to start time delay! 
             maxlag=maxlag/100
             while(status=="OFF"){        
               #timer
               tdelta = proc.time()[2] - t0
               if(tdelta>maxlag){
                 status="OFFON"
               }
               #
               if(ping(obj)==1){
                 status="ON"
                 message("RedeR is ready!")
               }
             }       
             
             #(8) ..send message if dubious connection status!----------------------
             if(status=="OFFON") {
               message("Prior to call RedeR check the interface! (e.g. 'ping' function)")
             } 
             
           }
)            

#-------------------------------------------------------------------------------
setMethod ('updateGraph', 'RedPort', 
           function (obj) { 
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost (obj@uri, 'RedHandler.updateGraph'))
           }
)

#Wrap up R graphics into Java classes   
#-------------------------------------------------------------------------------
setMethod ('dynwin', 'RedPort', 
           function (obj, width=400, height=300, ps=10) {
             if(ping(obj)==0)return(invisible())
             width=as.integer(width)
             height=as.integer(height)
             ps=as.integer(ps)
             if(width<0)return('Invalid width!') 
             if(height<0)return('Invalid height!')  
             if(ps<0)return('Invalid ps!')   
             Sys.setenv('JAVAGD_CLASS_NAME'=obj@jclass)
             JavaGD(width=width, height=height, ps=ps)
           }
)

#Get RedeR graph via RedeR methods and wrap it up into igraph objects  
#-------------------------------------------------------------------------------
setMethod ('getGraph', 'RedPort', 
           function (obj, status="all", type="node", attribs="plain") {
             if(ping(obj)==0)return(igraph::graph.empty(n=0, directed=FALSE))
             
             #check loaded igraph
             igraph.check()
             
             #Get graph objects from RedeR app
             nodes = getNodes(obj, status, type)
             edges = getEdges(obj, status, type) 
             
             #Build igraph object
             if(length(nodes)==1 && nodes==""){
               g = igraph::graph.empty(n=0, directed=FALSE)
               return(g) 
             } else if(length(edges)==1 && edges ==""){
               edges = NULL
               g	   = igraph::graph.empty(n=length(nodes), directed=FALSE)
               g     = igraph::set.vertex.attribute(g, "name", value=nodes) 
             } else {
               nodes	= data.frame(name=nodes, stringsAsFactors=FALSE)
               edges 	= matrix(edges,ncol=2, byrow=TRUE)
               colnames(edges) = c("NodeA","NodeB")
               edges 	= data.frame(edges,stringsAsFactors=FALSE) 
               g  	 	= igraph::graph.data.frame(edges, directed=FALSE, vertices=nodes)
             }  
             
             #Add required attributes    
             if(attribs=="minimal"){
               return(g) 
             } else if(attribs=="plain"){
               nodeX     = .getNodeX(obj, status, type )
               nodeY     = .getNodeY(obj, status, type )
               g         = igraph::set.vertex.attribute(g, "coordX", value=nodeX)
               g         = igraph::set.vertex.attribute(g, "coordY", value=nodeY)
               return(g)
             } else if(attribs=="all"){
               nodeAlias      = .getNodeAliases (obj, status, type ) 
               nodeBend       = .getNodeBend (obj, status, type )     
               nodeX          = .getNodeX (obj, status, type )
               nodeY          = .getNodeY (obj, status, type )
               nodeSize       = .getNodeSize (obj, status, type )
               nodeShape      = .getNodeShape (obj, status, type )
               nodeColor      = .getNodeColor (obj, status, type )
               nodeWeight     = .getNodeWeight (obj, status, type )
               nodeLineWidth  = .getNodeLineWidth (obj, status, type )
               nodeLineColor  = .getNodeLineColor (obj, status, type ) 
               nodeFontSize   = .getNodeFontSize (obj, status, type )
               nodeFontColor  = .getNodeFontColor (obj, status, type )        
               g     = igraph::set.vertex.attribute(g, "nodeAlias",  value=nodeAlias)
               g     = igraph::set.vertex.attribute(g, "nodeBend",   value=nodeBend)
               g     = igraph::set.vertex.attribute(g, "coordX",   value=nodeX)
               g     = igraph::set.vertex.attribute(g, "coordY",   value=nodeY)
               g     = igraph::set.vertex.attribute(g, "nodeSize",   value=nodeSize)          
               g     = igraph::set.vertex.attribute(g, "nodeShape",  value=nodeShape)
               g     = igraph::set.vertex.attribute(g, "nodeColor",  value=nodeColor)
               g     = igraph::set.vertex.attribute(g, "nodeWeight", value=nodeWeight)
               g     = igraph::set.vertex.attribute(g, "nodeLineWidth",  value=nodeLineWidth)
               g     = igraph::set.vertex.attribute(g, "nodeLineColor",  value=nodeLineColor)
               g     = igraph::set.vertex.attribute(g, "nodeFontSize",   value=nodeFontSize)
               g     = igraph::set.vertex.attribute(g, "nodeFontColor",  value=nodeFontColor) 
               #..get edge attrs. if present!
               if(!is.null(edges)){
                 arrowDirection = .getArrowDirection (obj, status, type )  
                 edgeWeight     = .getEdgeWeight(obj, status, type )
                 edgeWidth      = .getEdgeWidth(obj, status, type )
                 edgeColor      = .getEdgeColor(obj, status, type )
                 edgeType       = .getEdgeType(obj, status, type )
                 g     = igraph::set.edge.attribute(g, "arrowDirection", value=arrowDirection)
                 #g     = igraph::set.edge.attribute(g, "arrowLength",    value= arrowLength)
                 #g     = igraph::set.edge.attribute(g, "arrowAngle",     value= arrowAngle)
                 g     = igraph::set.edge.attribute(g, "edgeWeight",     value=edgeWeight)
                 g     = igraph::set.edge.attribute(g, "edgeWidth",      value=edgeWidth)
                 g     = igraph::set.edge.attribute(g, "edgeColor",      value=edgeColor)
                 g     = igraph::set.edge.attribute(g, "edgeType",       value=edgeType)
               }          
               return(g)       
             }
           }
)

#Add subgraph list to RedeR app
#-------------------------------------------------------------------------------
setMethod ('addSubgraph.list', 'RedPort', 
           function (obj, g, nodeList, gridRows=2, gridScale=80, gscale=20, gatt=NULL, update=NULL, theme='tm0') {
             if(ping(obj)==0)return(invisible())
             
             #check loaded igraph
             igraph.check()
             
             #Check igraph object-----------------------------------------------
             if(!igraph::is.igraph(g)){
               stop("Not an igraph object!")
             }
             if(igraph::vcount(g)==0){
               stop("Empty main graph!")
             }		
             #Further checks-----------------------------------------------------
             if(!is.list(nodeList)){
               stop("NOTE: 'nodeList' must be a list of vectors with node names!")
             }
             if(length(nodeList)==0){
               stop("NOTE: invalid 'nodeList' arg length!")
             }		
             if(is.list(gatt))gatt=as.data.frame(gatt)
             if(!is.null(gatt) && !is.data.frame(gatt)){
               stop("NOTE: 'gatt' should be either list or data.frame with graph attributes (e.g. attribute names on cols)!")
             }
             if(!is.null(gatt) && nrow(gatt)!=length(nodeList)){
               stop("NOTE: 'gatt' length must match 'nodeList' length!")
             }
             #Remove multiple edges and loops---
             if(!igraph::is.simple(g)){
               #warning("NOTE: loops and/or multiple edges were removed from your graph!")
               g=igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
             }     
             #Check direction
             if(igraph::is.directed(g)){
               # set direction to edge attributes
               gtemp=g
               E(gtemp)$arrowDirection=1
               E(gtemp)$arrowDirection[is.mutual(gtemp)]=3
               # collapse mutual edges to unique edges and check edge attributes
               gtemp=igraph::as.undirected(gtemp, mode="each")
               gtemp=igraph::simplify(gtemp)
               c1=length(igraph::list.edge.attributes(g))>0
               c2=igraph::ecount(g)>igraph::ecount(gtemp)
               if(c1 && c2){
                 warning("NOTE: attributes from mutual edges was collapsed to unique edges (see 'addGraph' doc).")
               }
               g=gtemp
             } 
             #Set as char.  	
             if(is.null(V(g)$name)){
               V(g)$name=as.character(V(g))
             } else {
               V(g)$name=as.character(V(g)$name)
             }
             if(!is.numeric(gridRows)){gridRows=NULL}else{gridRows=gridRows[1]}
             zoom=NULL
             if(is.numeric(gridScale)){
               gridScale=gridScale[1]
               #set gridScale to zoom
               if(gridScale>100)gridScale=100
               if(gridScale<0)gridScale=0
               zoom=100-gridScale   	
             }
             
             #get a basic layout just for sugraphs' first view
             if(is.null(gridRows)){
               gbasic=igraph::graph.empty(n=length(nodeList),directed=FALSE)  
               layout=igraph::layout.norm(igraph::layout.circle(gbasic),xmin = 25, xmax=75, ymin=25, ymax=75) 
             } else {
               gridCols=length(nodeList)/gridRows
               if((gridCols/as.integer(gridCols))!=1){
                 gridCols=as.integer(gridCols)+1
               }
               bin=100/(gridCols+1)
               xgrid=c(1:gridCols)*bin
               bin=100/(gridRows +1)
               ygrid=c(1:gridRows)*bin
               layout=cbind(x=xgrid,y=ygrid[1])
               if(gridRows>1){
                 for(i in 2:gridRows){
                   lt=cbind(x=xgrid,y=ygrid[i])
                   layout=rbind(layout, lt)
                 }
               }
             }    
             # 'update="default"' forces to keep old node coords and not to add new containers!
             if(!is.null(update) && !is.character(update))update=NULL
             # internal function (locks DragAndZoon interactivity while sending the subgraph list to the data bank)
             invisible(rederexpresspost (obj@uri,'RedHandler.lockDragAndZoom'))
             
             #send request to addSubgraph fuction
             for(i in 1:length(nodeList)){
               nodes=nodeList[[i]]
               nodes=as.character(nodes)
               nmat=pmatch(nodes,V(g)$name)
               if(sum(!is.na(nmat))>0){	  	
                 if(!is.null(gatt)){
                   att=as.list(gatt[i,])
                   if(length(gatt)==1)names(att)=names(gatt)
                   if(is.null(update)){
                     att$coordX=layout[i,1]
                     att$coordY=layout[i,2]
                     if(!is.null(zoom)){
                       att$zoom=zoom
                     } else {
                       att$zoom=50
                     }
                   } else {
                     g <- igraph::remove.vertex.attribute(g,"coordX")
                     g <- igraph::remove.vertex.attribute(g,"coordY")
                     att$isNest=FALSE
                     att$update=update[1]
                   }
                   if(is.null(att$isNest))att$isNest=TRUE
                   if(is.null(att$isAnchor))att$isAnchor=TRUE
                   addSubgraph(obj,g,nodes,gscale=gscale,gatt=att,theme=theme, .callchecks=FALSE)
                 } else {
                   att=list()
                   if(is.null(update)){
                     att$coordX=layout[i,1]
                     att$coordY=layout[i,2]
                     if(!is.null(zoom)){
                       att$zoom=zoom
                     } else if(is.null(G(g,"zoom"))){
                       att$zoom=50
                     } else {
                       att$zoom=G(g,"zoom")
                     }
                   } else {
                     g <- igraph::remove.vertex.attribute(g,"coordX")
                     g <- igraph::remove.vertex.attribute(g,"coordY")
                     G(g,"isNest")=FALSE 	
                     att$update=update[1] 
                   }
                   if(!is.null(G(g,"gscale")))att$gscale=G(g,"gscale")
                   if(!is.null(G(g,"isNest"))){att$isNest=G(g,"isNest")}else{att$isNest=TRUE}
                   if(!is.null(G(g,"nestImage")))att$nestImage=as.character(G(g,"nestImage"))
                   if(!is.null(G(g,"isAnchor"))){att$isAnchor=G(g,"isAnchor")} else {att$isAnchor=TRUE}
                   if(!is.null(G(g,"nestAlias")))att$nestAlias=as.character(G(g,"nestAlias"))
                   if(!is.null(G(g,"nestColor")))att$nestColor=as.character(G(g,"nestColor"))
                   if(!is.null(G(g,"nestLineType")))att$nestLineType=as.character(G(g,"nestLineType"))
                   if(!is.null(G(g,"nestFontSize")))att$nestFontSize = G(g,"nestFontSize")
                   if(!is.null(G(g,"nestFontColor")))att$nestFontColor = G(g,"nestFontColor")
                   if(!is.null(G(g,"nestFontX")))att$nestFontX=G(g,"nestFontX")
                   if(!is.null(G(g,"nestFontY")))att$nestFontY=G(g,"nestFontY")
                   if(!is.null(G(g,"nestShape")))att$nestShape=as.character(G(g,"nestShape"))
                   if(!is.null(G(g,"nestSize")))att$nestSize=G(g,"nestSize")
                   if(!is.null(G(g,"nestLineWidth")))att$nestLineWidth=G(g,"nestLineWidth")
                   if(!is.null(G(g,"nestLineColor")))att$nestLineColor=as.character(G(g,"nestLineColor"))
                   if(!is.null(G(g,"isAssign")))att$isAssign=G(g,"isAssign")
                   if(!is.null(G(g,"loadEdges")))att$loadEdges=G(g,"loadEdges")
                   addSubgraph(obj,g,nodes, gscale=gscale, gatt=att, theme=theme, .callchecks=FALSE)
                 }
               }
             }  		
             #Internal function (unlocks DragAndZoon interactivity after sent subgraph list)
             invisible(rederexpresspost (obj@uri,'RedHandler.unLockDragAndZoom'))
           }
)

#Add subgraphs to RedeR app
#-------------------------------------------------------------------------------
setMethod ('addSubgraph', 'RedPort', 
           function (obj, g, nodes, gscale=75, gcoord=c(75,75), gatt=NULL,theme='tm0', .callchecks=TRUE) {
             if(.callchecks){
               if(ping(obj)==0)return(invisible())
             }
             
             #check loaded igraph
             igraph.check()
             
             #Check igraph object-----------------------------------------------
             if(!igraph::is.igraph(g)){
               stop("Not an igraph object!")
             }    	
             if(igraph::vcount(g)==0){
               stop("Empty main graph!")
             } 
             #Further checks-----------------------------------------------------
             if(!is.null(gatt) && !is.list(gatt)){
               stop("NOTE: 'gatt' must be a list of graph attributes (e.g. gatt$coordX, gatt$coordY, gatt$gscale...)!")
             }
             #Remove multiple edges and loops---
             if(!igraph::is.simple(g)){
               #warning("NOTE: loops and/or multiple edges were removed from your graph!")
               g=igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
             }
             #Check direction
             if(igraph::is.directed(g)){
               # set direction to edge attributes
               gtemp=g
               E(gtemp)$arrowDirection=1
               E(gtemp)$arrowDirection[is.mutual(gtemp)]=3
               # collapse mutual edges to unique edges and check edge attributes
               gtemp=igraph::as.undirected(gtemp, mode="each")
               gtemp=igraph::simplify(gtemp)
               c1=length(igraph::list.edge.attributes(g))>0
               c2=igraph::ecount(g)>igraph::ecount(gtemp)
               if(c1 && c2){
                 warning("NOTE: attributes from mutual edges was collapsed to unique edges (see 'addGraph' doc).")
               }
               g=gtemp
             }
             #Set as char.  	
             if(is.null(V(g)$name)){
               V(g)$name=as.character(V(g))
             } else {
               V(g)$name=as.character(V(g)$name)
             }
             #Add subgraphs
             nodes=as.character(nodes)
             nmat=pmatch(nodes,V(g)$name)
             if(sum(is.na(nmat))>0){
               stop("NOTE: one or more nodes are not represented in the main graph!")
             }
             #Set subg attributes
             if(!is.null(gatt)){
               zoom         = gatt$zoom
               scale        = gatt$gscale
               coordX       = gatt$coordX
               coordY       = gatt$coordY
               isNest       = gatt$isNest
               nestImage    = gatt$nestImage
               isAnchor     = gatt$isAnchor
               isAssign     = gatt$isAssign
               loadEdges    = gatt$loadEdges
               nestColor    = gatt$nestColor
               bgColor      = gatt$bgColor
               nestAlias    = gatt$nestAlias
               nestFontSize = gatt$nestFontSize
               nestFontColor = gatt$nestFontColor
               nestFontX    = gatt$nestFontX
               nestFontY    = gatt$nestFontY	
               nestShape      = gatt$nestShape
               nestSize       = gatt$nestSize
               nestLineWidth  = gatt$nestLineWidth
               nestLineColor  = gatt$nestLineColor
               update         = gatt$update
               nestLineType   = gatt$nestLineType
             } else {
               zoom         = G(g,"zoom")
               scale        = G(g,"gscale")
               coordX       = G(g,"coordX")
               coordY       = G(g,"coordY")
               nestImage    = G(g,"nestImage")
               isAnchor     = G(g,"isAnchor")
               loadEdges    = G(g,"loadEdges")
               bgColor      = G(g,"bgColor")
               isNest       = G(g,"isNest")
               isAssign     = G(g,"isAssign")
               nestAlias    = G(g,"nestAlias")
               nestColor    = G(g,"nestColor")
               nestFontSize = G(g,"nestFontSize")
               nestFontColor = G(g,"nestFontColor")
               nestFontX     = G(g,"nestFontX")
               nestFontY     = G(g,"nestFontY")
               nestShape      = G(g,"nestShape")
               nestSize       = G(g,"nestSize")
               nestLineWidth  = G(g,"nestLineWidth")
               nestLineColor  = G(g,"nestLineColor")
               update         = G(g,"update")
               nestLineType   = G(g,"nestLineType")
             }
             
             #Add subgraph
             sg=igraph::induced.subgraph(graph=g,vids=nodes)
             if(!is.null(zoom))    G(sg,"zoom") = zoom
             if(!is.null(bgColor)) G(sg,"bgColor") = as.character(bgColor)
             if(!is.null(scale))   G(sg,"gscale") = scale
             if(!is.null(coordX))  G(sg,"coordX") = coordX
             if(!is.null(coordY))  G(sg,"coordY") = coordY
             if(!is.null(loadEdges)) G(sg,"loadEdges") = loadEdges
             if(!is.null(isNest))    G(sg,"isNest") = isNest
             if(!is.null(nestImage))   G(sg,"nestImage") = as.character(nestImage)
             if(!is.null(isAnchor)){G(sg,"isAnchor") = isAnchor} else {G(sg,"isAnchor")=TRUE}
             if(!is.null(nestAlias))G(sg,"nestAlias") = as.character(nestAlias)
             if(!is.null(nestColor))  G(sg,"nestColor") = as.character(nestColor)
             if(!is.null(nestFontSize)) G(sg,"nestFontSize") = nestFontSize
             if(!is.null(nestFontColor))G(sg,"nestFontColor") = nestFontColor
             if(!is.null(nestFontX)) G(sg,"nestFontX") = nestFontX
             if(!is.null(nestFontY)) G(sg,"nestFontY") = nestFontY
             if(!is.null(nestShape)) G(sg,"nestShape") = as.character(nestShape)
             if(!is.null(nestSize))G(sg,"nestSize")=nestSize
             if(!is.null(nestLineWidth)) G(sg,"nestLineWidth") = nestLineWidth
             if(!is.null(nestLineColor)) G(sg,"nestLineColor") = as.character(nestLineColor)
             if(!is.null(nestLineType))G(sg,"nestLineType") = nestLineType
             if(!is.null(update)){
               if(update=="all" || update=="partial")G(sg,"isUpdate")=TRUE
               if(update=="partial")G(sg,"loadEdges")=FALSE
             }
             if(!is.null(isAssign) && isAssign==TRUE){
               G(sg,"isAssign")=TRUE
             }
             if(!is.null(update)){
               ref=addGraph(obj, sg, .callchecks=FALSE)
             } else {		
               ref=addGraph(obj, sg, gscale=gscale, gcoord=gcoord, theme=theme, .callchecks=FALSE)
               if(!is.null(ref))return(ref)
             }
           }
)

#Duplicate RedeR networks and subnetworks
#-------------------------------------------------------------------------------
setMethod ('duplicateGraph', 'RedPort', 
           function (obj, isToCopyEdges=TRUE, isDefaultCopy=TRUE, nodes=NULL) {
             if(ping(obj)==0)return(invisible())
             
             #check loaded igraph
             igraph.check()
             
             if(!is.null(nodes)){
               arg1="yes"
               arg2="yes" 
               if(!isToCopyEdges){arg1="no"}
               if(!is.character(nodes)){
                 warning("NOTE: 'nodes' arg must be provided as character!")
               }      		
               else if(length(nodes)<=1){
                 warning("NOTE: invalid 'nodes' arg (length<=1)!")
               } else {
                 nodes=as.character(nodes)
                 message("... duplicate subgraph")
                 invisible( rederexpresspost(obj@uri, 'RedHandler.duplicateSubNetwork', arg1, arg2, nodes ) )
               }
             } else {
               arg1="yes"
               arg2="yes" 
               if(!isToCopyEdges){arg1="no"}
               if(!isDefaultCopy){arg2="no"}
               message("... duplicate graph")
               invisible( rederexpresspost(obj@uri, 'RedHandler.duplicateNetwork', arg1, arg2) )
             }		
           }
)

#Duplicate RedeR network and update the copy with new attributes 
#-------------------------------------------------------------------------------
setMethod ('addSeries', 'RedPort', 
           function (obj, g, setnodes=TRUE, setedges=TRUE) {
             if(ping(obj)==0)return(invisible())
             
             #check loaded igraph 
             igraph.check()
             
             #Check igraph object-----------------------------------------------
             if(!igraph::is.igraph(g)){
               stop("Not an igraph object!")
             }   
             if(igraph::vcount(g)==0){
               stop("Empty graph!")
             }
             #Remove attributes no longer needed!
             g <- igraph::remove.vertex.attribute(g,"coordX")
             g <- igraph::remove.vertex.attribute(g,"coordY")      
             if(!setnodes){  
               g <- igraph::remove.vertex.attribute(g,"nodeAlias")
               g <- igraph::remove.vertex.attribute(g,"nodeBend")
               g <- igraph::remove.vertex.attribute(g,"nodeSize")
               g <- igraph::remove.vertex.attribute(g,"nodeShape")
               g <- igraph::remove.vertex.attribute(g,"nodeColor")
               g <- igraph::remove.vertex.attribute(g,"nodeWeight")
               g <- igraph::remove.vertex.attribute(g,"nodeLineWidth")
               g <- igraph::remove.vertex.attribute(g,"nodeLineColor")
               g <- igraph::remove.vertex.attribute(g,"nodeFontSize")
               g <- igraph::remove.vertex.attribute(g,"nodeFontColor")
             }
             isToCopyEdges="no"
             if(!setedges){
               isToCopyEdges="yes"
               g <- remove.edge.attribute(g,"arrowDirection")
               g <- remove.edge.attribute(g,"arrowLength")
               g <- remove.edge.attribute(g,"arrowAngle") 
               g <- remove.edge.attribute(g,"linkType")     	
               g <- remove.edge.attribute(g,"edgeWeight")
               g <- remove.edge.attribute(g,"weight")
               g <- remove.edge.attribute(g,"edgeWidth")
               g <- remove.edge.attribute(g,"edgeColor")
               g <- remove.edge.attribute(g,"edgeType")
               g <- igraph::delete.edges(g,E(g))
             }
             
             #send request to duplicate the original network in the main panel
             isDefaultCopy="no"
             ref=try(rederexpresspost(obj@uri,'RedHandler.duplicateNetwork', isToCopyEdges, isDefaultCopy),TRUE)
             if(!inherits(ref, "try-error")){
               print(paste("New container ID: ", ref, sep=""))
               #send graph to RedeR (it will update the reference network)
               addGraph(obj,g)    	
             } else {
               warning("Unable to complete the request!")
             }
           }
)

#Wrap up igraph objects via RedeR methods and submit to RedeR app 
#-------------------------------------------------------------------------------
setMethod ('addGraph', 'RedPort', 
           function (obj, g, layout=igraph::layout.random(g), gscale=75, gcoord=c(50,50), 
                     zoom=NULL, isNest=FALSE, nestImage='plain', isAnchor=TRUE, isAssign=FALSE, 
                     loadEdges=TRUE, parent=NULL, minimal=FALSE, theme='tm0', igraphatt=TRUE, 
                     ntransform=FALSE, .callchecks=TRUE) {
             #Callcheck
             if(.callchecks)if(ping(obj)==0)return(invisible())
             
             #check loaded igraph
             igraph.check()
             
             #Check igraph object-----------------------------------------------
             if(!igraph::is.igraph(g))stop("Not an igraph object!")
             
             #Check igraph direction
             if(igraph::is.directed(g))g<-check.igraph.direction(g)
             
             #Check igraph format
             if(igraphatt==TRUE)g<-check.igraph.format(g)

             #Check igraph size-------------------------------------------------
             #...if empty graph!
             if(igraph::vcount(g)==0){
               g=igraph::add.vertices(g, 2)
               if((!is.null(G(g,"isNest")) && G(g,"isNest"))||isNest){
                 V(g)$name=c("NN0<$$>","NN1<$$>")
               } else {
                 V(g)$name=c("MM0<$$>","MM1<$$>")
               }
               layout=igraph::layout.random(g)
             }
             #Get names from vertex if not available------------------------------
             if(is.null(V(g)$name)){
               V(g)$name=as.character(V(g))
             } else {
               V(g)$name=as.character(V(g)$name)
             }
             #Add empty node if igraph::vcount(g)==1 
             #..to keep data type as vector during server connection!   
             if(igraph::vcount(g)==1){
               g=igraph::add.vertices(g, 1)
               if((!is.null(G(g,"isNest")) && G(g,"isNest"))||isNest){
                 V(g)$name[2]="NN0<$$>"   		
               } else {
                 V(g)$name[2]="MM0<$$>"  		
               }
               layout=igraph::layout.random(g)
               if(is.character(V(g)$nodeAlias))V(g)$nodeAlias=V(g)$nodeAlias[1]
               if(is.numeric(V(g)$coordX))V(g)$coordX=V(g)$coordX[1]
               if(is.numeric(V(g)$coordY))V(g)$coordY=V(g)$coordY[1]
               if(is.numeric(V(g)$nodeBend))V(g)$nodeBend=V(g)$nodeBend[1]   
               if(is.numeric(V(g)$nodeSize))V(g)$nodeSize=V(g)$nodeSize[1]
               if(is.character(V(g)$nodeShape))V(g)$nodeShape=V(g)$nodeShape[1]
               if(is.character(V(g)$nodeColor))V(g)$nodeColor=V(g)$nodeColor[1]
               if(is.numeric(V(g)$nodeWeight))V(g)$nodeWeight=V(g)$nodeWeight[1]
               if(is.numeric(V(g)$nodeLineWidth))V(g)$nodeLineWidth=V(g)$nodeLineWidth[1]
               if(is.character(V(g)$nodeLineColor))V(g)$nodeLineColor=V(g)$nodeLineColor[1]
               if(is.numeric(V(g)$nodeFontSize))V(g)$nodeFontSize=V(g)$nodeFontSize[1]
               if(is.character(V(g)$nodeFontColor))V(g)$nodeFontColor=V(g)$nodeFontColor[1]
             }
             
             #Check logical args.
             isNest=ifelse(!is.logical(isNest),FALSE,isNest)
             isAnchor=ifelse(!is.logical(isAnchor),TRUE,isAnchor)
             isAssign=ifelse(!is.logical(isAssign),FALSE,isAssign)
             loadEdges=ifelse(!is.logical(loadEdges),TRUE,loadEdges)
             minimal=ifelse(!is.logical(minimal),FALSE, minimal)
             
             #Check/get graph attribs.
             if(is.numeric(G(g,"gscale")) )gscale=G(g,"gscale")
             if(is.numeric(G(g,"coordX")) )gcoord[1]=G(g,"coordX") 
             if(is.numeric(G(g,"coordY")) )gcoord[2]=G(g,"coordY")   
             if(is.logical(G(g,"isNest")) )isNest=G(g,"isNest")
             if(is.logical(G(g,"isAssign")) )isAssign=G(g,"isAssign")
             if(is.character(G(g,"nestImage")) )nestImage=G(g,"nestImage")
             if(is.logical(G(g,"isAnchor")) )isAnchor=G(g,"isAnchor")
             if(is.numeric(G(g,"zoom")) )zoom=G(g,"zoom")
             
             #..nested assigments are not straightforward for R-J!!
             if(isAssign && isNest){
               temp=V(g)$name
               if(is.null(V(g)$nodeAlias)){
                 V(g)$nodeAlias=temp
               } else {
                 idx=is.na(V(g)$nodeAlias)
                 V(g)$nodeAlias[idx]=temp[idx]
               }
               id="N001"
               if(is.character(parent))id=parent
               V(g)$name=paste(temp,".$",id,sep="")
             } 
             #else if(isNest){
             #	parent=NULL
             #}
             
             if(is.logical(G(g,"loadEdges")))loadEdges=G(g,"loadEdges")   
             bgColor=NULL
             if(is.character(G(g,"bgColor")))bgColor=G(g,"bgColor")          
             nestColor=NULL
             # if theme is list, gatt gets all attributes for further settings
             # to address to nestNodes function, and also set some local graph attributes
             if(is.list(theme)){
               gatt=theme
               if(is.null(theme$theme)){
                 theme=0
               } else {
                 theme=theme$theme
               }
               if(is.numeric(gatt$zoom))zoom=gatt$zoom[1]
               if(is.numeric(gatt$gscale))gscale=gatt$gscale[1]
               if(is.numeric(gatt$gcoord))gcoord=gatt$gcoord
               if(is.logical(gatt$isNest))isNest=gatt$isNest
               if(is.logical(gatt$isAnchor))isAnchor=gatt$isAnchor
               if(is.logical(gatt$isAssign))isAssign=gatt$isAssign
               if(is.logical(gatt$loadEdges))loadEdges=gatt$loadEdges
               if(is.character(gatt$parent))parent=gatt$parent[1]
               if(is.character(gatt$nestImage))nestImage=gatt$nestImage[1] 			
             } else {
               gatt<-list()   
             }
             # but if g has nest attributes, it is prioritized over theme!
             if(is.character(G(g,"nestColor")) )gatt$nestColor=G(g,"nestColor") 
             nestAlias=NULL
             if(is.character(G(g,"nestAlias")))gatt$nestAlias=G(g,"nestAlias")
             nestFontSize=NULL
             if(is.numeric(G(g,"nestFontSize")))gatt$nestFontSize=G(g,"nestFontSize") 
             nestFontColor =NULL 
             if(is.character(G(g,"nestFontColor")))gatt$nestFontColor=G(g,"nestFontColor")    
             nestFontX=NULL 
             if(is.numeric(G(g,"nestFontX")))gatt$nestFontX=G(g,"nestFontX")
             nestFontY=NULL 
             if(is.numeric(G(g,"nestFontY")))gatt$nestFontY=G(g,"nestFontY")
             nestShape=NULL 
             if(is.character(G(g,"nestShape")))gatt$nestShape=G(g,"nestShape")
             nestSize=NULL 
             if(is.numeric(G(g,"nestSize"))) gatt$nestSize=G(g,"nestSize")  
             nestLineWidth=NULL 
             if(is.numeric(G(g,"nestLineWidth")))gatt$nestLineWidth=G(g,"nestLineWidth")  
             nestLineColor =NULL 
             if(is.character(G(g,"nestLineColor")))gatt$nestLineColor=G(g,"nestLineColor")
             nestLineType =NULL 
             if(is.character(G(g,"nestLineType")))gatt$nestLineType=G(g,"nestLineType")
             update="default"
             if(is.logical(G(g,"isUpdate"))){
               if(G(g,"isUpdate"))update="update"
             }      
             #Check gcoord option-----------------------------------------------
             c1=!is.numeric(gcoord)
             c2=!length(gcoord)==2
             if(c1 || c2){
               if(G(g,"gcoord")!=NULL){
                 warning("NOTE: attribute 'gcoord' is not set properly in the igraph object!")
               }    	
               stop("gcoord must be a numeric vector of length=2 (i.e. coords to the graph center)!")
             }
             
             #PS. the following methods must be used only in low-level calls!
             
             message('*** Uploading graph to RedeR server ***')
             
             #Set zoom if available
             if(!is.null(zoom)){ 
               zoom=zoom[1]
               if(!is.numeric(zoom)){
                 warning("NOTE: graph 'zoom' must be provided as numerics (range: 0.0 to 100.0)!")
               } else if(is.na(zoom)){
                 warning("NOTE: invalid graph 'zoom' declaration: 'NA' found'!")
               } else {            
                 message("** ... graph 'zoom'") 
                 invisible( rederexpresspost (obj@uri, 'RedHandler.setZoom', zoom) )
               }
             } else {
               zoom=100 # somente usado em 'themes'
             }
             
             #Check layout option-----------------------------------------------
             b=is.null(V(g)$coordX) || is.null(V(g)$coordY) 
             if(!is.null(layout) && !minimal && b){
               if(!is.matrix(layout)){
                 stop("Layout must be provided as matrix!")
               }     
               else if(ncol(layout)!=2){
                 stop("Layout matrix must have 2 cols (i.e. x and y coords)!")
               } 
               else if( nrow(layout)!=igraph::vcount(g) ) {
                 stop("Layout does not match graph vertices: inconsistent row number!")
               }
               else {
                 s1=!is.numeric(gscale)
                 s2=is.null(gscale)
                 s3=is.na(gscale)
                 if(s1 || s2 || s3){
                   warning("NOTE: attribute 'gscale' is not set properly; must be <numeric> of length=1!")
                   gscale = 75
                 }
                 pScale= rederexpresspost(obj@uri,'RedHandler.getPanelScale')
                 pScale=as.numeric(pScale)
                 if(is.numeric(pScale)){
                   pScale=pScale[1]*(gscale[1]/100)
                 } else {
                   pScale=500
                 }
                 if(isNest)pScale=pScale/sqrt(2)
                 layout=igraph::layout.norm(layout,xmin=0, xmax=pScale, ymin=0, ymax=pScale)
                 V(g)$coordX=layout[,1]
                 V(g)$coordY=layout[,2]
               }       
             }
             
             #Set/get nodes and edges to submit to the app------------------------        
             nodes = V(g)$name
             edges = igraph::get.edgelist(g, names=TRUE)
             edges = cbind(as.character(edges[,1]),as.character(edges[,2]))    
             
             #Set graph background color if available
             if(!is.null(bgColor)){ 
               bgColor=bgColor[1]
               if(!is.character(bgColor)){
                 warning("NOTE: graph 'color' must be provided as character (hexadecimal)!")
               }
               else if(is.na(bgColor)){
                 warning("NOTE: invalid graph 'color' declaration: 'NA' found'!")
               } else if(nchar(bgColor)>9){
                 warning("NOTE: invalid graph 'color' specification: not 'rgb' space! (ps. alpha not supported)")
               } else {            
                 message("** ... graph background 'color'") 
                 if(nchar(bgColor)>7) bgColor=substr(bgColor,0,7)
                 invisible( rederexpresspost (obj@uri, 'RedHandler.setBackground', bgColor) )
               }      
             }  
             
             #Add nodes, edges and set attributes (if available)------------------------
             if(igraph::vcount(g)>0)message("** ... nodes!") 
             if(igraph::ecount(g)>0)message("** ... edges!")
             
             #------------------------      
             #------------------------    	
             
             #...this option might be useful for large networks! not yet implemented!!
             if(minimal){	
               addNodes(obj, nodes)
               if(nrow(edges)>0 && loadEdges){
                 xedges <- matrix(data = NA, nrow = prod(dim(edges)), ncol = 1)
                 j=1
                 for(i in 1:nrow(edges)){
                   xedges[j]=edges[i,1]
                   j=j+1
                   xedges[j]=edges[i,2]
                   j=j+1
                 }
                 #internal function to load igraph edges! (..bit faster for dense graphs!)
                 invisible(rederexpresspost(obj@uri,'RedHandler.addEdgesFastload',as.character(xedges) ) )
                 return("Done!")
               }
             }
             
             if(length(igraph::list.vertex.attributes(g))>0){
               message('*** Uploading node attributes ...')   
             }
             
             nodeAlias      = V(g)$nodeAlias 
             coordX         = V(g)$coordX
             coordY         = V(g)$coordY 
             nodeBend       = V(g)$nodeBend       
             nodeSize       = V(g)$nodeSize
             nodeShape      = V(g)$nodeShape
             nodeColor      = V(g)$nodeColor
             nodeWeight     = V(g)$nodeWeight
             nodeLineWidth  = V(g)$nodeLineWidth
             nodeLineColor  = V(g)$nodeLineColor
             nodeFontSize   = V(g)$nodeFontSize
             nodeFontColor  = V(g)$nodeFontColor        
             #nodeAlias
             if(!is.null(nodeAlias) && length(nodeAlias)>0){       
               c1=!is.character(nodeAlias)
               if(c1){
                 warning("NOTE: node 'alias' must be provided as character!")
                 nodeAlias=as.character(c('',''))
               } else {
                 message("** ... node 'alias'")
                 nodeAlias[is.na(nodeAlias)]=nodes[is.na(nodeAlias)]
               }  
             }  else {	
               al=c(V(g)$name[1],V(g)$name[2])
               nodeAlias=as.character(al)
             }      
             #Node coords.
             c1=length(coordX)>0 && length(coordY)>0
             c2=!is.null(coordX) && !is.null(coordY)
             if(c1 && c2){
               c1=!is.numeric(coordX)
               c2=!is.numeric(coordY)
               if(c1 && c2){
                 warning("NOTE: node coords. must be provided as numerics!")
                 coordX=as.numeric(c(10,10))
                 coordY=as.numeric(c(10,10,10))
               }
               else if(sum(is.na(coordX))>0 || sum(is.na(coordY))>0 ){
                 warning("NOTE: invalid node coords. declaration: 'NA' found'!")
                 coordX=as.numeric(c(10,10))
                 coordY=as.numeric(c(10,10,10))
               } else {
                 message("** ... node 'coords'") 
                 coordX=as.numeric(coordX) 
                 coordY=as.numeric(coordY) 
               }      
             } else {
               coordX=as.numeric(c(10,10))
               coordY=as.numeric(c(10,10,10))      
             }
             #nodeBend
             if(!is.null(nodeBend) && length(nodeBend)>0){       
               c1=!is.numeric(nodeBend)
               if(c1){
                 warning("NOTE: node 'bend' must be provided as numerics!")
                 nodeBend=as.numeric(c(-1,-1))
               }
               else if(sum(is.na(nodeBend))>0){
                 warning("NOTE: invalid node 'bend' declaration: 'NA' found'!")
                 nodeBend=as.numeric(c(-1,-1))
               }
               else if(sum(nodeBend<0)>0 || sum(nodeBend>100)>0){
                 warning("NOTE: invalid node 'bend' input (options: 0-100%)")
                 nodeBend=as.numeric(c(-1,-1))
               } else {
                 message("** ... node 'bend'") 
                 nodeBend=as.numeric(nodeBend)
               }
             } else {
               nodeBend=as.numeric(c(-1,-1))
             }      
             #nodeSize
             if(!is.null(nodeSize) && length(nodeSize)>0){       
               c1=!is.numeric(nodeSize)
               if(c1){
                 warning("NOTE: node 'size' must be provided as numerics!")
                 nodeSize=as.numeric(c(-1,-1))
               }
               else if(sum(is.na(nodeSize))>0){
                 warning("NOTE: invalid node 'size' declaration: 'NA' found'!")
                 nodeSize=as.numeric(c(-1,-1))
               }
               else if(sum(nodeSize<0)>0){
                 warning("NOTE: invalid node 'size' input (options: >= 0)")
                 nodeSize=as.numeric(c(-1,-1))
               } else {            
                 message("** ... node 'size'") 
                 nodeSize=as.numeric(nodeSize)
               }
             } else {
               nodeSize=as.numeric(c(-1,-1))
             }        
             #nodeShape
             if(!is.null(nodeShape) && length(nodeShape)>0){       
               c1=!is.character(nodeShape)
               if(c1){
                 warning("NOTE: node 'shape' must be provided as character!")
                 nodeShape=as.character(c('',''))
               }
               else if(sum(is.na(nodeShape))>0){
                 warning("NOTE: invalid node 'shape' declaration: 'NA' found'!")
                 nodeShape=as.character(c('',''))
               } else {
                 message("** ... node 'shape'")
               }  
             }  else {
               nodeShape=as.character(c('',''))
             }                      
             #nodeColor
             if(!is.null(nodeColor) && length(nodeColor)>0 && is.character(nodeColor)){      
               nodeColor=colorRampPalette(colors=nodeColor)(length(nodeColor)) 
               if(sum(is.na(nodeColor))>0){
                 warning("NOTE: invalid node 'color' declaration: 'NA' found'!")
                 nodeColor=as.character(c('',''))
               } else if(sum(nchar(nodeColor)>9) ){
                 warning("NOTE: invalid node 'color' specification: not 'rgb' space! (ps. alpha not supported)")
                 nodeColor=as.character(c('',''))
               } else {            
                 message("** ... node 'color'")            
                 if(sum(nchar(nodeColor)>7))nodeColor=substr(nodeColor,0,7)
               }      
             } else {
               nodeColor=as.character(c('',''))
             }
             #nodeWeight
             if(!is.null(nodeWeight) && length(nodeWeight)>0){       
               c1=!is.numeric(nodeWeight)
               if(c1){
                 warning("NOTE: node 'weight' must be provided as numerics!")
                 nodeWeight=as.numeric(c(0.0,0.0))
               }
               else if(sum(is.na(nodeWeight))>0){
                 warning("NOTE: invalid node 'weight' declaration: 'NA' found'!")
                 nodeWeight=as.numeric(c(-1,-1))
               } else {
                 message("** ... node 'weight'")
                 nodeWeight=as.numeric(nodeWeight) 
               }
             } else {
               nodeWeight=as.numeric(c(0.0,0.0))
             }  
             #nodeLineWidth
             if(!is.null(nodeLineWidth) && length(nodeLineWidth)>0){       
               c1=!is.numeric(nodeLineWidth)
               if(c1){
                 warning("NOTE: node 'line width' must be provided as numerics!")
                 nodeLineWidth=as.numeric(c(-1,-1))
               }
               else if(sum(is.na(nodeLineWidth))>0){
                 warning("NOTE: invalid node 'line width' declaration: 'NA' found'!")
                 nodeLineWidth=as.numeric(c(-1,-1))
               }
               else if(sum(nodeLineWidth<0)>0){
                 warning("NOTE: invalid node 'line width' input (options: >= 0)")
                 nodeLineWidth=as.numeric(c(-1,-1))
               } else {            
                 message("** ... node 'line width'") 
                 nodeLineWidth=as.numeric(nodeLineWidth)
               }
             } else {
               nodeLineWidth=as.numeric(c(-1,-1))
             }
             #nodeLineColor
             if(!is.null(nodeLineColor) && length(nodeLineColor)>0 && is.character(nodeLineColor)){
               nodeLineColor=colorRampPalette(colors=nodeLineColor)(length(nodeLineColor)) 
               if(sum(is.na(nodeLineColor))>0){
                 warning("NOTE: invalid node 'line color' declaration: 'NA' found'!")
                 nodeLineColor=as.character(c('',''))
               } else if(sum(nchar(nodeLineColor)>9)){
                 warning("NOTE: invalid node 'line color' specification: not 'rgb' space! (ps. alpha not supported)")
                 nodeLineColor=as.character(c('',''))
               } else {           
                 message("** ... node 'line color'")
                 if(sum(nchar(nodeLineColor)>7))nodeLineColor=substr(nodeLineColor,0,7)
               }      
             } else {
               nodeLineColor=as.character(c('',''))
             }    
             #nodeFontSize
             if(!is.null(nodeFontSize) && length(nodeFontSize)>0){       
               c1=!is.integer(nodeFontSize)
               c2=!is.numeric(nodeFontSize)
               if(c1 && c2){
                 warning("NOTE: node 'font size' must be provided as integer!")
                 nodeFontSize=as.numeric(c(-1,-1))
               }
               else if(sum(is.na(nodeFontSize))>0){
                 warning("NOTE: invalid node 'font size' declaration: 'NA' found'!")
                 nodeFontSize=as.numeric(c(-1,-1))
               }
               else if(sum(nodeFontSize<0)>0){
                 warning("NOTE: invalid node 'font size' input (options: >= 0)")
                 nodeFontSize=as.numeric(c(-1,-1))
               } else {          
                 message("** ... node 'font size'")
                 nodeFontSize=as.numeric(nodeFontSize) 
               }
             } else {
               nodeFontSize=as.numeric(c(-1,-1))
             }
             #nodeFontColor
             if(!is.null(nodeFontColor) && length(nodeFontColor)>0 && is.character(nodeFontColor)){ 
               nodeFontColor=colorRampPalette(colors=nodeFontColor)(length(nodeFontColor))
               if(sum(is.na(nodeFontColor))>0){
                 warning("NOTE: invalid node 'font color' declaration: 'NA' found'!")
                 nodeFontColor=as.character(c('',''))
               } else if(sum(nchar(nodeFontColor)>9)){
                 warning("NOTE: invalid node 'font color' specification: not 'rgb' space! (ps. alpha not supported)")
                 nodeFontColor=as.character(c('',''))
               } else {            
                 message("** ... node 'font color'") 
                 if(sum(nchar(nodeFontColor)>7)) nodeFontColor=substr(nodeFontColor,0,7)
               }      
             } else {
               nodeFontColor=as.character(c('',''))
             }
             
             #Get/set edges attributes (if available)------------------------
             arrowDirection = E(g)$arrowDirection
             arrowLength    = E(g)$arrowLength
             arrowAngle     = E(g)$arrowAngle
             linkType       = E(g)$linkType
             edgeWeight     = E(g)$edgeWeight
             igraphWeight   = E(g)$weight
             edgeWidth      = E(g)$edgeWidth
             edgeColor      = E(g)$edgeColor
             edgeType       = E(g)$edgeType
             
             if(length(igraph::list.edge.attributes(g))>0 && igraph::ecount(g)>0){
               message('*** Uploading edge attributes ...')   
             }
             
             #set compatibility with igraph!          
             if(is.null(edgeWeight) && !is.null(igraphWeight)) edgeWeight=igraphWeight 
             #correct vectors case only one attr/edge is provided (use dummy values).
             if(nrow(edges)==1){
               edges=rbind(edges,edges)
               if(!is.null(arrowDirection)) arrowDirection=c(arrowDirection,-10)
               if(!is.null(arrowLength))arrowLength=c(arrowLength,-1)
               if(!is.null(arrowAngle))arrowAngle=c(arrowAngle,-1)
               if(!is.null(linkType))linkType=c(linkType,'')
               if(!is.null(edgeWidth)) edgeWidth=c(edgeWidth,-1)
               if(!is.null(edgeColor)) edgeColor=c(edgeColor,'')
               if(!is.null(edgeType)) edgeType=c(edgeType,'')
               if(!is.null(edgeWeight))edgeWeight=c(edgeWeight,0.0)
               if(!is.null(igraphWeight))edgeWeight =c(igraphWeight,0.0)
             }
             
             #arrowDirection                             
             if(!is.null(arrowDirection) && length(arrowDirection)>0){       
               c1=!is.integer(arrowDirection)
               c2=!is.numeric(arrowDirection)
               c3=(sum(arrowDirection< -4)>0 || sum(arrowDirection>4)>0)
               c4=length(arrowDirection)==2 && sum(arrowDirection<0)==1
               if(c1 && c2){
                 warning("NOTE: edge 'direction' must be provided as integers!")
                 arrowDirection=as.numeric(c(-10,-10))
               }
               else if(sum(is.na(arrowDirection))>0){
                 warning("NOTE: invalid edge 'direction' declaration: 'NA' found'!")
                 arrowDirection=as.numeric(c(-10,-10))
               }
               else if(c3 && !c4){
                 warning("NOTE: invalid 'direction' input (options: (+-) 0, 1, 2, 3 or 4)")
                 arrowDirection=as.numeric(c(-10,-10))
               } else { 
                 arrowDirection=as.numeric(as.integer(arrowDirection))           
                 message("** ... edge 'arrow type/direction'")
               }
             } else {
               arrowDirection=as.numeric(c(-10,-10))
             }
             #arrowLength
             if(!is.null(arrowLength) && length(arrowLength)>0){       
               c1=!is.numeric(arrowLength)
               if(c1){
                 warning("NOTE: arrow 'length' must be provided as numerics!")
                 arrowLength =as.numeric(c(-1,-1))
               }
               else if(sum(is.na(arrowLength))>0){
                 warning("NOTE: invalid arrow 'length' declaration: 'NA' found'!")
                 arrowLength=as.numeric(c(-1,-1))
               } else {            
                 message("** ... arrow 'length'") 
                 arrowLength=as.numeric(arrowLength)
               }
             } else {
               arrowLength=as.numeric(c(-1,-1))
             }   
             #arrowAngle
             if(!is.null(arrowAngle) && length(arrowAngle)>0){       
               c1=!is.numeric(arrowAngle)
               if(c1){
                 warning("NOTE: arrow 'angle' must be provided as numerics!")
                 arrowAngle=as.numeric(c(-1,-1))
               }
               else if(sum(is.na(arrowAngle))>0){
                 warning("NOTE: invalid arrow 'angle' declaration: 'NA' found'!")
                 arrowAngle=as.numeric(c(-1,-1))
               } else {            
                 message("** ... arrow 'angle'") 
                 arrowAngle=as.numeric(arrowAngle)
               }
             } else {
               arrowAngle=as.numeric(c(-1,-1))
             }   
             #edgeWeight
             if(!is.null(edgeWeight) && length(edgeWeight)>0 ){       
               c1=!is.numeric(edgeWeight)
               if(c1){
                 warning("NOTE: edge 'weight' must be provided as numerics!")
                 edgeWeight=as.numeric(c(0.0,0.0))
               }
               else if(sum(is.na(edgeWeight))>0){
                 warning("NOTE: invalid edge 'weight' declaration: 'NA' found'!")
                 edgeWeight=as.numeric(c(0.0,0.0))
               } else {            
                 message("** ... edge 'weight'") 
                 edgeWeight=as.numeric(edgeWeight)
               }
             } else {
               edgeWeight=as.numeric(c(0.0,0.0))
             }      
             #edgeWidth
             if(!is.null(edgeWidth) && length(edgeWidth)>0){       
               c1=!is.numeric(edgeWidth)
               c2=sum(edgeWidth<0)>0
               c3=length(edgeWidth)==2 && sum(edgeWidth<0)==1        
               if(c1){
                 warning("NOTE: edge 'width' must be provided as numerics!")
                 edgeWidth=as.numeric(c(-1,-1))
               }
               else if(sum(is.na(edgeWidth))>0){
                 warning("NOTE: invalid edge 'width' declaration: 'NA' found'!")
                 edgeWidth=as.numeric(c(-1,-1))
               }
               else if(c2 && !c3){
                 warning("NOTE: invalid edge 'width' input (options: > 0)")
                 edgeWidth=as.numeric(c(-1,-1))
               } else {            
                 message("** ... edge 'width'") 
                 edgeWidth=as.numeric(edgeWidth)
               }
             } else {
               edgeWidth=as.numeric(c(-1,-1))
             }              
             #edgeColor
             if(!is.null(edgeColor) && length(edgeColor)>0 && is.character(edgeColor)){ 
               edgeColor=colorRampPalette(colors=edgeColor)(length(edgeColor))
               if(sum(is.na(edgeColor))>0){
                 warning("NOTE: invalid edge 'color' declaration: 'NA' found'!")
                 edgeColor=as.character(c('',''))
               } else if(sum(nchar(edgeColor)>9)){
                 warning("NOTE: invalid edge 'color' specification: not 'rgb' space! (ps. alpha not supported)")
                 edgeColor=as.character(c('',''))
               } else {             
                 message("** ... edge 'color'") 
                 if(sum(nchar(edgeColor)>7)) edgeColor=substr(edgeColor,0,7)
               }      
             } else {
               edgeColor=as.character(c('',''))
             }
             #edgeType
             if(!is.null(edgeType) && length(edgeType)>0){       
               c1=!is.character(edgeType)
               if(c1){
                 warning("NOTE: edge 'type' must be provided as character!")
                 edgeType=as.character(c('',''))
               }
               else if(sum(is.na(edgeType))>0){
                 warning("NOTE: invalid edge 'type' declaration: 'NA' found'!")
                 edgeType=as.character(c('',''))
               } else {            
                 message("** ... edge 'type'") 
               }      
             } else {
               edgeType=as.character(c('',''))
             }
             #linkType
             if(!is.null(linkType) && length(linkType)>0){       
               c1=!is.character(linkType)
               if(c1){
                 warning("NOTE: 'linkType' must be provided as character!")
                 linkType=as.character(c('',''))
               }
               else if(sum(is.na(linkType))>0){
                 warning("NOTE: invalid 'linkType' declaration: 'NA' found'!")
                 linkType=as.character(c('',''))
               } else {            
                 message("** ... edge 'link type'") 
               }      
             } else {
               linkType=as.character(c('',''))
             }    
             #Check nesting condition
             nestref=NULL
             isnp=FALSE
             if(isNest){
               nestpack=nestNodes(obj, nodes, nestImage, isAssign, isAnchor, gscale=gscale, 
                                  gcoord=NULL,gatt=gatt, theme=theme, getpack=TRUE, .zoom=zoom, .callchecks=FALSE)
               np1=nestpack$nodes
               np2=nestpack$status
               np3=nestpack$charAtt
               np4=nestpack$numericAtt
               bl1=is.character(np1) && is.character(np2) && is.character(np3) && is.numeric(np4)
               bl2=length(np1)>=2 && length(np2)==3 && length(np3)==6 && length(np4)==8
               if(bl1 && bl2)isnp=TRUE
             }
             if(isnp){
               isnp="true"  	
             } else {
               np1=as.character(c('',''))
               np2=as.character(c('',''))
               np3=as.character(c('',''))
               np4=as.numeric(c(-1,-1)) 
               isnp="false"
               #usa "canal" np2 para passar cc status a eventuais nodos transformados!
               if(ntransform){
                 np2=c(as.character(nestImage), ifelse(isAnchor,'anchor',''), ifelse(isAssign,'assign',''))
               }
             }
             
             #Loading graph...      
             isBrandNew=ifelse(isNest && isAssign,'true','false')
             ntransform=ifelse(ntransform,'true','false')
             parent=ifelse(is.null(parent),'.$NULL', parent)
             parent=as.character(parent)
             numsuppl=c(gcoord[1],gcoord[2])
             charsuppl=c(update,isBrandNew) 
             
             if(igraph::ecount(g)>0 && loadEdges){
               #update, isBrandNew   	
               #Main call to load nodes and edges
               nestref=rederexpresspost(obj@uri, 'RedHandler.updateGraphMap', edges[,1],
                                        edges[,2], arrowDirection, edgeWeight, edgeWidth, edgeColor, edgeType,
                                        arrowLength, arrowAngle, linkType, nodes, coordX, coordY, nodeBend, nodeSize, nodeShape, 
                                        nodeColor, nodeWeight, nodeLineWidth, nodeLineColor, nodeFontSize,
                                        nodeFontColor, nodeAlias, numsuppl, charsuppl, np1, np2, np3, np4, isnp, 
                                        parent, ntransform)      
             } else {
               #Main call to load only nodes
               nestref=rederexpresspost(obj@uri, 'RedHandler.updateNodeMap', 
                                        nodes, coordX, coordY, nodeBend, nodeSize, nodeShape, nodeColor,
                                        nodeWeight, nodeLineWidth, nodeLineColor, nodeFontSize,
                                        nodeFontColor, nodeAlias, numsuppl, charsuppl, np1, np2, np3, np4, isnp, 
                                        parent, ntransform)
             }
             
             invisible( updateGraph(obj) )
             
             #Check nesting ref
             if(isNest){
               if(!is.null(nestref))return(nestref)
             }     
             
           }
)

#Internal function: fix an attribute conflict between igraph/0 versions!
#-------------------------------------------------------------------------------
G<-function(g,att){
  igraph::get.graph.attribute(g,att)
}

#Methods to get node attributes
#-------------------------------------------------------------------------------
setMethod ('getNodes', 'RedPort', 
           function (obj, status="selected", type="node") { 
             if(ping(obj)==0)return(NULL)
             return (xml.rpc(obj@uri, 'RedHandler.getNodes', type, status))
           }
)

#-------------------------------------------------------------------------------
setMethod ('getNodeIDs', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)
             nodes<-xml.rpc (obj@uri, 'RedHandler.getNodeIDs', type, status)
             nodes<-nodes+1 #set index for R!
             return(nodes)
           }
)

#-------------------------------------------------------------------------------
setMethod ('getEdgeIDs', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)
             edges<-xml.rpc(obj@uri, 'RedHandler.getEdgeIDs', type, status)
             edges<-edges+1 #set index for R!
             return(edges)
           }
)

#-------------------------------------------------------------------------------
setMethod ('getSourceEdgeIDs', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)  
             edges<-xml.rpc(obj@uri, 'RedHandler.getSourceEdgeIDs', type, status)
             edges<-edges+1 #set index for R!
             return(edges)
           }
)

#-------------------------------------------------------------------------------
setMethod ('getTargetEdgeIDs', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)
             edges<-xml.rpc(obj@uri, 'RedHandler.getTargetEdgeIDs', type, status)
             edges<-edges+1 #set index for R!
             return (xml.rpc(edges))
           }
)

#Methods to add/delete nodes and manipulate containers and nested objects
#-------------------------------------------------------------------------------
setMethod ('addNodes', 'RedPort', 
           function (obj, nodes) { 
             if(ping(obj)==0)return(invisible())
             nodes=as.character(nodes)
             return (rederexpresspost(obj@uri, 'RedHandler.addNodes', nodes))
           }
)

#-------------------------------------------------------------------------------
setMethod ('deleteNodes', 'RedPort', 
           function (obj, nodes) { 
             if(ping(obj)==0)return(invisible())
             nodes=as.character(nodes)
             return (rederexpresspost(obj@uri, 'RedHandler.deleteNodes', nodes))
           }
)    

#-------------------------------------------------------------------------------
setMethod ('nestNodes', 'RedPort', 
           function (obj, nodes, nestImage ='plain', isAssign=TRUE, isAnchor=FALSE, gscale=40, gcoord=NULL, parent=NULL, 
                     gatt=list(), theme=c('tm0','tm1','tm2','tm3','tm4','tm5','tm6'), getpack=FALSE, .zoom=NULL, .callchecks=TRUE) { 
             if(.callchecks){
               if(ping(obj)==0)return(invisible())
             }
             
             #check loaded igraph
             igraph.check()
             
             #Further checks---------------------------------------------------- 
             if(!is.list(gatt)){
               stop("NOTE: 'gatt' must be a list of graph attributes (e.g. gatt$nestColor, gatt$gscale...)!")
             }
             
             # get zoom for themes
             if(!is.numeric(.zoom)){
               if(getpack){
                 .zoom=100
               } else {
                 .zoom=rederexpresspost(obj@uri,'RedHandler.getZoom')
                 .zoom=as.numeric(.zoom)
                 if(is.nan(.zoom)){
                   .zoom=100
                 } else if(.zoom>100){
                   .zoom=100
                 }
                 if(.zoom>100).zoom=100
               }
             } else {
               if(is.nan(.zoom)).zoom=100
             }
             
             if(is.numeric(theme[1])){
               theme=as.integer(theme[1])
               theme=ifelse(theme>=0 && theme<=6, theme, 0)
             } else if(is.character(theme[1])){
               theme=switch(theme[1], tm1=1, tm2=2, tm3=3,tm4=4, tm5=5,tm6=6, 0)
             } else if(is.list(theme)){
               gatt=theme
               if(is.null(theme$theme)){
                 theme=0
               } else {
                 theme=theme$theme
               }
             } else {
               theme=0
             }
             if(theme==1){
               if(is.null(gatt$nestShape))gatt$nestShape='ROUNDED_RECTANGLE'
               if(is.null(gatt$nestColor))gatt$nestColor='#ffffff'
               if(is.null(gatt$nestLineWidth))gatt$nestLineWidth=4*(100/.zoom)
               if(is.null(gatt$nestFontSize))gatt$nestFontSize=24*(100/.zoom)
               if(is.null(gatt$nestFontX))gatt$nestFontX=5
               if(is.null(gatt$nestFontY))gatt$nestFontY=10.8
               if(is.null(gatt$isAssign))gatt$isAssign=TRUE
             } else if(theme==2){
               if(is.null(gatt$nestShape))gatt$nestShape='ROUNDED_RECTANGLE'
               if(is.null(gatt$nestColor))gatt$nestColor='#ffffff'
               if(is.null(gatt$nestLineWidth))gatt$nestLineWidth=3*(100/.zoom)
               if(is.null(gatt$nestLineColor))gatt$nestLineColor='#000000'
               if(is.null(gatt$nestLineType))gatt$nestLineType='DOTTED'
               if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
               if(is.null(gatt$isAssign))gatt$isAssign=TRUE
             } else if(theme==3){
               if(is.null(gatt$nestShape))gatt$nestShape='ROUNDED_RECTANGLE'
               if(is.null(gatt$nestColor))gatt$nestColor='#ffffff'
               if(is.null(gatt$nestLineWidth))gatt$nestLineWidth=2*(100/.zoom)
               if(is.null(gatt$nestLineColor))gatt$nestLineColor='#000000'
               if(is.null(gatt$nestLineType))gatt$nestLineType='DOTTED'
               if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
               if(is.null(gatt$isAssign))gatt$isAssign=TRUE
             } else if(theme==4){
               if(is.null(gatt$nestImage))gatt$nestImage='transparent'
               if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
             } else if(theme==5){
               if(is.null(gatt$nestImage))gatt$nestImage='hide'
               if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
             } else if(theme==6){
               if(is.null(gatt$nestShape))gatt$nestShape='ROUNDED_RECTANGLE'
               if(is.null(gatt$nestColor))gatt$nestColor='#ffffff'
               if(is.null(gatt$nestLineWidth))gatt$nestLineWidth=4*(100/.zoom)
               if(is.null(gatt$nestFontSize))gatt$nestFontSize=24*(100/.zoom)
               if(is.null(gatt$nestLineColor))gatt$nestLineColor='#000000'
               if(is.null(gatt$nestLineType))gatt$nestLineType='DOTTED'
               if(is.null(gatt$nestFontX))gatt$nestFontX=5
               if(is.null(gatt$nestFontY))gatt$nestFontY=10.8
               if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
             }
             
             node=as.character(nodes) 	
             status1='plain'
             if(is.character(gatt$nestImage)){
               if(gatt$nestImage=='plain' || gatt$nestImage=='hide' || gatt$nestImage=='transparent')status1=gatt$nestImage[1]
             } else {
               if(is.character(nestImage)){
                 if(nestImage=='plain' || nestImage=='hide' || nestImage=='transparent')status1=nestImage[1]
               }
             } 	
             status2='default'
             if(is.logical(gatt$isAnchor)){
               if(gatt$isAnchor)status2='anchor'
             } else {
               if(is.logical(isAnchor)){
                 if(isAnchor)status2='anchor'
               }
             }
             status3='default'
             if(is.logical(gatt$isAssign)){
               if(gatt$isAssign) status3='assign'
             } else {
               if(is.logical(isAssign)){
                 if(isAssign) status3='assign'
               }
             }  	  	
             
             if(is.character(gatt$parent) && length(gatt$parent)>0){
               parent=gatt$parent[1]
               nodes=paste(nodes,".$", parent,sep="")
             } else {
               if(is.character(parent) && length(parent)>0){
                 parent=parent[1]
                 nodes=paste(nodes,".$", parent,sep="")
               }
             }
             
             #Get string attributes 
             charAtt=rep("<$$>",6)
             if(!is.null(gatt) && !getpack){
               message('*** Uploading nest attributes ...')
             }
             #Nest aliases
             if(is.character(gatt$nestAlias) && length(gatt$nestAlias)>0){
               if(!getpack)message("** ... nest 'alias'")
               charAtt[1]=gatt$nestAlias[1]
             } else if(!is.null(gatt$nestAlias)){
               warning("NOTE: nest 'alias' must be provided as character!")
             }
             #Nest shape
             if(is.character(gatt$nestShape)){
               gatt$nestShape = gatt$nestShape[1]
               if(is.na(gatt$nestShape)){
                 warning("NOTE: invalid nest 'shape' declaration: 'NA' found'!")
               } else {            
                 if(!getpack)message("** ... nest 'shape'") 
                 charAtt[2]=gatt$nestShape
               }      
             } else if(!is.null(gatt$nestShape)){
               warning("NOTE: nest 'shape' must be provided as character!")
             } 	
             #Nest line type
             if(is.character(gatt$nestLineType)){
               gatt$nestLineType = gatt$nestLineType[1]
               if(is.na(gatt$nestLineType)){
                 warning("NOTE: invalid nest 'line type' declaration: 'NA' found'!")
               } else {            
                 if(!getpack)message("** ... nest 'line type'") 
                 charAtt[3]=gatt$nestLineType
               }      
             } else if(!is.null(gatt$nestLineType)){ 
               warning("NOTE: nest 'line type' must be provided as character!")
             }	
             #Nest color
             if(is.character(gatt$nestColor)){
               ncol=gatt$nestColor
               gatt$nestColor=colorRampPalette(colors=c(ncol,ncol))(1)
               if(is.na(gatt$nestColor)){
                 warning("NOTE: invalid nest 'color' declaration: 'NA' found'!")
               } else if(nchar(gatt$nestColor)>9){
                 warning("NOTE: invalid nest 'color' specification: not 'rgb' space! (ps. alpha not supported)")
               } else {            
                 if(nchar(gatt$nestColor)>7) gatt$nestColor=substr(gatt$nestColor,0,7)
                 if(!getpack)message("** ... nest 'color'")
                 charAtt[4]=gatt$nestColor
               }      
             } else if(!is.null(gatt$nestColor)){ 
               warning("NOTE: nest 'color' must be provided as character (hexadecimal)!")
             }
             #Nest line color
             if(is.character(gatt$nestLineColor)){
               ncol=gatt$nestLineColor
               gatt$nestLineColor=colorRampPalette(colors=c(ncol, ncol))(1)
               if(is.na(gatt$nestLineColor)){
                 warning("NOTE: invalid nest 'line color' declaration: 'NA' found'!")
               } else if(nchar(gatt$nestLineColor)>9){
                 warning("NOTE: invalid nest 'line color' specification: not 'rgb' space! (ps. alpha not supported)")
               } else {            
                 if(!getpack)message("** ... nest 'line color'") 
                 if(nchar(gatt$nestLineColor)>7)gatt$nestLineColor=substr(gatt$nestLineColor,0,7)
                 charAtt[5]=gatt$nestLineColor
               }      
             } else if(!is.null(gatt$nestLineColor)){ 
               warning("NOTE: nest 'line color' must be provided as character (hexadecimal)!")
             }
             #Nest font color
             if(is.character(gatt$nestFontColor)){
               ncol=gatt$nestFontColor
               gatt$nestFontColor = colorRampPalette(colors=c(ncol,ncol))(1)
               if(is.na(gatt$nestFontColor)){
                 warning("NOTE: invalid nest 'font color' declaration: 'NA' found'!")
               } else if(nchar(gatt$nestFontColor)>9){
                 warning("NOTE: invalid nest 'font color' specification: not 'rgb' space! (ps. alpha not supported)")
               } else {            
                 if(!getpack)message("** ... nest 'line color'") 
                 if(nchar(gatt$nestFontColor)>7) gatt$nestFontColor=substr(gatt$nestFontColor,0,7) 
                 charAtt[6]=gatt$nestFontColor
               }      
             } else if(!is.null(gatt$nestFontColor)){ 
               warning("NOTE: nest 'font color' must be provided as character (hexadecimal)!")
             }
             
             #Get numerics attributes   		 	
             numericAtt=c(-8,-8,-1,-1,-1,-1, 909192, 909192)
             #Nest font coords.
             if(is.numeric(gatt$nestFontX) && is.numeric(gatt$nestFontY)){
               gatt$nestFontX = gatt$nestFontX[1]
               gatt$nestFontY = gatt$nestFontY[1]
               if(is.na(gatt$nestFontX) || is.na(gatt$nestFontY) ){
                 warning("NOTE: invalid nest coords. declaration: 'NA' found'!")
               } else {
                 if(!getpack)message("** ... nest font 'coords'")
                 numericAtt[1]=gatt$nestFontX
                 numericAtt[2]=gatt$nestFontY
               }      
             }  
             #Nest font size.
             if(is.numeric(gatt$nestFontSize)){
               gatt$nestFontSize = gatt$nestFontSize[1]
               if(is.na(gatt$nestFontSize) ){
                 warning("NOTE: invalid nest 'font size' declaration: 'NA' found'!")              
               } else if(gatt$nestFontSize<0){
                 warning("NOTE: invalid nest 'font size' declaration (options: >= 0)")
               } else {
                 if(!getpack)message("** ... nest font 'size'") 
                 numericAtt[3]=gatt$nestFontSize
               }
             }	  		
             #Nest line width
             if(is.numeric(gatt$nestLineWidth)){
               gatt$nestLineWidth = gatt$nestLineWidth[1]
               if(is.na(gatt$nestLineWidth) ){
                 warning("NOTE: invalid nest 'line width' declaration: 'NA' found'!")              
               } else if(gatt$nestLineWidth<0){
                 warning("NOTE: invalid nest 'line width' declaration (options: >= 0)")
               } else {
                 if(!getpack)message("** ... nest 'line width'") 
                 numericAtt[4]=gatt$nestLineWidth
               }
             }
             #Nest size.
             if(is.numeric(gatt$nestSize)){
               gatt$nestSize = gatt$nestSize[1]
               if(is.na(gatt$nestSize) ){
                 warning("NOTE: invalid nest 'size' declaration: 'NA' found'!")              
               } else if(gatt$nestSize<0){
                 warning("NOTE: invalid nest 'size' declaration (options: >= 0)")
               } else {
                 if(!getpack)message("** ... nest 'size'") 
                 numericAtt[5]=gatt$nestSize
               }
             }	
             #Nest gscale
             if(!is.null(gscale) && is.null(gatt$nestSize)){	
               if(is.numeric(gscale)){
                 if(gscale>=1)numericAtt[6]=gscale[1]	
               } else if(is.numeric(gatt$gscale)){
                 gatt$gscale = gatt$gscale[1]
                 if(is.na(gatt$gscale) ){
                   warning("NOTE: invalid nest 'gscale' declaration: 'NA' found'!")              
                 } else if(gatt$gscale <0){
                   warning("NOTE: invalid nest 'gscale' declaration (options: > 0)")
                 } else {
                   if(!getpack)message("** ... nest 'gscale'") 
                   numericAtt[6]=gatt$gscale
                 }
               } 	
             }
             #Nest gcoord	
             if(!is.null(gcoord)){
               if(is.numeric(gcoord) && length(gcoord)==2){
                 numericAtt[7]= gcoord[1]
                 numericAtt[8]= gcoord[2]	
               } else if(is.numeric(gatt$gcoord) && length(gcoord)==2){
                 if(sum(is.na(gatt$gcoord))>0 ){
                   warning("NOTE: invalid nest 'gcoord' declaration: 'NA' found'!")              
                 } else {
                   if(!getpack)message("** ... nest 'gcoord'") 
                   numericAtt[7]=gatt$gcoord[1]
                   numericAtt[8]=gatt$gcoord[2]
                 }
               } 	
             }
             if(getpack){
               nestpack=list(nodes=nodes, status=c(status1,status2,status3), charAtt=charAtt, 
                             numericAtt=numericAtt, nestmap=length(nodes))
               return(nestpack)
             } else {
               ref=rederexpresspost(obj@uri, 'RedHandler.nestexpress', nodes, c(status1,status2,status3), 
                                    charAtt, numericAtt )      
               invisible( updateGraph(obj) )
               return(ref)		
             }   
             
           }
)

#-------------------------------------------------------------------------------
setMethod ('updateContainerSize', 'RedPort', 
           function (obj) { 
             if(ping(obj)==0)return(invisible())
             return (rederexpresspost(obj@uri, 'RedHandler.updateContainerSize'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('mergeOutEdges', 'RedPort', 
           function (obj,rescale=TRUE, lb=NULL, ub=NULL, nlev=1) { 
             if(ping(obj)==0)return(invisible())
             if(!is.numeric(nlev) && !is.integer(nlev))nlev=1
             if(is.nan(nlev))nlev=1
             if(is.logical(rescale)){
               rescale=ifelse(rescale,'true','false')
             } else {
               rescale='true'
             }
             lb=lb[1]
             ub=ub[1]
             if(!is.numeric(lb) || !is.numeric(ub)){
               lb=0
               ub=0
             } else {
               lb=max(lb,0)
               ub=max(ub,0)
             }
             for(i in 1:nlev)res=rederexpresspost(obj@uri, 'RedHandler.mergeContainerOutEdges', rescale, lb, ub)
             res
           }
)

#-------------------------------------------------------------------------------
setMethod ('getContainerComponets', 'RedPort', 
           function (obj, container) { 
             if(ping(obj)==0)return(invisible())
             container=as.character(container)
             return (xml.rpc(obj@uri, 'RedHandler.getContainerComponets', container))
           }
)

#-------------------------------------------------------------------------------
setMethod ('mergeNodes', 'RedPort', 
           function (obj, nodes) { 
             if(ping(obj)==0)return(invisible())
             node=as.character(nodes)
             return (rederexpresspost(obj@uri, 'RedHandler.mergeNodes', nodes))
           }
)

#Methods to get edge attributes
#-------------------------------------------------------------------------------
setMethod ('getEdges', 'RedPort', 
           function (obj, status="selected", type="node") { 
             if(ping(obj)==0)return(NULL)
             return (xml.rpc(obj@uri, 'RedHandler.getEdges', type, status))
           }
)

#Methods to set edge attributes    
#-------------------------------------------------------------------------------
setMethod ('setArrowDirection', 'RedPort', 
           function (obj, nodeA, nodeB, direction) { 
             if(ping(obj)==0)return(invisible())
             c1=!is.integer(direction)
             c2=!is.numeric(direction)
             if(c1 && c2){
               stop("Arrow 'direction' must be provided as integers!")
             }
             if(sum(is.na(direction))>0){
               stop("Invalid arrow 'direction' declaration: 'NA' found'!")
             }
             if(sum(direction<0)>0 || sum(direction>3)>0){
               stop("Invalid arrow 'direction' declaration (options: 0, 1, 2 or 3)")
             }
             nodeA=as.character(nodeA)
             nodeB=as.character(nodeB)  
             direction=as.numeric(direction)  
             return (rederexpresspost(obj@uri, 'RedHandler.setArrowDirection', 
                             nodeA, nodeB, direction))
           }
)

#Methods to add/delete edges
#-------------------------------------------------------------------------------
setMethod ('addEdges', 'RedPort', 
           function (obj, edges) {
             if(ping(obj)==0)return(invisible())
             if(is.list(edges) || is.data.frame(edges)){
               stop("Edges must be provided as 'array' or 'matrix' objects!")
             }      
             c1=is.matrix(edges) && ncol(edges)==2
             c2=is.vector(edges) || is.array(edges)
             if(c1){
               x <- matrix(data = NA, nrow = prod(dim(edges)), ncol = 1)
               j=1
               for(i in 1:nrow(edges)){
                 x[j]=edges[i,1]
                 j=j+1
                 x[j]=edges[i,2]
                 j=j+1
               }
               edges=x
             }
             else if(!c2) {
               stop("Edges must be provided as 'array' or 'matrix' objects!")
             }
             if(!is.character(edges)){
               edges=as.character(edges)
             }     
             return (rederexpresspost (obj@uri, 'RedHandler.addEdges', edges))
           }
)

#-------------------------------------------------------------------------------
setMethod ('deleteEdges', 'RedPort', 
           function (obj, edges) {  
             if(ping(obj)==0)return(invisible())
             if(is.list(edges) || is.data.frame(edges)){
               stop("Edges must be provided as 'array' or 'matrix' objects!")
             }      
             c1=is.matrix(edges) && ncol(edges)==2
             c2=is.vector(edges) || is.array(edges)
             if(c1){
               x <- matrix(data = NA, nrow = prod(dim(edges)), ncol = 1)
               j=1
               for(i in 1:nrow(edges)){
                 x[j]=edges[i,1]
                 j=j+1
                 x[j]=edges[i,2]
                 j=j+1
               }
               edges=x
             } 
             else if(!c2) {
               stop("Edges must be provided as 'array' or 'matrix' objects!")
             }
             if(!is.character(edges)){
               edges=as.character(edges)
             }     
             return (rederexpresspost(obj@uri, 'RedHandler.deleteEdges', edges))
           }
)

#-------------------------------------------------------------------------------
setMethod ('addEdgeBetweenContainers', 'RedPort', 
           function (obj, containerA, containerB) { 
             if(ping(obj)==0)return(invisible())
             containerA=as.character(containerA)
             containerB=as.character(containerB)   
             return (rederexpresspost(obj@uri, 'RedHandler.addEdgeBetweenContainers', 
                              containerA, containerB))
           }
)

#Further methods to manipulate edges and nodes
#-------------------------------------------------------------------------------
setMethod ('selectEdges', 'RedPort', 
           function (obj, nodeA, nodeB) {   
             if(ping(obj)==0)return(invisible())
             nodeA=as.character(nodeA)
             nodeB=as.character(nodeB)
             deSelectEdges(obj)  #deselect all edges previously to the call!    
             invisible(rederexpresspost(obj@uri, 'RedHandler.selectEdges', nodeA, nodeB))
           }
)

#-------------------------------------------------------------------------------
setMethod ('selectNodes', 'RedPort', 
           function (obj, nodes, nt=NULL) { 
             if(ping(obj)==0)return(invisible())
             nodes=as.character(nodes)
             nt=ifelse(is.null(nt[1]),"",as.character(nt)[1])
             deSelectNodes(obj)#deselect all nodes previously to the call!
             invisible (rederexpresspost(obj@uri, 'RedHandler.selectNodes', nodes, nt))
           }
)

#-------------------------------------------------------------------------------
setMethod ('selectAllEdges', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.selectAllEdges'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('selectAllNodes', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.selectAllNodes'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('selectGraph', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.selectGraph'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('deSelectEdges', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.deSelectEdges'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('deSelectNodes', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.deSelectNodes'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('deSelectGraph', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.deSelectGraph'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('deleteSelectedEdges', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.deleteSelectedEdges'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('deleteSelectedNodes', 'RedPort', 
           function (obj) {
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.deleteSelectedNodes'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('isDynamicsActive', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             return (rederexpresspost(obj@uri, 'RedHandler.isDynamicsActive'))
           }
)

#-------------------------------------------------------------------------------
setMethod ('relax', 'RedPort', 
           function (obj,p1=100,p2=100,p3=100,p4=100,p5=100,p6=100,p7=10,p8=10,ps=FALSE) {
             if(ping(obj)==0)return(invisible())
             if(!is.numeric(p1) || length(p1)==0)p1=100;p1=p1[1]
             if(!is.numeric(p2) || length(p2)==0)p2=100;p2=p2[1]
             if(!is.numeric(p3) || length(p3)==0)p3=100;p3=p3[1] 		  		
             if(!is.numeric(p4) || length(p4)==0)p4=100;p4=p4[1]
             if(!is.numeric(p5) || length(p5)==0)p5=100;p5=p5[1]  		
             if(!is.numeric(p6) || length(p6)==0)p6=100;p6=p6[1]  		
             if(!is.numeric(p7) || length(p7)==0)p7=10;p7=p7[1]
             if(!is.numeric(p8) || length(p8)==0)p8=10;p8=p8[1]
             if(!is.logical(ps))ps=FALSE
             ps=ifelse(ps[1],1,0)
             return (rederexpresspost(obj@uri, 'RedHandler.setDynamics',p1,p2,p3,p4,p5,p6,p7,p8,ps))
           }
)

#Methods to build RedeR plugins
#-------------------------------------------------------------------------------
setMethod ('deletePlugin', 'RedPort', 
           function (obj, pluginName) { 
             if(ping(obj)==0)return(invisible())
             pluginName=as.character(pluginName)      
             invisible (rederexpresspost(obj@uri, 'RedHandler.deletePlugin', pluginName))
           }
) 

#-------------------------------------------------------------------------------
setMethod ('updatePlugins', 'RedPort', 
           function (obj) {   
             if(ping(obj)==0)return(invisible())
             invisible (rederexpresspost(obj@uri, 'RedHandler.updatePlugins'))
           }
)    

#-------------------------------------------------------------------------------
setMethod ('submitPlugin', 'RedPort', 
           function (obj, plugin) {
             if(ping(obj)==0)return(invisible())
             if(class(plugin)!="PluginBuilder"){
               print("Not a 'PluginBuilder' class!")
               return(NULL)     	
             }	
             #Pack methods
             for(i in 1:length(plugin@allMethods)){
               nm=names(plugin@allMethods)[i]
               mt=plugin@allMethods[[i]]
               mt=pluginParser(nm,mt,args=FALSE)
               plugin@allMethods[[i]]=mt
             }
             #Pack addons
             if(is.function(plugin@allAddons[[1]])){
               for(i in 1:length(plugin@allAddons)){
                 nm=names(plugin@allAddons)[i]
                 ft=plugin@allAddons[[i]]
                 ft=pluginParser(nm,ft,args=TRUE)
                 plugin@allAddons[[i]]=ft
               }
             }
             pluginTitle   = as.character(plugin@title)
             pluginMethods = as.character(c(plugin@allMethods,''))
             pluginAddons  = as.character(c(plugin@allAddons,''))  
             #Submit plugin to RedeR
             invisible (xml.rpc(obj@uri, 'RedHandler.buildPlugin', pluginTitle, 
                                 pluginMethods, pluginAddons))
           }
)

#Map hclust to RedeR app
#-------------------------------------------------------------------------------
setMethod ('nesthc', 'RedPort', 
           function(obj, hc, cutlevel=2, metric=c("rootdist","leafdist","height"), nmemb=2, nlev=2, grid=c(2,3),
                    gridScale=75, gscale=c(30,75,45), isAssign=FALSE, isAnchor=TRUE, theme='tm6', nlinewidth=10, nfontsz=60,
                    col=NULL, plothc=TRUE, plotbox=TRUE, cex=0.6, xlab="Nodes", ylab="Height", main="Hierarchical Network",
                    alpha = 0.95, pv = "au", type = "geq", max.only = TRUE, labels=NULL, lwd=1,...){
             
             if(ping(obj)==0)return(invisible())
             
             #check loaded igraph
             igraph.check()
             
             #check hclust object-----------------------------------------------
             hctype="hclust"
             if(class(hc)=="pvclust"){
               pvedges=pvpick(x=hc, alpha=alpha, pv=pv, type=type, max.only=max.only)$edges
               hc=hc$hclust
               hctype="pvclust"
             } else if(class(hc)!="hclust"){
               stop("Not a hclust object!")
             }
             hc$height=hc$height/max(hc$height)
             
             #check args--------------------------------------------------------
             
             if(is.character(metric)){
               tp=switch(metric[1], rootdist=1, leafdist=2, height=3, 1)
             } else if(is.numeric(metric)){
               tp=metric[1]
               tp=ifelse(tp>0 && tp<=3,tp, 1)
             } else {
               tp=1
             }
             metric=c("rootdist","leafdist","height")[tp]
             nmemb=max(2,nmemb)
             
             #further checks----------------------------------------------------
             if(!is.numeric(cutlevel))cutlevel=2
             cutlevel=cutlevel[1]
             if(!is.numeric(nmemb))nmemb=2
             nmemb=max(2,nmemb[1])
             if(!is.numeric(grid))grid=c(2,3)
             if(length(grid)==1)grid=c(2,3)
             gridScale=gridScale[1]
             if(!is.numeric(gridScale))gridScale=75
             if(gridScale<1)gridScale=1
             s1=!is.numeric(gscale)
             s2=is.null(gscale)
             s3=sum(is.na(gscale))>0
             if(s1 || s2 || s3){
               gscale = c(30,75,45)
             }		
             
             if(is.numeric(grid) && length(grid)>1){
               gridRows=ifelse(!is.na(grid[1]),grid[1],2)
               gridCols=ifelse(!is.na(grid[2]),grid[2],3)
             } else {
               gridRows=2
               gridCols=3			
             }
             
             treemap=function(hc){
               A=hc$merge
               B=list()
               C=list()
               D=list()
               E=list()
               nest=list()
               if(is.null(hc$labels))hc$labels=as.character(sort(hc$order))
               for(i in 1:nrow(A)){
                 ai=A[i,1]      
                 if(ai < 0){
                   B[[i]]= -ai
                   C[[i]]=1
                 } else {
                   B[[i]]=B[[ai]]      
                   C[[i]]=C[[ai]]+1 
                 }
                 ai=A[i,2]
                 if(ai < 0){
                   B[[i]]=sort(c(B[[i]],-ai))
                 } else {
                   B[[i]]=sort(c(B[[i]],B[[ai]]))
                   C[[i]]=max(C[[i]],C[[ai]]+1)
                 }
                 p=match(i,A)
                 D[[i]]=ifelse(p>nrow(A),p-nrow(A),p)
                 nest[[i]]=hc$labels[B[[i]]]
               }
               D[[nrow(A)]]=nrow(A)+1
               for(i in 1:nrow(A)){
                 step=1
                 find=D[[i]]	
                 while(find<D[[nrow(A)]]){
                   find=D[[find]]
                   step=step+1
                 }
                 E[[i]]=step
               }
               # get dendogram xy position
               nn=nrow(A) + 1
               xaxis=c()
               yaxis=hc$height
               tp=rep(0,2)
               mm=match(1:length(hc$order),hc$order)
               for(i in 1:(nn-1)) {
                 ai=A[i,1]
                 if(ai < 0){
                   tp[1]=mm[-ai]
                 } else {
                   tp[1]=xaxis[ai]
                 }
                 ai=A[i,2]
                 if(ai < 0){
                   tp[2]=mm[-ai]
                 } else {
                   tp[2]=xaxis[ai]
                 }
                 xaxis[i]=mean(tp)
               }
               xyaxis=data.frame(xaxis=xaxis,yaxis=yaxis,stringsAsFactors=FALSE)
               # return res
               C=as.numeric(C)
               D=as.numeric(D)
               E=as.numeric(E)
               N=hc$merge>0
               N=N[,1]+N[,2]
               return(list(nest=nest,compids=B,labels=hc$labels,parent=D,leafdist=C,
                           rootdist=E,height=hc$height,nnest=N, xyaxis=xyaxis))
             }
             
             # map tree
             tm=treemap(hc)
             nn=length(tm$nest)
             
             #compute dist to root and select nested series	
             if(hctype=="pvclust"){
               selec=rep(0, nn)
               selec[pvedges]=1
               tp=tm$parent
               td=selec[tp]
               td[is.na(td)]=0
               nrtdist=td
               while(sum(td)>0){
                 tp=tm$parent[tp]
                 td=selec[tp]
                 td[is.na(td)]=0
                 nrtdist=nrtdist+td
               }
               selec=nrtdist+selec
               tp=rep(0, nn)
               tp[pvedges]=1
               selec[tp==0]=0
               selec[selec>nlev]=0
             } else {
               val=tm[[metric]]
               if(metric=="rootdist"){
                 selec=as.numeric(val>=cutlevel)
               } else {
                 selec=as.numeric(val<=cutlevel)
               }
               tp=tm$parent
               td=selec[tp]
               td[is.na(td)]=0
               nrtdist=td
               while(sum(td)>0){
                 tp=tm$parent[tp]
                 td=selec[tp]
                 td[is.na(td)]=0
                 nrtdist=nrtdist+td
               }
               selec=nrtdist+selec	
               selec[selec>nlev]=0		
             }
             tedges=c(1:length(selec))[selec!=0]
             # remap parents (e.g. after removing tree nodes via pvclust)
             tp=tm$parent
             td=tp[tedges]
             tdd=tp[tedges]
             idx=!td%in%tedges
             while(sum(idx)>0){
               td[idx]=tp[td[idx]]
               idx=!td%in%tedges
               idx[is.na(td)]=FALSE
               #td[is.na(td)]=tp[tedges][is.na(td)]	
               tdd[is.na(td)]=tp[tedges][is.na(td)]				
             }
             newparent=rep(0,nn)
             newparent[tedges]=tdd
             newnnest=rep(0,length(newparent))
             for(i in 1:length(newnnest)){
               newnnest[i]=sum(newparent==i)
             }
             nestcount=sum(selec!=0)
             
             #estimate grid
             if(!is.numeric(gridRows)){gridRows=NULL}else{gridRows=gridRows[1]}
             zoom=NULL
             if(is.numeric(gridScale)){
               gridScale=gridScale[1]
               #set gridScale to zoom
               if(gridScale>100)gridScale=100
               if(gridScale<0)gridScale=0
               zoom=100-gridScale   	
             }
             #get a basic layout just for graphs' first view
             if(is.null(gridRows)){
               gbasic=igraph::graph.empty(n=nestcount,directed=FALSE)  
               layout=igraph::layout.norm(igraph::layout.circle(gbasic), xmin = 25, xmax=75, ymin=25, ymax=75) 
             } else {
               bin=100/(gridCols+1)
               xgrid=c(1:gridCols)*bin
               bin=100/(gridRows +1)
               estimatedRows=as.integer(nestcount/gridCols)+1
               ygrid=c(1: estimatedRows)*bin
               layout=cbind(x=xgrid,y=ygrid[1])
               if(estimatedRows>1){
                 for(i in 2: estimatedRows){
                   lt=cbind(x=xgrid,y=ygrid[i])
                   layout=rbind(layout, lt)
                 }
               }
             }	
             
             # ' update="default" ' forces keeping old node coords and not to add new containers!
             if(is.null(update) || !is.character(update))update=NULL
             # internal function (locks DragAndZoon interactivity while sending the subgraph list to the data bank)
             invisible(rederexpresspost (obj@uri,'RedHandler.lockDragAndZoom'))
             if(!is.null(zoom))invisible( rederexpresspost (obj@uri, 'RedHandler.setZoom',zoom) )
             checknd=getGraph(obj, status="all", attribs="minimal")
             checknd=V(checknd)$name        
             
             # set lab colors
             if(is.null(col)){
               col=c("darkred","red","orange","darkgreen","cyan","blue","darkblue")
               col=colorRampPalette(colors=col)(nestcount)
             } else {
               if(length(col)<2)col=c(col,col)
               col=colorRampPalette(colors=col)(nestcount)
             }
             
             #add tree ----------------------------------------------------------
             gs0=gscale[1]
             gs1=gscale[2]
             gs2=gscale[3]
             gc1=c(58,58)
             gc2=c(70,70)
             gc3=c(30,30)
             stats=data.frame()
             nestpack=list()
             nestcount=1
             nid=rep(NA,nn)
             k=1
             for(i in rev(tedges)){			
               nodes=tm$nest[[i]]
               if(sum(nodes%in%checknd)>=nmemb){
                 pt=newparent[i]
                 if(!is.na(nid[pt]) && isAssign){
                   nodes=paste(nodes,".$", nid[pt],sep="")
                 }
                 if(is.na(nid[pt])){	
                   gs=gs0
                   gc=c(layout[k,1],layout[k,2])
                   k=k+1				
                 } else if(newnnest[pt]<2){
                   gs=gs1
                   gc=gc1  				
                 } else if(newnnest[pt]==2){
                   gs=gs2
                   gc=gc2
                   newnnest[pt]=3
                 } else if(newnnest[pt]==3) {
                   gs=gs2
                   gc=gc3  				
                 }
                 #scale nest.line.width by n. levels (just to get a better image!) 
                 gatt=list()
                 gatt$nestLineWidth=(nlinewidth/2)+((nlinewidth/2)*(1/max(1,selec[i])))
                 gatt$nestFontSize=(nfontsz/2)+((nfontsz/2)*(1/max(1,selec[i])))
                 #gatt$nestFontColor=col[tedges==i]
                 gatt$nestLineColor=col[tedges==i]
                 gatt$nestAlias=paste("NT",nestcount,sep="")
                 nid[i]=gatt$nestAlias
                 #send nested nodes!
                 if(isAssign){
                   nestNodes(obj, nodes, nestImage='plain', isAssign=isAssign, isAnchor=isAnchor, 
                             gscale=gs, gcoord=gc, gatt=gatt, theme=theme)
                 } else {
                   nestpack[[nestcount]]=nestNodes(obj, nodes, nestImage='plain', isAssign=isAssign, 
                                                   isAnchor=isAnchor, gscale=gs, gcoord=gc, gatt=gatt, theme=theme, 
                                                   getpack=TRUE, .callchecks=FALSE)	 
                   nestcount=nestcount+1
                 }
                 stats=rbind(stats, data.frame(nid=nid[i],did=i,nest.size=length(nodes),
                                               leafdist=tm$leafdist[i],root.dist=tm$rootdist[i],height=tm$height[i],
                                               nestroot.dist=selec[i],stringsAsFactors=FALSE))	    			
               } else {
                 selec[i]=0 #remove container due to n. members<threashold!!
               }
             }
             if(length(nestpack)>0){
               nodes=nestpack[[1]]$nodes
               status=nestpack[[1]]$status
               charAtt=nestpack[[1]]$charAtt
               numAtt=nestpack[[1]]$numericAtt
               nestmap=nestpack[[1]]$nestmap 
               if(length(nestpack)>1){ 	  	
                 for(i in 2:length(nestpack)){
                   nodes=c(nodes,nestpack[[i]]$nodes)
                   status=c(status,nestpack[[i]]$status)
                   charAtt=c(charAtt,nestpack[[i]]$charAtt)
                   numAtt=c(numAtt,nestpack[[i]]$numericAtt)
                   nestmap=c(nestmap,nestpack[[i]]$nestmap)    	  		
                 }
               }
               message('*** Uploading nest hclust...')
               rederexpresspost(obj@uri,'RedHandler.nestpackexpress', nodes, status, charAtt, numAtt, nestmap)
               invisible( updateGraph(obj) )
             }
             
             #Internal function (unlocks DragAndZoon interactivity after send subgraph list)
             invisible(rederexpresspost (obj@uri,'RedHandler.unLockDragAndZoom'))
             
             if(plothc){
               plot(x=hc, xlab=xlab, ylab=ylab, cex=cex, sub="", main=main, labels=labels, lwd=lwd,...)
               if(sum(selec>0)>0){
                 usr=par()$usr; wid=usr[4]-usr[3]
                 cex=cex*2
                 cex1=cex-(cex/2)
                 cex2=cex/2
                 cex=cex1+cex2*1/stats$nestroot.dist
                 text(x=tm$xyaxis[stats$did,1], y=tm$xyaxis[stats$did,2] + 0.02 * wid, 
                      labels=stats$nid, pos=4, offset=.3, col=rev(col), cex=cex, lwd=lwd,...)
                 if(plotbox){
                   comp=tm$compids
                   order=hc$order
                   xwd=usr[2]-usr[1]
                   ywd=usr[4]-usr[3]
                   cin=par()$cin	
                   for(i in rev(tedges)){
                     if(selec[i]>0){ #em funcao do corte para n.nodos
                       mi=comp[[i]]
                       ma=match(mi, order)
                       xl=min(ma)
                       xr=max(ma)
                       yt=hc$height[i]
                       yb=usr[3]
                       mx=xwd / length(comp) / 3
                       my=ywd / 200
                       nd=stats[stats$did==i,"nestroot.dist"]
                       rect(xleft=xl - mx, ybottom=yb + my, xright=xr + mx, ytop=yt + my, 
                            border=col[tedges==i], shade=NULL, lwd=lwd*2*(1/nd),...)
                     }
                   }
                 }
               }
             }
           }
)

#-------------------------------------------------------------------------------
setMethod ('addLegend.color', 'RedPort', 
           function (obj, colvec, type="node", labvec=NULL, position=NULL, dxborder=NULL, dyborder=NULL, vertical=NULL, 
                     ftsize=NULL, title=NULL, dxtitle=NULL, size=NULL, bend=NULL) {
             
             if(ping(obj)==0)return(invisible())
             
             #check loaded igraph
             igraph.check()
             
             # checks--------------------------------------------------------- 
             
             if(!is.character(type))type="nodecolor"
             type=switch(type, node="nodecolor", edge="edgecolor", "nodecolor")
             
             #Check if igraph object------------------------------------------
             if(igraph::is.igraph(colvec)){
               if(type=="nodecolor"){
                 if(!is.null(G(colvec,"legNodeColor")$scale)){
                   if(!is.null(G(colvec,"legNodeColor")$legend) && is.null(labvec)){
                     labvec=G(colvec,"legNodeColor")$legend
                     if(is.null(title))title=G(colvec,"legNodeColor")$title
                   }
                   colvec=G(colvec,"legNodeColor")$scale
                 } else {
                   stop("NOTE: there is no valid 'legNodeColor' legend information for this igraph object!!")
                 }
               } else {
                 if(!is.null(G(colvec,"legEdgeColor")$scale)){
                   if(!is.null(G(colvec,"legEdgeColor")$legend) && is.null(labvec)){
                     labvec=G(colvec,"legEdgeColor")$legend
                     if(is.null(title))title=G(colvec,"legEdgeColor")$title
                   }
                   colvec=G(colvec,"legEdgeColor")$scale
                 } else {
                   stop("NOTE: there is no valid 'legEdgeColor' legend information for this igraph object!!")
                 }			
               }
               
             }
             
             # color vec
             if(!is.null(colvec) && length(colvec)>1){    
               colvec=colorRampPalette(colors=colvec)(length(colvec))   
               c1=!is.character(colvec)
               if(c1){
                 stop("NOTE: 'color' must be provided as a vector (hexadecimal or valid R colors)!")
               } else {
                 colvec=colorRampPalette(colors=colvec)(length(colvec))
               }
               if(sum(is.na(colvec))>0){
                 stop("NOTE: invalid node 'color' declaration: 'NA' found'!")
               } else if(sum(nchar(colvec)>9) ){
                 stop("NOTE: invalid node 'color' specification: not 'rgb' space! (alpha not supported)")
               } else {            
                 if(sum(nchar(colvec)>7))colvec=substr(colvec,0,7)
               }
             } else if(is.null(colvec)){
               type="null"
               colvec=c("null","null")
               labvec=NULL	
             } else {
               stop("NOTE: 'color' must be provided as a vector (hexadecimal or valid R colors)!")
             }
             
             # label vec
             if(is.null(labvec))labvec=letters[c(1:length(colvec))]
             if(length(labvec)!=length(colvec)){
               stop("NOTE: 'labvec' and 'colvec' must have the same length!")
             }
             labvec=as.character(labvec)
             # further args
             if(!is.null(position) && !is.character(position))position=NULL
             position=position[1]
             if(!is.null(dxborder) && !is.numeric(dxborder))dxborder=NULL
             dxborder=dxborder[1]
             if(!is.null(dyborder) && !is.numeric(dyborder))dyborder=NULL
             dyborder=dyborder[1]
             if(!is.null(vertical) && !is.logical(vertical))vertical=NULL
             vertical=vertical[1]
             if(!is.null(ftsize) && !is.numeric(ftsize))ftsize=NULL
             ftsize=ftsize[1]
             if(!is.null(title) && !is.character(title))title=NULL
             title=title[1]	
             if(!is.null(dxtitle) && !is.numeric(dxtitle))dxtitle=NULL
             dxtitle=dxtitle[1]
             if(!is.null(size) && !is.numeric(size))size=NULL
             size=size[1]	
             if(!is.null(bend) && !is.numeric(bend))bend=NULL
             bend=bend[1]	
             
             if(!is.null(position)){
               position=switch(position, bottomright="bottomRight", 
                               bottomleft="bottomLeft", topright="topRight", topleft="topLeft", NULL)
             }	
             
             # default settings
             if(type=="nodecolor"){
               if(is.null(position))position="topRight"
               if(is.null(dxborder))dxborder=5
               if(is.null(dyborder))dyborder=5
               if(is.null(vertical))vertical=FALSE
               if(is.null(ftsize))ftsize=10
               if(is.null(title))title="nodecolorscale"
               if(is.null(dxtitle)){
                 mc<-max(nchar(labvec))
                 dxtitle=ftsize+ftsize*mc*0.6
               }
               if(is.null(size))size=20
               if(is.null(bend))bend=0.85	
             } else {
               if(is.null(position))position="topRight"
               if(is.null(dxborder))dxborder=5
               if(is.null(dyborder))dyborder=120
               if(is.null(vertical))vertical=FALSE
               if(is.null(ftsize))ftsize=10
               if(is.null(title))title="edgecolorscale"
               if(is.null(dxtitle)){
                 mc<-max(nchar(labvec))
                 dxtitle=ftsize+ftsize*mc*0.6
               }
               if(is.null(size))size=20
               if(is.null(bend))bend=0.85
             }
             bend=as.integer(bend*100)
             bend=min(max(bend,0),100)
             
             # set logical
             vertical=ifelse(vertical,"true","false")
             
             invisible( rederexpresspost(obj@uri, 'RedHandler.addLegendColor', colvec, labvec, size, 
                                         bend, ftsize, title, dxtitle, position, dxborder, dyborder, vertical, type ) )
             
           }
)

#-------------------------------------------------------------------------------
setMethod ('addLegend.size', 'RedPort', 
           function (obj, sizevec, type="node", labvec=NULL, position=NULL, dxborder=NULL, dyborder=NULL, vertical=NULL, 
                     ftsize=NULL, title=NULL, dxtitle=NULL, col=NULL, intersp=NULL, edgelen=NULL) {
             
             if(ping(obj)==0)return(invisible())
             
             #check loaded igraph
             igraph.check()
             
             # checks---------------------------------------------------- 
             if(!is.character(type))type="nodesize"
             type=switch(type, node="nodesize", edge="edgewidth", "nodesize")
             
             #Check if igraph object------------------------------------------
             if(igraph::is.igraph(sizevec)){
               if(type=="nodesize"){
                 if(!is.null(G(sizevec,"legNodeSize")$scale)){
                   if(!is.null(G(sizevec,"legNodeSize")$legend) && is.null(labvec)){
                     labvec=G(sizevec,"legNodeSize")$legend
                     if(is.null(title))title=G(sizevec,"legNodeSize")$title
                   }
                   sizevec=G(sizevec,"legNodeSize")$scale
                 } else {
                   stop("NOTE: there is no valid 'legNodeSize' legend information for this igraph object!!")
                 }
               } else {
                 if(!is.null(G(sizevec,"legEdgeWidth")$scale)){
                   if(!is.null(G(sizevec,"legEdgeWidth")$legend) && is.null(labvec)){
                     labvec=G(sizevec,"legEdgeWidth")$legend
                     if(is.null(title))title=G(sizevec,"legEdgeWidth")$title
                   }
                   sizevec=G(sizevec,"legEdgeWidth")$scale
                 } else {
                   stop("NOTE: there is no valid 'legEdgeWidth' legend information for this igraph object!!")
                 }			
               }
             }
             
             #size vec
             if(!is.null(sizevec) && length(sizevec)>1){       
               c1=!is.numeric(sizevec)
               if(c1){
                 stop("NOTE: 'size' must be provided as numerics!")
               } else if(sum(is.na(sizevec))>0){
                 stop("NOTE: invalid 'size' declaration: 'NA' found'!")
               } else if(sum(sizevec<0)>0){
                 stop("NOTE: invalid node 'size' input (options: >= 0)")
               } else {            
                 sizevec=as.numeric(sizevec)
               }
             } else if(is.null(sizevec)){
               type="null"
               sizevec=c(0,0)	
               labvec=NULL	
             } else {
               stop("NOTE: 'size' must be provided as a vector (numeric)!")
             }
             # label vec
             if(is.null(labvec))labvec=as.character(sizevec)	
             if(length(labvec)!=length(sizevec)){
               stop("NOTE: 'labvec' and 'sizevec' must have the same length!")
             }
             labvec=as.character(labvec)
             # further args
             if(!is.null(position) && !is.character(position))position=NULL
             position=position[1]
             if(!is.null(dxborder) && !is.numeric(dxborder))dxborder=NULL
             dxborder=dxborder[1]
             if(!is.null(dyborder) && !is.numeric(dyborder))dyborder=NULL
             dyborder=dyborder[1]
             if(!is.null(vertical) && !is.logical(vertical))vertical=NULL
             vertical=vertical[1]
             if(!is.null(ftsize) && !is.numeric(ftsize))ftsize=NULL
             ftsize=ftsize[1]
             if(!is.null(title) && !is.character(title))title=NULL
             title=title[1]	
             if(!is.null(dxtitle) && !is.numeric(dxtitle))dxtitle=NULL
             dxtitle=dxtitle[1]
             if(!is.null(edgelen) && !is.numeric(edgelen))edgelen=NULL
             edgelen=edgelen[1]
             if(!is.null(col) && !is.character(col))col=NULL
             col=col[1]
             if(sum(nchar(col)>7))col=substr(col,0,7)
             if(!is.null(intersp) && !is.numeric(intersp)) intersp=NULL
             intersp=intersp[1]
             
             if(!is.null(position)){
               position=switch(position, bottomright="bottomRight", 
                               bottomleft="bottomLeft", topright="topRight", topleft="topLeft", NULL)
             }
             
             # default settings
             if(type=="nodesize"){
               if(is.null(position))position="bottomLeft"
               if(is.null(dxborder))dxborder=10
               if(is.null(dyborder))dyborder=10
               if(is.null(vertical))vertical=FALSE
               if(is.null(ftsize))ftsize=10
               if(is.null(title))title="nodesize"
               if(is.null(dxtitle)){
                 mc<-max(nchar(labvec))
                 dxtitle=ftsize+ftsize*mc*0.6
               }
               if(is.null(col))col="#000000"
               if(is.null(intersp))intersp=3	
               if(is.null(edgelen))edgelen=0 #not used for nodes!
             } else {
               if(is.null(position))position="bottomLeft"
               if(is.null(dxborder))dxborder=10
               if(is.null(dyborder))dyborder=120
               if(is.null(vertical))vertical=TRUE
               if(is.null(ftsize))ftsize=10
               if(is.null(title))title="edgewidth"
               if(is.null(dxtitle)){
                 mc<-max(nchar(labvec))
                 dxtitle=ftsize+ftsize*mc*0.6
               }
               if(is.null(col))col="#000000"
               if(is.null(intersp))intersp=10
               if(is.null(edgelen))edgelen=50
             }
             
             # final col setting
             col=colorRampPalette(colors=c(col,col))(1)	
             # final logical setting
             vertical=ifelse(vertical,"true","false")
             
             invisible( rederexpresspost(obj@uri, 'RedHandler.addLegendSize', sizevec, labvec, col, intersp, 
                                         ftsize, title, dxtitle, position, dxborder, dyborder, vertical, type, edgelen ) )
             
           }
)

#-------------------------------------------------------------------------------
setMethod ('addLegend.shape', 'RedPort', 
           function (obj, shapevec, type="node", labvec=NULL, position=NULL, dxborder=NULL, dyborder=NULL, vertical=NULL, 
                     ftsize=NULL, title=NULL, dxtitle=NULL, col=NULL, size=NULL, intersp=NULL) {
             
             if(ping(obj)==0)return(invisible())
             
             #check loaded igraph
             igraph.check()
             
             # checks---------------------------------------------------- 
             
             if(!is.character(type))type="nodeshape"
             type=switch(type, node="nodeshape", edge="edgeshape", "nodeshape")
             
             #Check if igraph object------------------------------------------
             if(igraph::is.igraph(shapevec)){
               if(type=="nodeshape"){
                 if(!is.null(G(shapevec,"legNodeShape")$shape)){
                   if(!is.null(G(shapevec,"legNodeShape")$legend) && is.null(labvec)){
                     labvec=G(shapevec,"legNodeShape")$legend
                     if(is.null(title))title=G(shapevec,"legNodeShape")$title
                   }
                   shapevec=G(shapevec,"legNodeShape")$shape
                 } else {
                   stop("NOTE: there is no valid 'legNodeShape' legend information for this igraph object!!")
                 }
               } else {
                 if(!is.null(G(shapevec,"legEdgeType")$shape)){
                   if(!is.null(G(shapevec,"legEdgeType")$legend) && is.null(labvec)){
                     labvec=G(shapevec,"legEdgeType")$legend
                     if(is.null(title))title=G(shapevec,"legEdgeType")$title
                   }
                   shapevec=G(shapevec,"legEdgeType")$shape
                 } else {
                   stop("NOTE: there is no valid 'legEdgeType' legend information for this igraph object!!")
                 }			
               }
             }
             
             #shapes
             defaultv=c('ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND')
             defaulte=c('LONG_DASH','SOLID','DOTTED','DOTTED_SHORT')
             if(type=='nodeshape'){
               defaultshapes=defaultv
             } else {
               defaultshapes= defaulte
             }
             if(!is.null(shapevec) && length(shapevec)>1){       
               c1=!is.character(shapevec)
               if(c1){
                 stop("NOTE: 'shape' must be provided as character!")
               } else if(sum(is.na(shapevec))>0){
                 stop("NOTE: invalid 'shape' declaration: 'NA' found'!")
               } else if(sum(!shapevec%in%defaultshapes)>0){
                 stop(paste("NOTE: invalid 'shape' input. Options:", paste(defaultshapes, collapse="  ")))
               } else {            
                 shapevec=as.character(shapevec)
               }
             } else if(is.null(shapevec)){
               type="null"
               shapevec=c("null","null")	
               labvec=NULL	
             } else {
               stop("NOTE: 'shape' must be provided as a vector (character)!")
             }
             # label vec
             if(is.null(labvec))labvec=letters[c(1:length(shapevec))]
             if(length(labvec)!=length(shapevec)){
               stop("NOTE: 'labvec' and 'shapevec' must have same length!")
             }
             labvec=as.character(labvec)
             # further args
             if(!is.null(position) && !is.character(position))position=NULL
             position=position[1]
             if(!is.null(dxborder) && !is.numeric(dxborder))dxborder=NULL
             dxborder=dxborder[1]
             if(!is.null(dyborder) && !is.numeric(dyborder))dyborder=NULL
             dyborder=dyborder[1]
             if(!is.null(vertical) && !is.logical(vertical))vertical=NULL
             vertical=vertical[1]
             if(!is.null(ftsize) && !is.numeric(ftsize))ftsize=NULL
             ftsize=ftsize[1]
             if(!is.null(title) && !is.character(title))title=NULL
             title=title[1]	
             if(!is.null(dxtitle) && !is.numeric(dxtitle))dxtitle=NULL
             dxtitle=dxtitle[1]
             if(!is.null(col) && !is.character(col))col=NULL
             col=col[1]
             if(sum(nchar(col)>7))col=substr(col,0,7)
             if(!is.null(size) && !is.numeric(size))size=NULL
             size=size[1]	
             if(!is.null(intersp) && !is.numeric(intersp)) intersp=NULL
             intersp=intersp[1]
             
             if(!is.null(position)){
               position=switch(position, bottomright="bottomRight", 
                               bottomleft="bottomLeft", topright="topRight", topleft="topLeft", NULL)
             }
             
             # default settings
             if(type=="nodeshape"){
               if(is.null(position))position="topRight"
               if(is.null(dxborder))dxborder=5
               if(is.null(dyborder))dyborder=260
               if(is.null(vertical))vertical=TRUE
               if(is.null(ftsize))ftsize=10
               if(is.null(title))title=""
               if(is.null(dxtitle)){
                 mc<-max(nchar(labvec))
                 dxtitle=ftsize+ftsize*mc*0.6
               }
               if(is.null(col))col="#000000"
               if(is.null(size))size=15
               if(is.null(intersp))intersp=5	
             } else {
               if(is.null(position))position="topRight"
               if(is.null(dxborder))dxborder=100
               if(is.null(dyborder))dyborder=260
               if(is.null(vertical))vertical=TRUE
               if(is.null(ftsize))ftsize=10
               if(is.null(title))title=""
               if(is.null(dxtitle)){
                 mc<-max(nchar(labvec))
                 dxtitle=ftsize+ftsize*mc*0.6
               }
               if(is.null(col))col="#000000"
               if(is.null(size))size=1.2
               if(is.null(intersp))intersp=10
             }
             
             # final col setting
             col=colorRampPalette(colors=c(col,col))(1)	
             # final logical setting
             vertical=ifelse(vertical,"true","false")
             
             invisible( rederexpresspost(obj@uri, 'RedHandler.addLegendShape', shapevec, labvec, col, size, 
                                         intersp, ftsize, title, dxtitle, position, dxborder, dyborder, vertical, type) )
             
           }
)



############################################################
############################################################
############## All Deprecated Methods ######################
############################################################
############################################################


#-------------------------------------------------------------------------------
setMethod ('getNodeAliases', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeAliases") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeAliases', type, status))
           }
)
.getNodeAliases<-function (obj, status="all", type="node") { 
  return (xml.rpc (obj@uri, 'RedHandler.getNodeAliases', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeX', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeX") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeX', type, status))
           }
)
.getNodeX<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeX', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeY', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeY") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeY', type, status))
           }
)
.getNodeY<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeY', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeBend', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeBend") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeBend', type, status))
           }
)
.getNodeBend<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeBend', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeSize', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeSize") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeSize', type, status))
           }
)
.getNodeSize<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeSize', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeShape', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeShape") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeShape', type, status))
           }
)
.getNodeShape<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeShape', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeColor', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeColor") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeColor', type, status))
           }
)
.getNodeColor<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeColor', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeLineWidth', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeLineWidth") #2
             return (xml.rpc (obj@uri, 'RedHandler.getNodeLineWidth', type, status))
           }
)
.getNodeLineWidth<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeLineWidth', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeLineColor', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeLineColor") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeLineColor', type, status))
           }
)
.getNodeLineColor<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeLineColor', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeFontSize', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeFontSize") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeFontSize', type, status))
           }
)
.getNodeFontSize<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeFontSize', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeFontColor', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeFontColor") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeFontColor', type, status))
           }
)
.getNodeFontColor<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeFontColor', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getNodeWeight', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeWeight") #2
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeWeight', type, status))
           }
)    
.getNodeWeight<-function(obj, status="all", type="node"){
  return (xml.rpc (obj@uri, 'RedHandler.getNodeWeight', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getArrowDirection', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getArrowDirection") #2
             
             return (xml.rpc(obj@uri, 'RedHandler.getArrowDirection', type, status))
           }
)
.getArrowDirection<-function(obj, status="all", type="node"){
  return (xml.rpc(obj@uri, 'RedHandler.getArrowDirection', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getEdgeWidth', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getEdgeWidth") #2
             
             return (xml.rpc(obj@uri, 'RedHandler.getEdgeWidth', type, status))
           }
)
.getEdgeWidth<-function(obj, status="all", type="node"){
  return (xml.rpc(obj@uri, 'RedHandler.getEdgeWidth', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getEdgeColor', 'RedPort', 
           function (obj, status="all", type="node") {
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getEdgeColor") #2
             
             return (xml.rpc(obj@uri, 'RedHandler.getEdgeColor', type, status))
           }
)
.getEdgeColor<-function(obj, status="all", type="node"){
  return (xml.rpc(obj@uri, 'RedHandler.getEdgeColor', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getEdgeType', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getEdgeType") #2
             
             return (xml.rpc(obj@uri, 'RedHandler.getEdgeType', type, status))
           }
)
.getEdgeType<-function(obj, status="all", type="node"){
  return (xml.rpc(obj@uri, 'RedHandler.getEdgeType', type, status))
}

#-------------------------------------------------------------------------------
setMethod ('getEdgeWeight', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getEdgeWeight") #2
             
             return (xml.rpc(obj@uri, 'RedHandler.getEdgeWeight', type, status))
           }
)
.getEdgeWeight<-function(obj, status="all", type="node"){
  return (xml.rpc(obj@uri, 'RedHandler.getEdgeWeight', type, status))
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
setMethod ('getNodeW', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeW") 
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeW', type, status))
           }
)

#-------------------------------------------------------------------------------
setMethod ('getNodeH', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeH") 
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeH', type, status))
           }
)

#-------------------------------------------------------------------------------
setMethod ('getNodeFontName', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeFontName") 
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeFontName', type, status))
           }
)

#-------------------------------------------------------------------------------
setMethod ('getNodeFontStyle', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeFontStyle") 
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeFontStyle', type, status))
           }
)

#-------------------------------------------------------------------------------
setMethod ('getNodeFontX', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeFontX") 
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeFontX', type, status))
           }
)    

#-------------------------------------------------------------------------------
setMethod ('getNodeFontY', 'RedPort', 
           function (obj, status="all", type="node") { 
             if(ping(obj)==0)return(NULL)
             
             .Deprecated(new="addGraph/getGraph related methods",old="getNodeFontY") 
             
             return (xml.rpc (obj@uri, 'RedHandler.getNodeFontY', type, status))
           }
)

#Methods to set node attributes
#-------------------------------------------------------------------------------
setMethod ('setNodeAlias', 'RedPort', 
           function (obj, node, alias) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeAlias") 
             
             node=as.character(node)
             alias=as.character(alias)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeAlias', node, alias))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeXY', 'RedPort', 
           function (obj, node, x, y) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeXY") 
             
             c1=!is.numeric(x)
             c2=!is.numeric(y)
             if(c1 && c2){
               stop("Node coords. must be provided as numerics!")
             }
             if(sum(is.na(x))>0 || sum(is.na(y))>0 ){
               stop("Invalid node coords. declaration: 'NA' found'!")
             }     
             node=as.character(node) 
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeXY', node, x, y))
           }
) 

#-------------------------------------------------------------------------------
setMethod ('setNodeBend', 'RedPort', 
           function (obj, node, bend) {  
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeBend") 
             
             c1=!is.numeric(bend)
             if(c1){
               stop("Node 'bend' must be provided as numerics!")
             }
             if(sum(is.na(bend))>0){
               stop("Invalid node 'bend' declaration: 'NA' found'!")
             }       
             if(sum(bend<0)>0 || sum(bend>100)>0){
               stop("Invalid node 'bend' declaration (options: 0-100%)" )
             }      
             node=as.character(node)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeBend', node, bend))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeSize', 'RedPort', 
           function (obj, node, size) {
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeSize") 
             
             c1=!is.numeric(size)
             if(c1){
               stop("Node 'size' must be provided as numerics!")
             }
             if(sum(is.na(size))>0){
               stop("Invalid node 'size' declaration: 'NA' found'!")
             }
             if(sum(size<0)>0){
               stop("Invalid node 'size' declaration (options: >= 0)")
             }      
             node=as.character(node)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeSize', node, size))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeShape', 'RedPort', 
           function (obj, node, shape) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeShape") 
             
             node=as.character(node)
             shape=as.character(shape)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeShape', node, shape))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeColor', 'RedPort', 
           function (obj, node, color) {
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeColor") 
             
             if(!is.null(color)){       
               c1=!is.character(color)
               if(c1){
                 warning("NOTE: node 'color' must be provided as character (hexadecimal)!")
               } else if(is.na(color)){
                 warning("NOTE: invalid node 'color' declaration: 'NA' found'!")
               } else if(nchar(color)>9 ){
                 warning("NOTE: invalid node 'color' specification: not 'rgb' space! (ps. alpha not supported)")
               } else {
                 node=as.character(node)
                 color=as.character(color) 
                 if(nchar(color)>7) color=substr(color,0,7)
                 return (rederexpresspost(obj@uri, 'RedHandler.setNodeColor', node, color))
               }      
             }   
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeLineWidth', 'RedPort', 
           function (obj, node, width) {  
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeLineWidth") 
             
             c1=!is.numeric(width)
             if(c1){
               stop("Node line 'width' must be provided as numerics!")
             }
             if(sum(is.na(width))>0){
               stop("Invalid node line 'width' declaration: 'NA' found'!")
             }
             if(sum(width<0)>0){
               stop("Invalid node line 'width' declaration (options: >= 0)")
             }      
             node=as.character(node)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeLineWidth', node, width))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeLineColor', 'RedPort', 
           function (obj, node, color) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeLineColor") 
             
             node=as.character(node)
             color=as.character(color)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeLineColor', node, color))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeFontName', 'RedPort', 
           function (obj, node, name) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeFontName") 
             
             node=as.character(node)
             name=as.character(name)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeFontName', node, name))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeFontStyle', 'RedPort', 
           function (obj, node, style) {
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeFontStyle") 
             
             c1=!is.integer(style)
             c2=!is.numeric(style)
             if(c1 && c2){
               stop("Node 'style' must be provided as integers!")
             }
             if(sum(is.na(style))>0){
               stop("Invalid node 'style' declaration: 'NA' found'!")
             }
             if(sum(style<0)>0 || style>3){
               stop("Invalid node 'style' declaration (options: 0, 1 or 2)")
             }      
             node=as.character(node)
             style=as.numeric(style)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeFontStyle', node, style))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeFontSize', 'RedPort', 
           function (obj, node, size) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeFontSize") 
             
             c1=!is.numeric(size)
             if(c1){
               stop("Node 'font size' must be provided as numerics!")
             }
             if(sum(is.na(size))>0){
               stop("Invalid node 'font size' declaration: 'NA' found'!")
             }
             if(sum(size<0)>0){
               stop("Invalid node 'font size' declaration (options: >= 0)")
             }      
             node=as.character(node)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeFontSize', node, size))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeFontColor', 'RedPort', 
           function (obj, node, color) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeFontColor") 
             
             node=as.character(node)
             color=as.character(color)
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeFontColor', node, color))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeFontXY', 'RedPort', 
           function (obj, node, x, y) {  
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeFontXY")
             
             c1=!is.numeric(x)
             c2=!is.numeric(y)
             if(c1 && c2){
               stop("Node 'font coords.' must be provided as numerics!")
             }
             if(sum(is.na(x))>0 || sum(is.na(y))>0 ){
               stop("Invalid 'font coords.' declaration: 'NA' found'!")
             }   
             node=as.character(node)   
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeFontXY', node, x, y))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setNodeWeight', 'RedPort', 
           function (obj, node, weight) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setNodeWeight") 
             
             c1=!is.numeric(weight)
             if(c1){
               stop("Node 'weight' must be provided as numerics!")
             }
             if(sum(is.na(weight))>0){
               stop("Invalid node 'weight' declaration: 'NA' found'!")
             }         
             node=as.character(node) 
             return (rederexpresspost(obj@uri, 'RedHandler.setNodeWeight', node, weight))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setEdgeWidth', 'RedPort', 
           function (obj, nodeA, nodeB, width) {  
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setEdgeWidth") 
             
             c1=!is.numeric(width)
             if(c1){
               stop("Edge 'width' must be provided as numerics!")
             }
             if(sum(is.na(width))>0){
               stop("Invalid edge 'width' declaration: 'NA' found'!")
             }
             if(sum(width<=0)>0){
               stop("Invalid edge 'width' declaration (options: > 0)")
             }      
             nodeA=as.character(nodeA)
             nodeB=as.character(nodeB)
             return (rederexpresspost(obj@uri, 'RedHandler.setEdgeWidth', nodeA, nodeB, width))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setEdgeColor', 'RedPort', 
           function (obj, nodeA, nodeB, color) {
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setEdgeColor") 
             
             nodeA=as.character(nodeA)
             nodeB=as.character(nodeB)  
             color=as.character(color)
             return (rederexpresspost(obj@uri, 'RedHandler.setEdgeColor', nodeA, nodeB, color))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setEdgeType', 'RedPort', 
           function (obj, nodeA, nodeB, type) { 
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setEdgeType") 
             
             nodeA=as.character(nodeA)
             nodeB=as.character(nodeB)  
             type=as.character(type)
             return (rederexpresspost(obj@uri, 'RedHandler.setEdgeType', nodeA, nodeB, type))
           }
)

#-------------------------------------------------------------------------------
setMethod ('setEdgeWeight', 'RedPort', 
           function (obj, nodeA, nodeB, weight) {  
             if(ping(obj)==0)return(invisible())
             
             .Deprecated(new="addGraph/getGraph related methods",old="setEdgeWeight") 
             
             c1=!is.numeric(weight)
             if(c1){
               stop("Edge 'weight' must be provided as numerics!")
             }
             if(sum(is.na(weight))>0){
               stop("Invalid edge 'weight' declaration: 'NA' found'!")
             }      
             nodeA=as.character(nodeA)
             nodeB=as.character(nodeB)
             return (rederexpresspost(obj@uri,'RedHandler.setEdgeWeight', nodeA, nodeB, weight))
           }
)
