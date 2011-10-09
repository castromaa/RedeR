 
   
#Primary-call methods 
    
#-------------------------------------------------------------------------------
setMethod ('ping', 'RedPort', 
  function (obj) { 
      #Check if the port is not in use by the app---------------------------
      calltest=try(xml.rpc(obj@uri,'RedHandler.ping'), TRUE) 
      if(is.numeric(calltest) || is.integer(calltest)){
      	if(calltest==1){
      		return(1)
      	} else {
      		return(0)
      	}
      } else {
		    return(0)
      }
    })  
#-------------------------------------------------------------------------------
setMethod ('exitd', 'RedPort', 
  function (obj) { 
  		if(ping(obj)==0)return(invisible())
  		Sys.sleep(0.5)
    	invisible(xml.rpc(obj@uri, 'RedHandler.exit'))
    	Sys.sleep(0.5)
    }) 
#-------------------------------------------------------------------------------
setMethod ('resetd', 'RedPort', 
  function (obj) {
  		if(ping(obj)==0)return(invisible())
    	invisible(xml.rpc(obj@uri, 'RedHandler.reset'))
    })      
#-------------------------------------------------------------------------------
setMethod ('version', 'RedPort', 
  function (obj) { 
  		if(ping(obj)==0)return(invisible())
   	 	return (xml.rpc (obj@uri, 'RedHandler.version'))
    })
#-------------------------------------------------------------------------------
setMethod ('calld', 'RedPort',

   function (obj, filepath='default', ADDPATH='', checks='lock') {

      #Check if the port is not in use by the app---------------------------
      if(ping(obj)==1){
          tx=paste("R interface is already in use! Port: ", obj@port, sep='') 
          return(tx)
      }
         
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
      while(status=="OFF"){        
          #timer
          tdelta = proc.time()[2] - t0
          if(tdelta>0.30){
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
                           
    })            
      
#-------------------------------------------------------------------------------
setMethod ('updateGraph', 'RedPort', 
  function (obj) { 
  	if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.updateGraph'))
    })


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
    require (JavaGD)                  ##This dependence so far has no namespace!
    JavaGD(width=width, height=height, ps=ps)
    })


#Get RedeR graph via RedeR methods and wrap it up into igraph objects  
#-------------------------------------------------------------------------------
setMethod ('getGraph', 'RedPort', 
  function (obj, type="node", status="all", attribs="plain") {
      if(ping(obj)==0)return(graph.empty(n=0, directed=FALSE))
      
      #Get graph objects from RedeR app
      nodes = getNodes(obj, type, status)
      edges = getEdges(obj, type, status) 

	  #Build igraph object
      if(length(nodes)==1 && nodes==""){
          	g 		= graph.empty(n=0, directed=FALSE)
          	return(g) 
      } else if(length(edges)==1 && edges ==""){
      		edges	  = NULL
      		g	      = graph.empty(n=length(nodes), directed=FALSE)
      		g         = set.vertex.attribute(g, "name", value=nodes) 
      } else {
      		nodes	= data.frame(name=nodes)
       		edges 	= matrix(edges,ncol=2, byrow=TRUE)
      		colnames(edges) = c("NodeA","NodeB")
      		edges 	= data.frame(edges) 
      		g  	  	= graph.data.frame(edges, directed=FALSE, vertices=nodes)
      }  
      
      #Add required attributes    
      if(attribs=="minimal"){
      	  return(g) 
      } else if(attribs=="plain"){
          nodeX     = getNodeX (obj, type, status )
          nodeY     = getNodeY (obj, type, status )
          g         = set.vertex.attribute(g, "coordX", value=nodeX)
          g         = set.vertex.attribute(g, "coordY", value=nodeY)
          return(g)
      } else if(attribs=="all"){
          nodeAlias      = getNodeAliases (obj, type, status ) 
          nodeBend       = getNodeBend (obj, type, status )     
          nodeX          = getNodeX (obj, type, status )
          nodeY          = getNodeY (obj, type, status )
          nodeSize       = getNodeSize (obj, type, status )
          nodeShape      = getNodeShape (obj, type, status )
          nodeColor      = getNodeColor (obj, type, status )
          nodeWeight     = getNodeWeight (obj, type, status )
          nodeLineWidth  = getNodeLineWidth (obj, type, status )
          nodeLineColor  = getNodeLineColor (obj, type, status ) 
          nodeFontSize   = getNodeFontSize (obj, type, status )
          nodeFontColor  = getNodeFontColor (obj, type, status )        
          g     = set.vertex.attribute(g, "nodeAlias",  value=nodeAlias)
          g     = set.vertex.attribute(g, "nodeBend",   value=nodeBend)
          g     = set.vertex.attribute(g, "coordX",   value=nodeX)
          g     = set.vertex.attribute(g, "coordY",   value=nodeY)
          g     = set.vertex.attribute(g, "nodeSize",   value=nodeSize)          
          g     = set.vertex.attribute(g, "nodeShape",  value=nodeShape)
          g     = set.vertex.attribute(g, "nodeColor",  value=nodeColor)
          g     = set.vertex.attribute(g, "nodeWeight", value=nodeWeight)
          g     = set.vertex.attribute(g, "nodeLineWidth",  value=nodeLineWidth)
          g     = set.vertex.attribute(g, "nodeLineColor",  value=nodeLineColor)
          g     = set.vertex.attribute(g, "nodeFontSize",   value=nodeFontSize)
          g     = set.vertex.attribute(g, "nodeFontColor",  value=nodeFontColor) 
      	  #..get edge attrs. if present!
      	  if(!is.null(edges)){
          	arrowDirection = getArrowDirection (obj, type, status )  
          	edgeWeight     = getEdgeWeight(obj, type, status )
          	edgeWidth      = getEdgeWidth (obj, type, status )
          	edgeColor      = getEdgeColor (obj, type, status )
          	edgeType       = getEdgeType  (obj, type, status )
          	g     = set.edge.attribute (g, "arrowDirection", value=arrowDirection)
          	g     = set.edge.attribute (g, "edgeWeight",     value=edgeWeight)
          	g     = set.edge.attribute (g, "edgeWidth",      value=edgeWidth)
          	g     = set.edge.attribute (g, "edgeColor",      value=edgeColor)
          	g     = set.edge.attribute (g, "edgeType",       value=edgeType)
          }          
          return(g)       
      }    
      
    })
  
#Add subgraph list to RedeR app
#-------------------------------------------------------------------------------
setMethod ('addSubgraph.list', 'RedPort', 
	function (obj, g, nodeList, gridRows=2, gridScale=80, gscale=20, gatt=NULL, update=NULL, theme='tm0') {
		if(ping(obj)==0)return(invisible())
    	#Check igraph object-----------------------------------------------
    	if(!is.igraph(g)){
        	stop("Not an igraph object!")
    	}
    	if(vcount(g)==0){
        	stop("Empty main graph!")
    	}		
    	#Further checks-----------------------------------------------------
		if(!is.list(nodeList)){
			stop("NOTE: 'nodeList' must be a list of vectors with node names!")
		}
		if(length(nodeList)==0){
			stop("NOTE: invalid 'nodeList' arg length!")
		}		
     	if(!is.null(gatt) && !is.data.frame(gatt)){
    		stop("NOTE: 'gatt' must be a data.frame with graph attributes (ps. attribute names on cols)!")
    	}        
    	if(!is.null(gatt) && nrow(gatt)!=length(nodeList)){
    		stop("NOTE: nrow 'gatt' must match 'nodeList' length!")
    	}
    	#Remove multiple edges and loops---
    	if(!is.simple(g)){
    		#warning("NOTE: loops and/or multiple edges were removed from your graph!")
    		g=simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    	}     
    	#Check direction
    	if(is.directed(g)){
        	# set direction to edge attributes
        	gtemp=g
        	E(gtemp)$arrowDirection=1
        	E(gtemp)$arrowDirection[is.mutual(gtemp)]=3
        	# collapse mutual edges to unique edges and check edge attributes
        	gtemp=as.undirected(gtemp, mode="each")
        	gtemp=simplify(gtemp)
        	c1=length(list.edge.attributes(g))>0
        	c2=ecount(g)>ecount(gtemp)
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
    		gbasic=graph.empty(n=length(nodeList),directed=FALSE)  
        	layout=layout.norm(layout.circle(gbasic),xmin = 25, xmax=75, ymin=25, ymax=75) 
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
  		invisible(xml.rpc (obj@uri,'RedHandler.lockDragAndZoom'))
		
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
		  				g <- remove.vertex.attribute(g,"coordX")
    					g <- remove.vertex.attribute(g,"coordY")
    					att$isNest=FALSE
    					att$update=update[1]
		  			}
		  			if(is.null(att$isNest))att$isNest=TRUE
		  			if(is.null(att$isAnchor))att$isAnchor=TRUE
		  			addSubgraph(obj,g,nodes,gscale=gscale,gatt=att,theme=theme)
		  		} else {
		  			att=list()
		  			if(is.null(update)){
        				att$coordX=layout[i,1]
        				att$coordY=layout[i,2]
		  				if(!is.null(zoom)){
		  					att$zoom=zoom
		  				} else if(is.null(g$zoom)){
		  					att$zoom=50
		  				} else {
		  					att$zoom=g$zoom
		  				}
		  			} else {
		  				g <- remove.vertex.attribute(g,"coordX")
    					g <- remove.vertex.attribute(g,"coordY")
    					g$isNest=FALSE 	
     					att$update=update[1] 
		  			}
					if(!is.null(g$gscale))att$gscale=g$gscale
					if(!is.null(g$isNest)){att$isNest=g$isNest}else{att$isNest=TRUE}
					if(!is.null(g$nestImage))att$nestImage=as.character(g$nestImage)
					if(!is.null(g$isAnchor)){att$isAnchor=g$isAnchor} else {att$isAnchor=TRUE}
					if(!is.null(g$nestAlias))att$nestAlias=as.character(g$nestAlias)
					if(!is.null(g$nestColor))att$nestColor=as.character(g$nestColor)
					if(!is.null(g$nestLineType))att$nestLineType=as.character(g$nestLineType)
					if(!is.null(g$nestFontSize))att$nestFontSize = g$nestFontSize
					if(!is.null(g$nestFontColor))att$nestFontColor = g$nestFontColor
					if(!is.null(g$nestFontX))att$nestFontX=g$nestFontX
					if(!is.null(g$nestFontY))att$nestFontY=g$nestFontY
					if(!is.null(g$nestShape))att$nestShape=as.character(g$nestShape)
					if(!is.null(g$nestSize))att$nestSize=g$nestSize
					if(!is.null(g$nestLineWidth))att$nestLineWidth=g$nestLineWidth
					if(!is.null(g$nestLineColor))att$nestLineColor=as.character(g$nestLineColor)
					if(!is.null(g$isAssign))att$isAssign=g$isAssign
					if(!is.null(g$loadEdges))att$loadEdges=g$loadEdges
		  			addSubgraph(obj,g,nodes, gscale=gscale, gatt=att, theme=theme)
		  		}
		  }
	   }  		
	   	#Internal function (unlocks DragAndZoon interactivity after sent subgraph list)
  		invisible(xml.rpc (obj@uri,'RedHandler.unLockDragAndZoom'))
    })


#Add subgraphs to RedeR app
#-------------------------------------------------------------------------------
setMethod ('addSubgraph', 'RedPort', 
	function (obj, g, nodes, gscale=75, gcoord=c(75,75), gatt=NULL,theme='tm0') {
		if(ping(obj)==0)return(invisible())
	
    	#Check igraph object-----------------------------------------------
    	if(!is.igraph(g)){
        	stop("Not an igraph object!")
    	}    	
    	if(vcount(g)==0){
        	stop("Empty main graph!")
     	} 
    	#Further checks-----------------------------------------------------
     	if(!is.null(gatt) && !is.list(gatt)){
    		stop("NOTE: 'gatt' must be a list of graph attributes (e.g. gatt$coordX, gatt$coordY, gatt$gscale...)!")
    	}
    	#Remove multiple edges and loops---
    	if(!is.simple(g)){
    		#warning("NOTE: loops and/or multiple edges were removed from your graph!")
    		g=simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    	}     
    	#Check direction
    	if(is.directed(g)){
        	# set direction to edge attributes
        	gtemp=g
        	E(gtemp)$arrowDirection=1
        	E(gtemp)$arrowDirection[is.mutual(gtemp)]=3
        	# collapse mutual edges to unique edges and check edge attributes
        	gtemp=as.undirected(gtemp, mode="each")
        	gtemp=simplify(gtemp)
        	c1=length(list.edge.attributes(g))>0
        	c2=ecount(g)>ecount(gtemp)
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
			zoom         = g$zoom
			scale        = g$gscale
			coordX       = g$coordX
			coordY       = g$coordY
			nestImage    = g$nestImage
			isAnchor     = g$isAnchor
			loadEdges    = g$loadEdges
			bgColor      = g$bgColor
			isNest       = g$isNest
			isAssign     = g$isAssign
			nestAlias    = g$nestAlias
			nestColor    = g$nestColor
			nestFontSize = g$nestFontSize
			nestFontColor = g$nestFontColor
			nestFontX     = g$nestFontX
			nestFontY     = g$nestFontY
      		nestShape      = g$nestShape
      		nestSize       = g$nestSize
      		nestLineWidth  = g$nestLineWidth
      		nestLineColor  = g$nestLineColor
      		update         = g$update
      		nestLineType   = g$nestLineType
		}
				
		#Add subgraph
		sg=subgraph(g,nodes)
		if(!is.null(zoom))    sg$zoom = zoom
		if(!is.null(bgColor)) sg$bgColor = as.character(bgColor)
		if(!is.null(scale))   sg$gscale = scale
		if(!is.null(coordX))  sg$coordX = coordX
		if(!is.null(coordY))  sg$coordY = coordY
		if(!is.null(loadEdges)) sg$loadEdges = loadEdges
		if(!is.null(isNest))    sg$isNest = isNest
		if(!is.null(nestImage))   sg$nestImage = as.character(nestImage)
		if(!is.null(isAnchor)){sg$isAnchor = isAnchor} else {sg$isAnchor=TRUE}
		if(!is.null(nestAlias))sg$nestAlias = as.character(nestAlias)
		if(!is.null(nestColor))  sg$nestColor = as.character(nestColor)
		if(!is.null(nestFontSize)) sg$nestFontSize = nestFontSize
		if(!is.null(nestFontColor))sg$nestFontColor = nestFontColor
		if(!is.null(nestFontX)) sg$nestFontX = nestFontX
		if(!is.null(nestFontY)) sg$nestFontY = nestFontY
		if(!is.null(nestShape)) sg$nestShape = as.character(nestShape)
		if(!is.null(nestSize))sg$nestSize=nestSize
		if(!is.null(nestLineWidth)) sg$nestLineWidth = nestLineWidth
		if(!is.null(nestLineColor)) sg$nestLineColor = as.character(nestLineColor)
		if(!is.null(nestLineType))sg$nestLineType = nestLineType
		if(!is.null(update)){
			if(update=="all" || update=="partial")sg$isUpdate=TRUE
			if(update=="partial")sg$loadEdges=FALSE
		}
		if(!is.null(isAssign) && isAssign==TRUE){
			sg$isAssign=TRUE
		}
    	if(!is.null(update)){
    		ref=addGraph(obj, sg)
    	} else {		
			ref=addGraph(obj, sg, gscale=gscale, gcoord=gcoord, theme=theme)
			if(!is.null(ref))return(ref)
		}
   })		  

   
#Duplicate RedeR networks and subnetworks
#-------------------------------------------------------------------------------
setMethod ('duplicateGraph', 'RedPort', 
	function (obj, isToCopyEdges=TRUE, isDefaultCopy=TRUE, nodes=NULL) {
		if(ping(obj)==0)return(invisible())
		
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
       			invisible( xml.rpc (obj@uri, 'RedHandler.duplicateSubNetwork', arg1, arg2, nodes ) )
       		}
       	} else {
 			arg1="yes"
			arg2="yes" 
       		if(!isToCopyEdges){arg1="no"}
       		if(!isDefaultCopy){arg2="no"}
       		message("... duplicate graph")
       		invisible( xml.rpc (obj@uri, 'RedHandler.duplicateNetwork', arg1, arg2) )
       	}		
		
    })
    		

#Duplicate RedeR network and update the copy with new attributes 
#-------------------------------------------------------------------------------
setMethod ('addSeries', 'RedPort', 
	function (obj, g, setnodes=TRUE, setedges=TRUE) {
		if(ping(obj)==0)return(invisible())
		
    #Check igraph object-----------------------------------------------
    if(!is.igraph(g)){
        stop("Not an igraph object!")
    }   
    if(vcount(g)==0){
        stop("Empty graph!")
    }
    #Remove attributes no longer needed!
    g <- remove.vertex.attribute(g,"coordX")
    g <- remove.vertex.attribute(g,"coordY")      
    if(!setnodes){  
    	g <- remove.vertex.attribute(g,"nodeAlias")
    	g <- remove.vertex.attribute(g,"nodeBend")
    	g <- remove.vertex.attribute(g,"nodeSize")
    	g <- remove.vertex.attribute(g,"nodeShape")
    	g <- remove.vertex.attribute(g,"nodeColor")
    	g <- remove.vertex.attribute(g,"nodeWeight")
    	g <- remove.vertex.attribute(g,"nodeLineWidth")
    	g <- remove.vertex.attribute(g,"nodeLineColor")
    	g <- remove.vertex.attribute(g,"nodeFontSize")
    	g <- remove.vertex.attribute(g,"nodeFontColor")
    }
    isToCopyEdges="no"
    if(!setedges){
    	isToCopyEdges="yes"
    	g <- remove.edge.attribute(g,"arrowDirection")
    	g <- remove.edge.attribute(g,"edgeWeight")
    	g <- remove.edge.attribute(g,"weight")
    	g <- remove.edge.attribute(g,"edgeWidth")
    	g <- remove.edge.attribute(g,"edgeColor")
    	g <- remove.edge.attribute(g,"edgeType")
    	g <- delete.edges(g,E(g))
    }
    
    #send request to duplicate the original network in the main panel
    isDefaultCopy="no"
    ref=try(xml.rpc(obj@uri,'RedHandler.duplicateNetwork', isToCopyEdges, isDefaultCopy),TRUE)
    if(!inherits(ref, "try-error")){
    	print(paste("New container ID: ", ref, sep=""))
    	#send graph to RedeR (it will update the reference network)
    	addGraph(obj,g)    	
    } else {
    	warning("Unable to complete the request!")
    }
  
  })
  
#Wrap up igraph objects via RedeR methods and submit to RedeR app 
#-------------------------------------------------------------------------------
setMethod ('addGraph', 'RedPort', 
  function (obj, g, layout=layout.fruchterman.reingold(g), gscale=75, gcoord=c(50,50), isNest=FALSE, 
  	nestImage='plain', isAnchor=TRUE, isAssign=FALSE, loadEdges=TRUE, parent=NULL, 
  	minimal=FALSE, theme='tm0') {
	
	if(ping(obj)==0)return(invisible())
    
    #Check igraph object-----------------------------------------------
    if(!is.igraph(g)){
        stop("Not an igraph object!")
    }
    #Remove multiple edges and loops---
    if(!is.simple(g)){
    	#warning("NOTE: loops and/or multiple edges were removed from your graph!")
    	g=simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    }     
    #Check direction
    if(is.directed(g)){
        # set direction to edge attributes
        gtemp=g
        E(gtemp)$arrowDirection=1
        E(gtemp)$arrowDirection[is.mutual(gtemp)]=3
        # collapse mutual edges to unique edges and check edge attributes
        gtemp=as.undirected(gtemp, mode="each")
        gtemp=simplify(gtemp)
        c1=length(list.edge.attributes(g))>0
        c2=ecount(g)>ecount(gtemp)
        if(c1 && c2){
        	warning("NOTE: attributes from mutual edges was collapsed to unique edges (see 'addGraph' doc).")
        }
        g=gtemp
    }
    #Check igraph size-------------------------------------------------
    #...if empty graph!
    if(vcount(g)==0){
    	g=add.vertices(g, 2)
    	if((!is.null(g$isNest) && g$isNest)||isNest){
    		V(g)$name=c("NN0<$$>","NN1<$$>")
    	} else {
    		V(g)$name=c("MM0<$$>","MM1<$$>")
    	}
    	layout=layout.random(g)
    }
    #Get names from vertex if not available------------------------------
    if(is.null(V(g)$name)){
    	V(g)$name=as.character(V(g))
    } else {
    	V(g)$name=as.character(V(g)$name)
    }
    #Add empty node if vcount(g)==1 
    #..to keep data type as vector during server connection!   
    if(vcount(g)==1){
     	g=add.vertices(g, 1)
    	if((!is.null(g$isNest) && g$isNest)||isNest){
    		V(g)$name[2]="NN0<$$>"   		
    	} else {
    		V(g)$name[2]="MM0<$$>"  		
    	}
     	layout=layout.random(g)
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
    if(is.numeric(g$gscale))gscale=g$gscale[1]
	if(is.numeric(g$coordX))gcoord[1]=g$coordX[1] 
	if(is.numeric(g$coordY))gcoord[2]=g$coordY[1]     
    if(is.logical(g$isNest))isNest=g$isNest[1]
	if(is.character(g$nestImage))nestImage=g$nestImage[1] 
    if(is.logical(g$isAnchor))isAnchor=g$isAnchor[1]
    if(is.logical(g$isAssign) && isNest){
    	isAssign=g$isAssign[1]
    	if(isAssign){
			temp=V(g)$name
			if(is.null(V(g)$nodeAlias)){
				V(g)$nodeAlias=temp
			} else {
				idx=is.na(V(g)$nodeAlias)
				V(g)$nodeAlias[idx]=temp[idx]
			}
			id="N001"
  			if(!is.null(parent))id=parent
			V(g)$name=paste(temp,".$",id,sep="")
    	}
    }
    if(is.logical(g$loadEdges))loadEdges=g$loadEdges[1]
    zoom=NULL
	if(is.numeric(g$zoom))zoom=g$zoom[1]    
    bgColor=NULL
	if(is.character(g$bgColor))bgColor=g$bgColor[1]          
    nestColor=NULL
    gatt<-list()           
	if(is.character(g$nestColor))gatt$nestColor=g$nestColor[1] 
    nestAlias=NULL
	if(is.character(g$nestAlias))gatt$nestAlias=g$nestAlias[1]
    nestFontSize=NULL
	if(is.numeric(g$nestFontSize))gatt$nestFontSize=g$nestFontSize[1]  
    nestFontColor =NULL 
	if(is.character(g$nestFontColor))gatt$nestFontColor=g$nestFontColor[1]    
    nestFontX=NULL 
	if(is.numeric(g$nestFontX))gatt$nestFontX=g$nestFontX[1]
    nestFontY=NULL 
	if(is.numeric(g$nestFontY))gatt$nestFontY=g$nestFontY[1]
    nestShape=NULL 
	if(is.character(g$nestShape))gatt$nestShape=g$nestShape[1]
    nestSize=NULL 
	if(is.numeric(g$nestSize)) gatt$nestSize=g$nestSize[1]   
    nestLineWidth=NULL 
	if(is.numeric(g$nestLineWidth))gatt$nestLineWidth=g$nestLineWidth[1]   
    nestLineColor =NULL 
	if(is.character(g$nestLineColor))gatt$nestLineColor=g$nestLineColor[1]
    nestLineType =NULL 
	if(is.character(g$nestLineType))gatt$nestLineType=g$nestLineType[1]
    update="default"
    if(is.logical(g$isUpdate)){
    	if(g$isUpdate)update="update"
    }      
    #Check gcoord option-----------------------------------------------
    c1=!is.numeric(gcoord)
    c2=!length(gcoord)==2
    if(c1 || c2){
    	if(g$gcoord!=NULL){
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
       		 invisible( xml.rpc (obj@uri, 'RedHandler.setZoom', zoom) )
        }
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
       else if( nrow(layout)!=vcount(g) ) {
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
       	  pScale=xml.rpc(obj@uri,'RedHandler.getPanelScale')
       	  if(is.numeric(pScale)){
       	  		pScale=pScale[1]*(gscale[1]/100)
       	  } else {
       	  		pScale=500
       	  }
       	  if(isNest)pScale=pScale/sqrt(2)
          layout=layout.norm(layout,xmin=0, xmax=pScale, ymin=0, ymax=pScale)
          V(g)$coordX=layout[,1]
          V(g)$coordY=layout[,2]
       }       
    }
    
    #Set/get nodes and edges to submit to the app------------------------        
    nodes = V(g)$name
    edges = get.edgelist(g, names=TRUE)
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
       		 invisible( xml.rpc (obj@uri, 'RedHandler.setBackground', bgColor) )
        }      
    }  
        
    #Add nodes, edges and set attributes (if available)------------------------
    if(vcount(g)>0)message("** ... nodes!") 
    if(ecount(g)>0)message("** ... edges!")
    
	#library(RCurl)
	#myOpts=list()
	#myOpts[["tcp.nodelay"]]=TRUE
	#h = getCurlHandle()
	#(..., .opts=myOpts, .curl=h)
	
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
        	invisible(xml.rpc(obj@uri,'RedHandler.addEdgesFastload',as.character(xedges)) )
        	return("Done!")
   	 	}
	  }

      if(length(list.vertex.attributes(g))>0){
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
                coordY=as.numeric(c(10,10))
           }
           else if(sum(is.na(coordX))>0 || sum(is.na(coordY))>0 ){
                warning("NOTE: invalid node coords. declaration: 'NA' found'!")
                coordX=as.numeric(c(10,10))
                coordY=as.numeric(c(10,10))
           } else {
                message("** ... node 'coords'") 
                coordX=as.numeric(coordX) 
                coordY=as.numeric(coordY) 
           }      
      } else {
           coordX=as.numeric(c(10,10))
           coordY=as.numeric(c(10,10))      
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
      if(!is.null(nodeColor) && length(nodeColor)>0){       
        c1=!is.character(nodeColor)
        if(c1){
          warning("NOTE: node 'color' must be provided as character (hexadecimal)!")
          nodeColor=as.character(c('',''))
        }
        else if(sum(is.na(nodeColor))>0){
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
      if(!is.null(nodeLineColor) && length(nodeLineColor)>0){       
        c1=!is.character(nodeLineColor)
        if(c1){
          warning("NOTE: node 'line color' must be provided as hexadecimal!")
          nodeLineColor=as.character(c('',''))
        }
        else if(sum(is.na(nodeLineColor))>0){
            warning("NOTE: invalid node 'line color' declaration: 'NA' found'!")
            nodeLineColor=as.character(c('',''))
        } else if(sum(nchar(nodeLineColor)>9)){
        	warning("NOTE: invalid node 'line color' specification: not 'rgb' space! (ps. alpha not supported)")
        	nodeLineColor=as.character(c('',''))
        } else {           
             message("** ... node 'line color'")
             if(sum(nchar(nodeLineColor)>7)) nodeLineColor=substr(nodeLineColor,0,7)
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
      if(!is.null(nodeFontColor) && length(nodeFontColor)>0){       
        c1=!is.character(nodeFontColor)
        if(c1){
          warning("NOTE: node 'font color' must be provided as hexadecimal!")
          nodeFontColor=as.character(c('',''))
        }
        else if(sum(is.na(nodeFontColor))>0){
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
      edgeWeight     = E(g)$edgeWeight
      igraphWeight   = E(g)$weight
      edgeWidth      = E(g)$edgeWidth
      edgeColor      = E(g)$edgeColor
      edgeType       = E(g)$edgeType
      
      if(length(list.edge.attributes(g))>0 && ecount(g)>0){
      		message('*** Uploading edge attributes ...')   
      }
          
      #set compatibility with igraph!          
      if(is.null(edgeWeight) && !is.null(igraphWeight)) edgeWeight=igraphWeight 
      #to correct vector in case of only one attr/one edge.
      if(nrow(edges)==1){
         edges=rbind(edges,edges)
         if(!is.null(arrowDirection)) arrowDirection=c(arrowDirection,-1)
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
        c3=(sum(arrowDirection<0)>0 || sum(arrowDirection>3)>0)
        c4=length(arrowDirection)==2 && sum(arrowDirection<0)==1
        if(c1 && c2){
            warning("NOTE: edge 'direction' must be provided as integers!")
            arrowDirection=as.numeric(c(-1,-1))
        }
        else if(sum(is.na(arrowDirection))>0){
            warning("NOTE: invalid edge 'direction' declaration: 'NA' found'!")
            arrowDirection=as.numeric(c(-1,-1))
        }
        else if(c3 && !c4){
          warning("NOTE: invalid 'direction' input (options: 0,1,2 or 3)")
          arrowDirection=as.numeric(c(-1,-1))
        } else { 
           arrowDirection=as.numeric(arrowDirection)            
           message("** ... edge 'arrow direction'")
        }
    } else {
        arrowDirection=as.numeric(c(-1,-1))
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
    if(!is.null(edgeColor) && length(edgeColor)>0){       
      c1=!is.character(edgeColor)
      if(c1){
          warning("NOTE: edge 'color' must be provided as hexadecimal!")
          edgeColor=as.character(c('',''))
      }
      else if(sum(is.na(edgeColor))>0){
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
    
    #Loading graph...      
    isBrandNew=ifelse(isNest && isAssign,'true','false')
    numsuppl=c(gcoord[1],gcoord[2])
    charsuppl=c(update,isBrandNew)    
    if(ecount(g)>0 && loadEdges){
    	#update, isBrandNew   	
        #Main call to load nodes and edges
        invisible( xml.rpc (obj@uri, 'RedHandler.updateGraphMap', edges[,1],
              edges[,2], arrowDirection, edgeWeight, edgeWidth, edgeColor, edgeType,
              nodes, coordX, coordY, nodeBend, nodeSize, nodeShape, nodeColor,
              nodeWeight, nodeLineWidth, nodeLineColor, nodeFontSize,
              nodeFontColor, nodeAlias, numsuppl, charsuppl) )             
    } else {
        #Main call to load only nodes
        invisible (xml.rpc (obj@uri, 'RedHandler.updateNodeMap', 
              nodes, coordX, coordY, nodeBend, nodeSize, nodeShape, nodeColor,
              nodeWeight, nodeLineWidth, nodeLineColor, nodeFontSize,
              nodeFontColor, nodeAlias, numsuppl, charsuppl) )
    }
    
    #Check nesting condition
    nestref=NULL
    if(isNest){
        nestref=nestNodes(obj, nodes, nestImage, isAssign, isAnchor, gscale=gscale, gcoord=NULL,gatt=gatt, theme=theme)
        if(!is.null(nestref))return(nestref)
    } else {
    	invisible( updateGraph(obj) )
    }
    

  })
        
       
#Methods to get node attributes

#-------------------------------------------------------------------------------
setMethod ('getNodes', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodes', type, status))
    })    
#-------------------------------------------------------------------------------
setMethod ('getNodeIDs', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeIDs', type, status))
    }) 
#-------------------------------------------------------------------------------
setMethod ('getNodeAliases', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeAliases', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeX', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeX', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeY', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeY', type, status))
    })    
#-------------------------------------------------------------------------------
setMethod ('getNodeW', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeW', type, status))
    })    
#-------------------------------------------------------------------------------
setMethod ('getNodeH', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeH', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeBend', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeBend', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeSize', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeSize', type, status))
    })            
#-------------------------------------------------------------------------------
setMethod ('getNodeShape', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeShape', type, status))
    })            
#-------------------------------------------------------------------------------
setMethod ('getNodeColor', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeColor', type, status))
    })            
#-------------------------------------------------------------------------------
setMethod ('getNodeLineWidth', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeLineWidth', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeLineColor', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeLineColor', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeFontName', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeFontName', type, status))
    })     
#-------------------------------------------------------------------------------
setMethod ('getNodeFontStyle', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeFontStyle', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeFontSize', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeFontSize', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeFontColor', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeFontColor', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeFontX', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeFontX', type, status))
    })    
#-------------------------------------------------------------------------------
setMethod ('getNodeFontY', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeFontY', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getNodeWeight', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc (obj@uri, 'RedHandler.getNodeWeight', type, status))
    })    
        
                
#Methods to set node attributes

#-------------------------------------------------------------------------------
setMethod ('setNodeAlias', 'RedPort', 
  function (obj, node, alias) { 
  	if(ping(obj)==0)return(invisible())
    node=as.character(node)
    alias=as.character(alias)
    return (xml.rpc(obj@uri, 'RedHandler.setNodeAlias', node, alias))
    })
#-------------------------------------------------------------------------------
setMethod ('setNodeXY', 'RedPort', 
  function (obj, node, x, y) { 
  	if(ping(obj)==0)return(invisible())
    c1=!is.numeric(x)
    c2=!is.numeric(y)
    if(c1 && c2){
        stop("Node coords. must be provided as numerics!")
    }
    if(sum(is.na(x))>0 || sum(is.na(y))>0 ){
        stop("Invalid node coords. declaration: 'NA' found'!")
    }     
    node=as.character(node) 
    return (xml.rpc(obj@uri, 'RedHandler.setNodeXY', node, x, y))
    }) 
#-------------------------------------------------------------------------------
setMethod ('setNodeBend', 'RedPort', 
  function (obj, node, bend) {  
  	if(ping(obj)==0)return(invisible())
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
    return (xml.rpc(obj@uri, 'RedHandler.setNodeBend', node, bend))
    }) 
#-------------------------------------------------------------------------------
setMethod ('setNodeSize', 'RedPort', 
  function (obj, node, size) {
  	if(ping(obj)==0)return(invisible())
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
    return (xml.rpc(obj@uri, 'RedHandler.setNodeSize', node, size))
    })
#-------------------------------------------------------------------------------
setMethod ('setNodeShape', 'RedPort', 
  function (obj, node, shape) { 
  	if(ping(obj)==0)return(invisible())
    node=as.character(node)
    shape=as.character(shape)
    return (xml.rpc(obj@uri, 'RedHandler.setNodeShape', node, shape))
    })    
#-------------------------------------------------------------------------------
setMethod ('setNodeColor', 'RedPort', 
  function (obj, node, color) {
  	if(ping(obj)==0)return(invisible())
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
            return (xml.rpc(obj@uri, 'RedHandler.setNodeColor', node, color))
        }      
      } 	
   }) 
#-------------------------------------------------------------------------------
setMethod ('setNodeLineWidth', 'RedPort', 
  function (obj, node, width) {  
  	if(ping(obj)==0)return(invisible())
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
    return (xml.rpc(obj@uri, 'RedHandler.setNodeLineWidth', node, width))
    }) 
#-------------------------------------------------------------------------------
setMethod ('setNodeLineColor', 'RedPort', 
  function (obj, node, color) { 
  	if(ping(obj)==0)return(invisible())
    node=as.character(node)
    color=as.character(color)
    return (xml.rpc(obj@uri, 'RedHandler.setNodeLineColor', node, color))
    })    
#-------------------------------------------------------------------------------
setMethod ('setNodeFontName', 'RedPort', 
  function (obj, node, name) { 
  	if(ping(obj)==0)return(invisible())
    node=as.character(node)
    name=as.character(name)
    return (xml.rpc(obj@uri, 'RedHandler.setNodeFontName', node, name))
    })
#-------------------------------------------------------------------------------
setMethod ('setNodeFontStyle', 'RedPort', 
  function (obj, node, style) {
  	if(ping(obj)==0)return(invisible())
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
    return (xml.rpc(obj@uri, 'RedHandler.setNodeFontStyle', node, style))
    })    
#-------------------------------------------------------------------------------
setMethod ('setNodeFontSize', 'RedPort', 
  function (obj, node, size) { 
  	if(ping(obj)==0)return(invisible())
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
    return (xml.rpc(obj@uri, 'RedHandler.setNodeFontSize', node, size))
    })
#-------------------------------------------------------------------------------
setMethod ('setNodeFontColor', 'RedPort', 
  function (obj, node, color) { 
  	if(ping(obj)==0)return(invisible())
    node=as.character(node)
    color=as.character(color)
    return (xml.rpc(obj@uri, 'RedHandler.setNodeFontColor', node, color))
    })
#-------------------------------------------------------------------------------
setMethod ('setNodeFontXY', 'RedPort', 
  function (obj, node, x, y) {  
  	if(ping(obj)==0)return(invisible())
    c1=!is.numeric(x)
    c2=!is.numeric(y)
    if(c1 && c2){
        stop("Node 'font coords.' must be provided as numerics!")
    }
    if(sum(is.na(x))>0 || sum(is.na(y))>0 ){
        stop("Invalid 'font coords.' declaration: 'NA' found'!")
    }   
    node=as.character(node)   
    return (xml.rpc(obj@uri, 'RedHandler.setNodeFontXY', node, x, y))
    })
#-------------------------------------------------------------------------------
setMethod ('setNodeWeight', 'RedPort', 
  function (obj, node, weight) { 
  	if(ping(obj)==0)return(invisible())  
    c1=!is.numeric(weight)
    if(c1){
        stop("Node 'weight' must be provided as numerics!")
    }
    if(sum(is.na(weight))>0){
        stop("Invalid node 'weight' declaration: 'NA' found'!")
    }         
    node=as.character(node) 
    return (xml.rpc(obj@uri, 'RedHandler.setNodeWeight', node, weight))
    })
    

#Methods to add/delete nodes and manipulate containers and nested objects

#-------------------------------------------------------------------------------
setMethod ('addNodes', 'RedPort', 
  function (obj, nodes) { 
  	if(ping(obj)==0)return(invisible())
  	node=as.character(nodes)
    return (xml.rpc(obj@uri, 'RedHandler.addNodes', nodes))
    })
#-------------------------------------------------------------------------------
setMethod ('deleteNodes', 'RedPort', 
  function (obj, nodes) { 
  	if(ping(obj)==0)return(invisible())
  	node=as.character(nodes)
    return (xml.rpc(obj@uri, 'RedHandler.deleteNodes', nodes))
    })    
    
#-------------------------------------------------------------------------------
setMethod ('nestNodes', 'RedPort', 
  function (obj, nodes, nestImage ='plain', isAssign=TRUE, isAnchor=FALSE, gscale=40, gcoord=NULL, parent=NULL, 
  gatt=list(), theme=c('tm0','tm1','tm2','tm3','tm4','tm5')) { 
  	if(ping(obj)==0)return(invisible())
  	
	#Further checks---------------------------------------------------- 
	if(!is.list(gatt)){
		stop("NOTE: 'gatt' must be a list of graph attributes (e.g. gatt$nestColor, gatt$gscale...)!")
	}
	if(theme[1]=='tm1'){
		if(is.null(gatt$nestShape))gatt$nestShape='ROUNDED_RECTANGLE'
		if(is.null(gatt$nestLineWidth))gatt$nestLineWidth=15
	}	
	if(theme[1]=='tm2'){
		if(is.null(gatt$nestShape))gatt$nestShape='ROUNDED_RECTANGLE'
		if(is.null(gatt$nestColor))gatt$nestColor='#ffffff'
		if(is.null(gatt$nestLineWidth))gatt$nestLineWidth=15
		if(is.null(gatt$nestLineColor))gatt$nestLineColor='#000000'
		if(is.null(gatt$nestLineType))gatt$nestLineType='DOTTED'
		if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
	}	
	if(theme[1]=='tm3'){
		if(is.null(gatt$nestImage))gatt$nestImage='transparent'
		if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
	}	
	if(theme[1]=='tm4'){
		if(is.null(gatt$nestImage))gatt$nestImage='hide'
		if(is.null(gatt$isAnchor))gatt$isAnchor=TRUE
	}	
	if(theme[1]=='tm5'){
		if(is.null(gatt$nestShape))gatt$nestShape='ROUNDED_RECTANGLE'
		if(is.null(gatt$nestColor))gatt$nestColor='#ffffff'
		if(is.null(gatt$nestLineWidth))gatt$nestLineWidth=5
		if(is.null(gatt$nestLineColor))gatt$nestLineColor='#000000'
		if(is.null(gatt$nestLineType))gatt$nestLineType='DOTTED'
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
	if(!is.null(gatt)){
		message('*** Uploading nest attributes ...')
	}
	#Nest aliases
	if(is.character(gatt$nestAlias) && length(gatt$nestAlias)>0){
		message("** ... nest 'alias'")
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
			message("** ... nest 'shape'") 
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
			message("** ... nest 'line type'") 
			charAtt[3]=gatt$nestLineType
		}      
	} else if(!is.null(gatt$nestLineType)){ 
		warning("NOTE: nest 'line type' must be provided as character!")
	}	
	#Nest color
	if(is.character(gatt$nestColor)){
		gatt$nestColor=gatt$nestColor[1]
		if(is.na(gatt$nestColor)){
			warning("NOTE: invalid nest 'color' declaration: 'NA' found'!")
		} else if(nchar(gatt$nestColor)>9){
			warning("NOTE: invalid nest 'color' specification: not 'rgb' space! (ps. alpha not supported)")
		} else {            
			if(nchar(gatt$nestColor)>7) gatt$nestColor=substr(gatt$nestColor,0,7)
			message("** ... nest 'color'")
			charAtt[4]=gatt$nestColor
		}      
	} else if(!is.null(gatt$nestColor)){ 
		warning("NOTE: nest 'color' must be provided as character (hexadecimal)!")
	}
	#Nest line color
	if(is.character(gatt$nestLineColor)){
		gatt$nestLineColor = gatt$nestLineColor[1]
		if(is.na(gatt$nestLineColor)){
			warning("NOTE: invalid nest 'line color' declaration: 'NA' found'!")
		} else if(nchar(gatt$nestLineColor)>9){
			warning("NOTE: invalid nest 'line color' specification: not 'rgb' space! (ps. alpha not supported)")
		} else {            
			message("** ... nest 'line color'") 
			if(nchar(gatt$nestLineColor)>7)gatt$nestLineColor=substr(gatt$nestLineColor,0,7)
			charAtt[5]=gatt$nestLineColor
		}      
	} else if(!is.null(gatt$nestLineColor)){ 
		warning("NOTE: nest 'line color' must be provided as character (hexadecimal)!")
	}
	#Nest font color
	if(is.character(gatt$nestFontColor)){
		gatt$nestFontColor = gatt$nestFontColor[1]
		if(is.na(gatt$nestFontColor)){
			warning("NOTE: invalid nest 'font color' declaration: 'NA' found'!")
		} else if(nchar(gatt$nestFontColor)>9){
			warning("NOTE: invalid nest 'font color' specification: not 'rgb' space! (ps. alpha not supported)")
		} else {            
			message("** ... nest 'line color'") 
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
			message("** ... nest font 'coords'")
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
			message("** ... nest font 'size'") 
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
			message("** ... nest 'line width'") 
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
			message("** ... nest 'size'") 
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
			message("** ... nest 'gscale'") 
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
			message("** ... nest 'gcoord'") 
			numericAtt[7]=gatt$gcoord[1]
			numericAtt[8]=gatt$gcoord[2]
		}
	  } 	
	}	
			   
    ref =xml.rpc(obj@uri, 'RedHandler.nestNodes', nodes, status1, status2, status3, charAtt, numericAtt )      
    invisible( updateGraph(obj) )
    return(ref)  
    
    })
#-------------------------------------------------------------------------------
setMethod ('updateContainerSize', 'RedPort', 
  function (obj) { 
  	if(ping(obj)==0)return(invisible())
    return (xml.rpc(obj@uri, 'RedHandler.updateContainerSize'))
    })
#-------------------------------------------------------------------------------
setMethod ('mergeOutEdges', 'RedPort', 
  function (obj,isNorm=TRUE, lb=NULL, ub=NULL) { 
  	if(ping(obj)==0)return(invisible())
  	if(is.logical(isNorm)){
  		isNorm=ifelse(isNorm,'true','false')
  	} else {
  		isNorm='true'
  	}
  	lb=lb[1]
  	ub=ub[1]
  	isNull="false"
  	if(!is.numeric(lb) || !is.numeric(ub)){
  		lb=0
  		ub=0
  		isNull="true"
  	}
    res=xml.rpc(obj@uri, 'RedHandler.mergeContainerOutEdges', isNorm, lb, ub, isNull)
    invisible( updateGraph(obj) )
    res
    })        
#-------------------------------------------------------------------------------
setMethod ('getContainerComponets', 'RedPort', 
  function (obj, container) { 
  	if(ping(obj)==0)return(invisible())
    container=as.character(container)
    return (xml.rpc(obj@uri, 'RedHandler.getContainerComponets', container))
    })
#-------------------------------------------------------------------------------
setMethod ('mergeNodes', 'RedPort', 
  function (obj, nodes) { 
  	if(ping(obj)==0)return(invisible())
  	node=as.character(nodes)
    return (xml.rpc(obj@uri, 'RedHandler.mergeNodes', nodes))
    })


#Methods to get edge attributes

#-------------------------------------------------------------------------------
setMethod ('getEdges', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getEdges', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getSourceEdges', 'RedPort', 
  function (obj, type="node", status="all") {
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getSourceEdges', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getTargetEdges', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getTargetEdges', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getEdgeIDs', 'RedPort', 
  function (obj, type="node", status="all") {
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getEdgeIDs', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getSourceEdgeIDs', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getSourceEdgeIDs', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getTargetEdgeIDs', 'RedPort', 
  function (obj, type="node", status="all") {
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getTargetEdgeIDs', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getArrowDirection', 'RedPort', 
  function (obj, type="node", status="all") {
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getArrowDirection', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getEdgeWidth', 'RedPort', 
  function (obj, type="node", status="all") {
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getEdgeWidth', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getEdgeColor', 'RedPort', 
  function (obj, type="node", status="all") {
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getEdgeColor', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getEdgeType', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getEdgeType', type, status))
    })
#-------------------------------------------------------------------------------
setMethod ('getEdgeWeight', 'RedPort', 
  function (obj, type="node", status="all") { 
  	if(ping(obj)==0)return(NULL)
    return (xml.rpc(obj@uri, 'RedHandler.getEdgeWeight', type, status))
    })
    
    
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
    return (xml.rpc(obj@uri, 'RedHandler.setArrowDirection', 
            nodeA, nodeB, direction))
    })
#-------------------------------------------------------------------------------
setMethod ('setEdgeWidth', 'RedPort', 
  function (obj, nodeA, nodeB, width) {  
  	if(ping(obj)==0)return(invisible())
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
    return (xml.rpc(obj@uri, 'RedHandler.setEdgeWidth', nodeA, nodeB, width))
    })
#-------------------------------------------------------------------------------
setMethod ('setEdgeColor', 'RedPort', 
  function (obj, nodeA, nodeB, color) {
  	if(ping(obj)==0)return(invisible())
    nodeA=as.character(nodeA)
    nodeB=as.character(nodeB)  
    color=as.character(color)
    return (xml.rpc(obj@uri, 'RedHandler.setEdgeColor', nodeA, nodeB, color))
    })
#-------------------------------------------------------------------------------
setMethod ('setEdgeType', 'RedPort', 
  function (obj, nodeA, nodeB, type) { 
  	if(ping(obj)==0)return(invisible())
    nodeA=as.character(nodeA)
    nodeB=as.character(nodeB)  
    type=as.character(type)
    return (xml.rpc(obj@uri, 'RedHandler.setEdgeType', nodeA, nodeB, type))
    })
#-------------------------------------------------------------------------------
setMethod ('setEdgeWeight', 'RedPort', 
  function (obj, nodeA, nodeB, weight) {  
  	if(ping(obj)==0)return(invisible())
    c1=!is.numeric(weight)
    if(c1){
        stop("Edge 'weight' must be provided as numerics!")
    }
    if(sum(is.na(weight))>0){
        stop("Invalid edge 'weight' declaration: 'NA' found'!")
    }      
    nodeA=as.character(nodeA)
    nodeB=as.character(nodeB)
    return (xml.rpc(obj@uri,'RedHandler.setEdgeWeight', nodeA, nodeB, weight))
    })


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
    return (xml.rpc (obj@uri, 'RedHandler.addEdges', edges))
    })
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
    return (xml.rpc (obj@uri, 'RedHandler.deleteEdges', edges))
    })    
#-------------------------------------------------------------------------------
setMethod ('addEdgeBetweenContainers', 'RedPort', 
  function (obj, containerA, containerB) { 
  	if(ping(obj)==0)return(invisible())
    containerA=as.character(containerA)
    containerB=as.character(containerB)   
    return (xml.rpc (obj@uri, 'RedHandler.addEdgeBetweenContainers', 
            containerA, containerB))
    })



#Further methods to manipulate edges and nodes

#-------------------------------------------------------------------------------
setMethod ('selectEdges', 'RedPort', 
  function (obj, nodeA, nodeB) {   
  		if(ping(obj)==0)return(invisible())
    	nodeA=as.character(nodeA)
    	nodeB=as.character(nodeB)
    	deSelectEdges(obj)  #deselect all edges previously to the call!    
    	invisible(xml.rpc (obj@uri, 'RedHandler.selectEdges', nodeA, nodeB))
    })
#-------------------------------------------------------------------------------
setMethod ('selectNodes', 'RedPort', 
  function (obj, nodes) { 
  		if(ping(obj)==0)return(invisible())
  		nodes=as.character(nodes)
  		deSelectNodes(obj)#deselect all nodes previously to the call!
    	invisible (xml.rpc (obj@uri, 'RedHandler.selectNodes', nodes))
    })
#-------------------------------------------------------------------------------
setMethod ('selectAllEdges', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.selectAllEdges'))
    })
#-------------------------------------------------------------------------------
setMethod ('selectAllNodes', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.selectAllNodes'))
    })
#-------------------------------------------------------------------------------
setMethod ('selectGraph', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.selectGraph'))
    })
#-------------------------------------------------------------------------------
setMethod ('deSelectEdges', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.deSelectEdges'))
    })
#-------------------------------------------------------------------------------
setMethod ('deSelectNodes', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.deSelectNodes'))
    })
#-------------------------------------------------------------------------------
setMethod ('deSelectGraph', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.deSelectGraph'))
    })
#-------------------------------------------------------------------------------
setMethod ('deleteSelectedEdges', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.deleteSelectedEdges'))
    })
#-------------------------------------------------------------------------------
setMethod ('deleteSelectedNodes', 'RedPort', 
  function (obj) {
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.deleteSelectedNodes'))
    })
#-------------------------------------------------------------------------------
setMethod ('isDynamicsActive', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	return (xml.rpc (obj@uri, 'RedHandler.isDynamicsActive'))
    })


#Methods to build RedeR plugins
  
#-------------------------------------------------------------------------------
setMethod ('deletePlugin', 'RedPort', 
  function (obj, pluginName) { 
  		if(ping(obj)==0)return(invisible())
    	pluginName=as.character(pluginName)      
    	invisible (xml.rpc (obj@uri, 'RedHandler.deletePlugin', pluginName))
    }) 
   
#-------------------------------------------------------------------------------
setMethod ('updatePlugins', 'RedPort', 
  function (obj) {   
  		if(ping(obj)==0)return(invisible())
    	invisible (xml.rpc (obj@uri, 'RedHandler.updatePlugins'))
    })    
 
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
    invisible (xml.rpc (obj@uri, 'RedHandler.buildPlugin', pluginTitle, 
            pluginMethods, pluginAddons))
    })


#Map hclust to RedeR app
#-------------------------------------------------------------------------------
setMethod ('nesthc', 'RedPort', 
	function(obj, hc, cutlevel=2, metric=c("rootdist","leafdist","height"), ncomp=3, nlev=2, grid=c(2,3),
		gridScale=75, gscale=c(30,75,45), gatt=list(), theme='tm5', isAssign=TRUE, isAnchor=TRUE, 
		order=c("topdown","bottomup"), getcounts=FALSE ){
				
		if(ping(obj)==0)return(invisible())
		
    	#check hclust object-----------------------------------------------
    	if(class(hc)!="hclust"){
        	stop("Not a hclust object!")
    	}    

		#check args--------------------------------------------------------
		if(!is.character(metric))metric="rootdist"
		tp=switch(metric[1], rootdist=1, leafdist=2, height=3)
		if(is.null(tp))metric="rootdist"
		metric=metric[1]
				
		#this feature is not implemented yet!
		if(!is.character(order))order="topdown"
		tp=switch(order[1], topdown =1,bottomup=2)
		if(is.null(order))order="topdown"
		#further checks---------------------------------------------------- 
		if(!is.list(gatt)){
			stop("NOTE: 'gatt' must be a list of graph attributes (e.g. gatt$nestColor, gatt$gscale...)!")
		}
		if(is.null(gatt$nestLineWidth) || !is.numeric(gatt$nestLineWidth))gatt$nestLineWidth=5
		lwidth=gatt$nestLineWidth[1]
		
		if(!is.numeric(cutlevel))cutlevel=2
		cutlevel=cutlevel[1]
		if(!is.numeric(ncomp) || ncomp<=0)ncomp=3
		ncomp=ncomp[1]		
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
	    #Re-scale hc height to [0-1]
	    tp=sort(hc$height)[1]
	    if(tp>0){
	    	hc$height=hc$height-tp
	    } else if(tp<0){
	    	hc$height=hc$height+abs(tp)
	    }
	    tp=sort(hc$height)[length(hc$height)]
	    hc$height=hc$height/tp
	    #return results
	    C=as.numeric(C)
	    D=as.numeric(D)
	    E=as.numeric(E)
	    N=hc$merge>0
	    N=N[,1]+N[,2]
	    return(list(nest=nest,labels=hc$labels,parent=D,leafdist=C,rootdist=E,height=hc$height,nnest=N))
		}
		
		#computa mapa e estima numero de ninhos abaixo do nivel do corte
    	tm=treemap(hc)
    	nn=length(tm$nest)
  		if(metric=="rootdist"){
  			nestcount=sum(tm[[metric]]<=cutlevel)
  		} else {
  			nestcount=sum(tm[[metric]]>=cutlevel)
  		}
    	
    	#estima grid
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
    		gbasic=graph.empty(n=nestcount,directed=FALSE)  
        	layout=layout.norm(layout.circle(gbasic), xmin = 25, xmax=75, ymin=25, ymax=75) 
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
        # 'update="default"' forces to keep old node coords and not to add new containers!
    	if(is.null(update) || !is.character(update))update=NULL
    	# internal function (locks DragAndZoon interactivity while sending the subgraph list to the data bank)
  		invisible(xml.rpc (obj@uri,'RedHandler.lockDragAndZoom'))
  		if(!is.null(zoom))invisible( xml.rpc (obj@uri, 'RedHandler.setZoom',zoom) )
  		checknd=getGraph(obj,attribs="minimal")
  		checknd=V(checknd)$name
  		
  		#compute dist to nest root (after cut!)
  		val=tm[[metric]]
  		if(metric=="rootdist"){
  			selec=as.numeric(val>=cutlevel)
  		} else {
  			selec=as.numeric(val<=cutlevel)
  		}
  		tp=tm$parent
  		td=selec[tp]
  		td[is.na(td)]=0
  		distrt=td
  		while(sum(td)>0){
  			tp=tm$parent[tp]
  			td=selec[tp]
  			td[is.na(td)]=0
  			distrt=distrt+td
  		}
  		distrt=distrt+selec
  		
    	#add tree ----------------------------------------------------------
    	gs0=gscale[1]
    	gs1=gscale[2]
    	gs2=gscale[3]
    	gc1=c(58,58)
    	gc2=c(70,70)
    	gc3=c(30,30)
    	stats=data.frame()
    	if(order[1]=="topdown"){
    	  nid=rep(NA,nn)
    	  k=1
    	  for(i in nn:1){
    	  	val=distrt[i]
    		if(val>0 && val<=nlev){  			
    			nodes=tm$nest[[i]]
    			if(sum(nodes%in%checknd)>=ncomp){
	    			idx=tm$parent[i]
	  				if(!is.na(nid[idx]) && isAssign){
	  					nodes=paste(nodes,".$", nid[idx],sep="")
	  				}
	  				if(is.na(nid[idx])){	
	  					gs=gs0
	  					gc=c(layout[k,1],layout[k,2])
	  					k=k+1				
	  				} else if(tm$nnest[idx]<2){
	   					gs=gs1
	  					gc=gc1  				
	  				} else if(tm$nnest[idx]==2){
	  					gs=gs2
	  					gc=gc2
	  					tm$nnest[idx]=3
	  				} else if(tm$nnest[idx]==3) {
	  					gs=gs2
	  					gc=gc3  				
	  				}
	  				#scale nest.line.width by n. levels (just to get a better image!) 
	  				gatt$nestLineWidth=(lwidth/2)+((lwidth/2)*(1/max(1,distrt[i])))
	  				#send nested nodes!
	    			nid[i]=nestNodes(obj, nodes, nestImage='plain', isAssign=isAssign, isAnchor=isAnchor, 
	    					gscale=gs, gcoord=gc, gatt=gatt, theme=theme)
	    			stats=rbind(stats, data.frame(nest.id=nid[i],nest.size=length(nodes),leafdist=tm$leafdist[i],
	    				  dendoroot.dist=tm$rootdist[i],height=tm$height[i],nestroot.dist=distrt[i]))
    			}
    		}
    	 } 
    	} else { 
    	  gc=gc1
    	  gs=gs1
    	  labs=hc$labels    		
    	  for(i in 1:nn){
			val=distrt[i]
    		if(val>0 && val<=nlev){ 
    			nodes=tm$nest[[i]]
    			if(sum(nodes%in%checknd)>=ncomp){
	    			nodes=labs[hc$labels%in%nodes]
	    			nds=unique(nodes)
	    			if(length(nds)<length(nodes)){
	    				gs=NULL
	    				gc=round(runif(2,min=20,max=80))
	    			} else {
	    				gs=gs1
	    				gc=gc1
	    			}
	    			nid=nestNodes(obj, nds, nestImage='plain', isAssign=isAssign, isAnchor=isAnchor, 
	    					gscale=gs, gcoord=gc, gatt=gatt, theme=theme)
	    			labs[labs%in%tm$nest[[i]]]=nid
	    			stats=rbind(stats, data.frame(nest.id=nid[i],nest.size=length(nodes),leafdist=tm$leafdist[i],
	    				  dendoroot.dist=tm$rootdist[i],height=tm$height[i],nestroot.dist=distrt[i]))
    			}
    		}
    	  }   	 	
      }  
	  #Internal function (unlocks DragAndZoon interactivity after send subgraph list)
   	  invisible(xml.rpc (obj@uri,'RedHandler.unLockDragAndZoom'))     	
   	  if(getcounts)return(stats)
  }
  )



