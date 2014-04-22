#-------------------------------------------------------------
#Simple function to generate random graphs with a modular structure
gtoy.rm<-function(m=3, nmax=30, nmin=3, p1=0.5, p2=0.05, p3=0.9){
	#check args------------------------------------
	if(!is.numeric(m) || m<1){
		stop("NOTE: 'm' must be a number > 0!")
	}
	m=m[1]
	if(!is.numeric(nmax)){
		stop("NOTE: 'nmax' must be a number > 0!")
	}
	nmax=nmax[1]
	if(!is.numeric(nmin) || nmin>=nmax){
		stop("NOTE: 'nmin' must be a number > nmax!")
	}
	nmin=nmin[1]
	if(!is.numeric(p1) || p1>=1 || p1<=0){
		stop("NOTE: 'p1' must be numeric (0.0 < p < 1.0)!")
	}	
	p1=p1[1]
	if(!is.numeric(p2) || p2>=1 || p2<=0){
		stop("NOTE: 'p2' must be numeric (0.0 < p < 1.0)!")
	}		
	p2=p2[1]	
	if(!is.numeric(p3) || p3>=1 || p3<=0){
		stop("NOTE: 'p3' must be numeric (0.0 < p < 1.0)!")
	}
	p3= p3[1]
	nver=function(nmax,p1){sum(runif(nmax)>1-p1)}
	gg=igraph::graph.empty(n=0, directed=FALSE)
	mdmap=c()	
	for(i in 1:m){
		v=max(nmin,nver(nmax,p1))
		g=igraph::erdos.renyi.game(n=v, p.or.m=p3, type="gnp", directed=FALSE)
		gg=igraph::graph.disjoint.union(gg,g)
		mdmap=c(mdmap,rep(i,v))		
	}
	adj=igraph::get.adjacency(gg,sparse=FALSE)
	adj[,]=0
	adj[,]=runif(nrow(adj)*ncol(adj))
	adj[adj<(1-(p2/m))]=0
	for(i in 1:m){
		adj[mdmap==i,mdmap==i]=0
	}
	adj[adj>0]=1
	adj=adj+igraph::get.adjacency(gg, sparse=FALSE)
	gg=igraph::graph.adjacency(adj, mode="undirected",diag=FALSE)
	gg=igraph::simplify(gg, remove.multiple = TRUE, remove.loops = TRUE)
	V(gg)$name=paste("n",1:igraph::vcount(gg),sep="")
	cols=terrain.colors(m)
	V(gg)$nodeColor=cols[mdmap]
	V(gg)$module.id=mdmap
	return(gg)
}

#-------------------------------------------------------------
# simplified remote calls for RedeR!
# designed for internal handler, only for simple calls!
rederpost<-function(url, RedHandler, ..., .args=list(...),.curl=getCurlHandle()){
  aXML<-function(method, .args){
    node<-newXMLNode("methodCall", newXMLNode("methodName", method))
    params<-newXMLNode("params", parent=node)
    nattrs<-function(x)newXMLNode("value",newXMLNode("string",newXMLCDataNode(x)))
    sapply(.args,function(x)newXMLNode("param",nattrs(x),parent=params))
    node
  }
  bXML<-function(node){
    if(is.null(node))return(node)
    if(xmlName(node)=="value")node<-node[[1]]
    if(is(node, "XMLInternalTextNode"))return(xmlValue(node))
    type<-xmlName(node)
    if(type=='array'){
      node<-xmlApply(node[["data"]], function(x) bXML(x[[1]]))
      bl<-!is.list(node[[1]]) && all(sapply(node, typeof)==typeof(node[[1]]))
      if(bl) node<-structure(unlist(node),names=NULL)
    } else {
      node<-xmlValue(node)
    }
    if(type=="i4"||type=="int")node<-as.integer(node)
    if(type=="double")node<-as.numeric(node)
    node
  }
  cXML<-dynCurlReader(.curl, baseURL=url)
  dXML<-list(httpheader=c('Content-Type'="text/xml"), 
             followlocation=TRUE, useragent="R-XMLRPC",
             postfields=saveXML(aXML(RedHandler,.args)),
             headerfunction=cXML$update)
  postForm(url,.opts=dXML,style="POST",curl=.curl)
  rdcall<-cXML$value()
  rdcall<-xmlParse(rdcall,asText=TRUE)
  rdcall<-xpathApply(rdcall, "//param/value", bXML)
  if(length(rdcall)==1)rdcall<-rdcall[[1]]
  rdcall
}

#-------------------------------------------------------------
# express post: direct calls to RedeR 
# improve loading performance for large/sequential objects!
# designed for internal handler, only for large and complex calls!
rederexpresspost<-function(uri, method, ..., gdata=list(...), hdl=getCurlHandle()){	
	getminimumxml<-function(x, method){
		getserial<-function(x){
			if(is.character(x)){
				x<-gsub("&","&amp;",x)
				x<-gsub("<","&lt;",x)
				x<-gsub(">","&gt;",x)
			}
			type<-c("integer"="double", "double"="double", "character"="string")[typeof(x)]
			head<-paste("<value><",type,">",sep="",collapse="")
			tail<-paste("</",type,"></value>",sep="",collapse="")
			doc<-paste(head,x,tail,sep="",collapse="")
			if(length(x)==1){
				paste("<param>",doc,"</param>",sep="", collapse="")					
			} else {
				paste("<param><value><array><data>",doc,
				"</data></array></value></param>",sep="", collapse="")				
			}
		}
		doc<-sapply(x, function(x) getserial(x) )
		doc<-paste(doc,sep="", collapse="")
		doc<-paste("<params>",doc,"</params>",sep="", collapse="")	
		mt<-paste("<methodName>",method,"</methodName>", sep="", collapse="")
		doc<-paste("<methodCall>",mt,doc,"</methodCall>",sep="", collapse="")
		list(httpheader=c('Content-Type'="text/xml"),postfields=doc)	
	}	
	doc<-getminimumxml(gdata, method)
	p<-postForm(uri=uri, .opts=doc, style="POST", curl=hdl)
	p<-xmlParse(p, asText = TRUE)
	xmlValue(xmlChildren(p)[[1]])	
}

#------------------------------------------------------------------------------
# Set RedeR att. to vertices in igraph objects
#..todo: patela 2 somente aceita vetor de cores de numero par!!!
att.setv=function(g=NULL, from='name', to='nodeColor', pal=1, cols=NULL, na.col=grey(0.7), 
                  xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, isrev=FALSE, getleg=TRUE, 
                  roundleg=1,title=NULL){
  #check loaded igraph
  igraph.check()
	# set att---------------------------------------------------------
	coltype=c('nodeColor','nodeLineColor','nodeFontColor')
	numtype=c('nodeSize','nodeLineWidth','nodeFontSize','nodeBend','coordX','coordY')
	defaultatt=c(coltype, numtype,'nodeAlias','nodeShape')
	defaultshapes=c('ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND')	
	d1='name, id, or hexadecimal <string or integer>'
	d2='[0,+inf) <numeric>'
	d3='[0,100] <numeric>'
	d4='(-inf,+inf) <numeric>'
	d5='name or id <string or integer>'
	description=c(d1,d1,d1,d2,d2,d2,d3,d4,d4,d5,d5)
	if(is.null(g)){
		message("*List of attributes handled by 'att.setv' function:")
		print(cbind(attribute=defaultatt,id=1:11,description=description),quote=F)
		message("*Shape names and ids:")
		print(cbind(shape=defaultshapes,id=1:5),quote=F)
		return(invisible())
	}
	# check igraph object and main args---------------------------------
	if(!igraph::is.igraph(g)){
	  stop("Not an igraph object!")
	}
	if(!is.character(from))stop("NOTE: arg. 'from' should be a string!")
	from=from[1]
	if(!is.character(to) && !is.numeric(to) && !is.integer(to) )stop("NOTE: arg. 'to' should be a string or an integer!")
	to=to[1]
	# get ref. att---
	fromatt=igraph::get.vertex.attribute(g, from)
	if(is.null(fromatt) || length(fromatt)!=igraph::vcount(g)){
		stop(paste("NOTE: graph attribute '",from,"' is absent or not consistent with node count!",sep=""))
	}
	if(is.numeric(to) || is.integer(to)){
		to=as.integer(to)
		if(to<1 || to>length(defaultatt)){
			message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
			message("*List of attributes handled by 'att.setv' function:")
			print(cbind(attribute=defaultatt,id=1:11,description=description),quote=F)
			message("*Shape names and ids:")
			print(cbind(shape=defaultshapes,id=1:5),quote=F)
			return(g)
		}
		to=defaultatt[to]
	}
	if(!to%in%defaultatt){
		message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
		message("*List of attributes handled by 'att.setv' function:")
		print(cbind(attribute=defaultatt,id=1:11,description=description),quote=F)
		message("*Shape names and ids:")
		print(cbind(shape=defaultshapes,id=1:5),quote=F)
		return(g)
	}
	if(!is.numeric(fromatt) && !is.integer(fromatt) && !is.character(fromatt)){
		ms=paste("NOTE: graph attribute '",from,"' is not consistent with supported data types: character, numeric or integer!",sep="")
		stop(ms)
	}
	# more checks--------------------------------------------------------
	if(!is.null(breaks) && !is.numeric(breaks))stop("NOTE: arg. 'breaks' should be numeric!")
	if(!is.numeric(pal) && !is.integer(pal))stop("arg. 'pal' (pallete) should be an integer!");pal=pal[1]
	if(!pal%in%c(1,2))stop("NOTE: not a valid pallete (pal)! options: 1 or 2!")
	if(!is.numeric(xlim) && !is.integer(xlim))stop("NOTE: arg. 'xlim' should be numeric!")
	if(length(xlim)<3)stop(paste("NOTE: 'xlim=",xlim,"' is not consistent with expected arg. length! ...3!",sep=""))
	if(sum(is.null(xlim)>0))stop("NOTE: 'xlim' arg. does not support null values!")
	if(!is.logical(isrev))stop("NOTE: 'isrev' arg. should be a logical value!")
	if(!is.null(nquant) && !is.numeric(nquant))stop("NOTE: 'nquant' arg. should be an integer!");nquant=nquant[1]
	if(is.null(title) || !is.character(title) )title<-from
	#----------------------------------------	
	# main functions to set attribute scales!
	#----------------------------------------	
	#--simple color palette for 'category' or 'enumerated' data type (i.e. factor levels)
	colorcategory=function(x,cols,na.col,categvec,isrev){
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(isrev)cols=rev(cols)	
		# compute mapping
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec,ordered=FALSE)
		}			
		cols=colorRampPalette(colors=cols)(nlevels(x))
		x.col=cols[x]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get scale (for any legend) and return results
		leg=list(scale=cols,legend=levels(x))
		res=list(res=x.col,leg=leg)
		return(res)
	}	
	#--color scale palette with breaks
	colorscale1=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks<-quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		# adjust breaks and get palette
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		cols=colorRampPalette(colors=cols)(length(bkcenter)-1)
		# set colors to x
		x.col=rep(NA,length(x))
		cuts=cut(x[!is.na(x)], breaks=bkcenter,include.lowest=TRUE)
		x.col[!is.na(x)]=cols[as.integer(cuts)]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get intervals
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}		
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)	
		leg=list(scale=cols,legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)	
		return(res)
	}
	#--neg/pos color scale palette with breaks (left/right)
	colorscale2=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check args
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks<-quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.null(cols))cols=c("darkblue","white","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2		
		# check color vec		
		lt=length(cols)
		if(lt/2==as.integer(lt/2))lt=lt+1	
		cols=colorRampPalette(colors=cols)(lt)
		lfrt=as.integer(lt/2)+1
		# get neg/pos colors
		negCols=cols[1:lfrt]
		posCols=cols[lfrt:lt]
		ct.col=cols[lfrt]
		# check and adjust breaks
		lt=length(bkcenter)
		if(lt/2==as.integer(lt/2)){
			lf=lt/2
			rt=(lt/2)+1
			center=(bkcenter[lf]+bkcenter[rt])/2
			negBreaks=c(-Inf,bkcenter[1:lf],center)
			posBreaks=c(center,bkcenter[rt:lt],+Inf)
		} else {
			lfrt=as.integer(lt/2)+1
			center=bkcenter[lfrt]
			negBreaks=c(-Inf,bkcenter[1:lfrt])
			posBreaks=c(bkcenter[lfrt:lt],+Inf)
		}
		# set main palettes	
		negCols=colorRampPalette(colors=negCols)(length(negBreaks))[-length(negBreaks)]
		posCols=colorRampPalette(colors=posCols)(length(posBreaks))[-1]
		# set minor palettesscale
		na.col=colorRampPalette(colors=c(na.col,na.col))(1)
		ct.col=colorRampPalette(colors=c(ct.col,ct.col))(1)		
		# set colors to x
		x.col=rep(NA,length(x))
		idx=x<center & !is.na(x)
		negcuts=cut(x[idx],breaks=negBreaks,include.lowest=TRUE)
		x.col[idx]=negCols[as.integer(negcuts)]
		idx=x>center & !is.na(x)
		poscuts=cut(x[idx],breaks=posBreaks)
		x.col[idx]=posCols[as.integer(poscuts)]
		x.col[x==center]=ct.col
		x.col[is.na(x.col)]=na.col
		# get intervals
		if(is.null(nquant)){
			interv=c(levels(negcuts),levels(poscuts))
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}
		testlen=length(breaks)/2
		if(as.integer(testlen)<testlen){
			idx=as.integer(testlen)+1
			breaks=breaks[c(1:idx,idx:length(breaks))]
		}
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=c(negCols,posCols),legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)
		return(res)
	}
	#--simple size scale for 'category' or 'enumerated' data type (i.e. factor levels)
	xcategory=function(x, szmin, szmax, na.sz, categvec, isrev){
		# set sz and return vec
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec)
		}		
		szlevs=seq(szmin, szmax, length.out=nlevels(x))
		if(isrev)szlevs=rev(szlevs)
		x.sz=szlevs[x]
		x.sz[is.na(x.sz)]=na.sz
		# get scale (for any legend) and return results
		leg=list(scale=szlevs,legend=levels(x))
		res=list(res=x.sz,leg=leg)
		return(res)		
	}
	#--size scale with breaks
	xscale=function(x,breaks,szmin,szmax,na.sz,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks<-quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		# adjust breaks
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		# get sz levels
		szlevs=seq(szmin,szmax,length.out=length(bkcenter)-1)
		if(isrev)szlevs=rev(szlevs)		
		# set sz to x
		x.sz=rep(NA,length(x))
		cuts=cut(x[!is.na(x)],breaks=bkcenter,include.lowest=TRUE)
		x.sz[!is.na(x)]=szlevs[as.integer(cuts)]
		x.sz[is.na(x.sz)]=na.sz
		# get interval
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}		
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=szlevs,legend=breaks,interval=interv)
		res=list(res=x.sz,leg=leg)
		return(res)
	}
	#--set node shapes for 'category' or 'enumerated' data type (i.e. factor levels)
	shapecategory=function(x,shapes,categvec,isrev){
		validnames=c('ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND')
		if(is.null(shapes)){
			shapes=validnames
		} else if(is.numeric(shapes) || is.integer(shapes)){
			shapes=as.integer(shapes)
			if(sum(sum(shapes<1),sum(shapes>5))>0){
				message("Error: NOTE: arg. 'shapes' with one or more inconsistent shape ids!")
				message("*List of shape names and ids handled by 'att.setv' function:")
				print(cbind(shape= validnames,id=1:5),quote=F)
				return(g)
			} 
			shapes=validnames[shapes]
		} else {
			if(sum(shapes%in%validnames)!=length(shapes)){
				message("Error: NOTE: arg. 'shapes' with one or more inconsistent shape names!")
				message("*List of shape names and ids handled by 'att.setv' function:")
				print(cbind(shape= validnames,id=1:5),quote=F)
				return(g)
			}			
		}
		if(sum(is.na(x))>0){
			stop("NOTE: 'NA' not supported for node shape mapping!")
		}
		if(isrev)shapes=rev(shapes)
		x=as.factor(x)
		if(length(levels(x))>length(shapes)){
			stop("NOTE: graph att. 'from' with more levels than shape options!")
		}
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			if(length(categvec)!=length(shapes)){
				if(length(categvec)>length(shapes)){
					stop("NOTE: arg. 'categvec' with more levels than shapes!")
				} else {
					shapes=shapes[1:length(categvec)]
				}
			}
			x=factor(x,levels=categvec)
		}
		att=shapes[x]
		shapes=shapes[1:nlevels(x)]
		# get shapes (for any legend) and return results
		leg=list(shape=shapes,legend=levels(x))
		res=list(res=att,leg=leg)
		return(res)
	}	
	# end of main functions!
	#-----------------------
	# map attribute to default names!---------------------------------------
	att=NULL
	if(to%in%coltype){
		if(is.null(breaks) && is.null(nquant)){
			att=colorcategory(fromatt,cols,na.col,categvec,isrev)
		} else {
			if(pal==1){
				att=colorscale1(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else if(pal==2){
				att=colorscale2(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else {
				att=colorcategory(fromatt,cols,na.col,categvec,isrev)
			}
		}
	} else if(to%in%numtype){
		szmin=xlim[1]
		szmax=xlim[2]
		na.sz=xlim[3]
		if(!to=='coordX' && !to=='coordY'){
			szmin=max(0,szmin)
			szmax=max(0,szmax)
			na.sz=max(0,na.sz)			
			if(to=='nodeBend'){
				szmin=min(100,szmin)
				szmax=min(100,szmax)
				na.sz=min(100,na.sz)
			}		
		}
		if(is.null(breaks) && is.null(nquant)){
			att=xcategory(fromatt,szmin,szmax,na.sz,categvec,isrev)
		} else {
			att=xscale(fromatt,breaks,szmin,szmax,na.sz,isrev,nquant,roundleg)
		}
	} else if(to=='nodeShape'){
		att=shapecategory(fromatt,shapes,categvec,isrev)
	} else if(to=='nodeAlias'){
		att=list()
		att$res=as.character(fromatt)
		getleg=FALSE
	}
	# return updated graph
	if(!is.null(att)){
		g=igraph::set.vertex.attribute(graph=g, name=to, value=att$res)
		if(is.logical(getleg) && getleg){
			to=gsub("\\b(\\w)","\\U\\1",to,perl=TRUE)
			leg=paste("leg",to,sep="")
			att$leg$title<-title
			g=igraph::set.graph.attribute(graph=g, name=leg, value=att$leg)
		}
	} else {
		message("...unable to conclude the command!")
	}
	return(g)
}


#------------------------------------------------------------------------------
# set RedeR att. to edges in igraph objects
att.sete=function(g=NULL, from='name', to='edgeColor', pal=1, cols=NULL, na.col=grey(0.7),
                  xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, 
                  isrev=FALSE, getleg=TRUE, roundleg=1,title=NULL){
  #check loaded igraph
  igraph.check()
	# set att---------------------------------------------------------
	coltype=c('edgeColor')
	numtype=c('edgeWidth','edgeWeight','arrowDirection')
	defaultatt=c(coltype, numtype, 'edgeType')
	defaultshapes=c('SOLID', 'DOTTED', 'DOTTED_SHORT', 'LONG_DASH')	
	d1='name, id, or hexadecimal <string or integer>'
	d2='[0,+inf) <numeric>'
	d3='0, 1, 2, or 3 <integer>'
	d4='name or id <string or integer>'
	description=c(d1,d2,d2,d3,d4)
	if(is.null(g)){
		message("*List of attributes handled by 'att.sete' function:")
		print(cbind(attribute=defaultatt,id=1:5,description=description),quote=F)
		message("*Edge types (names and ids):")
		print(cbind(type=defaultshapes,id=1:4),quote=F)
		return(invisible())
	}
    # check igraph object and main args---------------------------------
    if(!igraph::is.igraph(g)){
        stop("Not an igraph object!")
    }	
	if(!is.character(from))stop("NOTE: arg. 'from' should be a string!")
	from=from[1]
	if(!is.character(to) && !is.numeric(to) && !is.integer(to) )stop("NOTE: arg. 'to' should be a string or an integer!")
	to=to[1]
	# get ref. att---
	fromatt=igraph::get.edge.attribute(g, from)	
	if(is.null(fromatt) || length(fromatt)!=igraph::ecount(g)){
		stop(paste("NOTE: graph attribute '",from,"' is absent or not consistent with edge count!",sep=""))
	}
	if(is.numeric(to) || is.integer(to)){
		to=as.integer(to)
		if(to<1 || to>length(defaultatt)){
			message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
			message("*List of attributes handled by 'att.sete' function:")
			print(cbind(attribute=defaultatt,id=1:5,description=description),quote=F)
			message("*Shape names and ids:")
			print(cbind(shape=defaultshapes,id=1:4),quote=F)
			return(g)
		}
		to=defaultatt[to]
	}
	if(!to%in%defaultatt){
		message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
		message("*List of attributes handled by 'att.sete' function:")
		print(cbind(attribute=defaultatt,id=1:5,description=description),quote=F)
		message("*Shape names and ids:")
		print(cbind(shape=defaultshapes,id=1:4),quote=F)
		return(g)
	}
	if(!is.numeric(fromatt) && !is.integer(fromatt) && !is.character(fromatt)){
		ms=paste("NOTE: graph attribute '",from,"' is not consistent with supported data types: character, numeric or integer!",sep="")
		stop(ms)
	}
	# more checks--------------------------------------------------------
	if(!is.null(breaks) && !is.numeric(breaks))stop("NOTE: arg. 'breaks' should be numeric!")
	if(!is.numeric(pal) && !is.integer(pal))stop("arg. 'pal' (pallete) should be an integer!");pal=pal[1]
	if(!pal%in%c(1,2))stop("NOTE: not a valid pallete (pal)! options: 1 or 2!")
	if(!is.numeric(xlim) && !is.integer(xlim))stop("NOTE: arg. 'xlim' should be numeric!")
	if(length(xlim)<3)stop(paste("NOTE: 'xlim=",xlim,"' is not consistent with expected arg. length! ...3!",sep=""))
	if(sum(is.null(xlim)>0))stop("NOTE: 'xlim' arg. does not support null values!")
	if(!is.logical(isrev))stop("NOTE: 'isrev' arg. should be a logical value!")
	if(!is.null(nquant) && !is.numeric(nquant))stop("NOTE: 'nquant' arg. should be an integer!");nquant=nquant[1]
	if(is.null(title) || !is.character(title) )title<-from
	#----------------------------------------	
	# main functions to set attribute scales!
	#----------------------------------------	
	#--simple color palette for 'category' or 'enumerated' data type (i.e. factor levels)
	colorcategory=function(x,cols,na.col,categvec,isrev){
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(isrev)cols=rev(cols)	
		# compute mapping
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec,ordered=FALSE)
		}			
		cols=colorRampPalette(colors=cols)(nlevels(x))
		x.col=cols[x]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get scale (for any legend) and return results
		leg=list(scale=cols,legend=levels(x))
		res=list(res=x.col,leg=leg)
		return(res)
	}	
	#--color scale palette with breaks
	colorscale1=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks=quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		# adjust breaks and get palette
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		cols=colorRampPalette(colors=cols)(length(bkcenter)-1)
		# set colors to x
		x.col=rep(NA,length(x))
		cuts=cut(x[!is.na(x)],breaks=bkcenter,include.lowest=TRUE)
		x.col[!is.na(x)]=cols[as.integer(cuts)]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get scale (for any legend) and return results
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=cols,legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)	
		return(res)
	}
	#--neg/pos color scale palette with breaks (left/right)
	colorscale2=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check args
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks=quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.null(cols))cols=c("darkblue","white","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2		
		# check color vec		
		lt=length(cols)
		if(lt/2==as.integer(lt/2))lt=lt+1	
		cols=colorRampPalette(colors=cols)(lt)
		lfrt=as.integer(lt/2)+1
		# get neg/pos colors
		negCols=cols[1:lfrt]
		posCols=cols[lfrt:lt]
		ct.col=cols[lfrt]
		# check and adjust breaks
		lt=length(bkcenter)
		if(lt/2==as.integer(lt/2)){
			lf=lt/2
			rt=(lt/2)+1
			center=(bkcenter[lf]+bkcenter[rt])/2
			negBreaks=c(-Inf,bkcenter[1:lf],center)
			posBreaks=c(center,bkcenter[rt:lt],+Inf)
		} else {
			lfrt=as.integer(lt/2)+1
			center=bkcenter[lfrt]
			negBreaks=c(-Inf,bkcenter[1:lfrt])
			posBreaks=c(bkcenter[lfrt:lt],+Inf)
		}
		# set main palettes	
		negCols=colorRampPalette(colors=negCols)(length(negBreaks))[-length(negBreaks)]
		posCols=colorRampPalette(colors=posCols)(length(posBreaks))[-1]
		# set minor palettesscale
		na.col=colorRampPalette(colors=c(na.col,na.col))(1)
		ct.col=colorRampPalette(colors=c(ct.col,ct.col))(1)		
		# set colors to x
		x.col=rep(NA,length(x))
		idx=x<center & !is.na(x)
		negcuts=cut(x[idx],breaks=negBreaks,include.lowest=TRUE)
		x.col[idx]=negCols[as.integer(negcuts)]
		idx=x>center & !is.na(x)
		poscuts=cut(x[idx],breaks=posBreaks)
		x.col[idx]=posCols[as.integer(poscuts)]
		x.col[x==center]=ct.col
		x.col[is.na(x.col)]=na.col
		# get intervals
		if(is.null(nquant)){
			interv=c(levels(negcuts),levels(poscuts))
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}
		testlen=length(breaks)/2
		if(as.integer(testlen)<testlen){
			idx=as.integer(testlen)+1
			breaks=breaks[c(1:idx,idx:length(breaks))]
		}
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=c(negCols,posCols),legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)
		return(res)
	}
	#--simple size scale for 'category' or 'enumerated' data type (i.e. factor levels)
	xcategory=function(x, szmin, szmax, na.sz, categvec, isrev){
		# set sz and return vec
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec)
		}		
		szlevs=seq(szmin, szmax, length.out=nlevels(x))
		if(isrev)szlevs=rev(szlevs)
		x.sz=szlevs[x]
		x.sz[is.na(x.sz)]=na.sz
		# get scale (for any legend) and return results
		leg=list(scale=szlevs,legend=levels(x))
		res=list(res=x.sz,leg=leg)
		return(res)		
	}
	#--size scale with breaks
	xscale=function(x,breaks,szmin,szmax,na.sz,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks=quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		# adjust breaks
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		# get sz levels
		szlevs=seq(szmin,szmax,length.out=length(bkcenter)-1)
		if(isrev)szlevs=rev(szlevs)		
		# set sz to x
		x.sz=rep(NA,length(x))
		cuts=cut(x[!is.na(x)],breaks=bkcenter,include.lowest=TRUE)
		x.sz[!is.na(x)]=szlevs[as.integer(cuts)]
		x.sz[is.na(x.sz)]=na.sz
		# get scale (for any legend) and return results
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}	
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=szlevs,legend=breaks,interval=interv)
		res=list(res=x.sz,leg=leg)
		return(res)
	}
	#--set node shapes for 'category' or 'enumerated' data type (i.e. factor levels)
	shapecategory=function(x,shapes,categvec,isrev){
		validnames=c('SOLID', 'DOTTED', 'DOTTED_SHORT', 'LONG_DASH')
		if(is.null(shapes)){
			shapes=validnames
		} else if(is.numeric(shapes) || is.integer(shapes)){
			shapes=as.integer(shapes)
			if(sum(sum(shapes<1),sum(shapes>5))>0){
				message("Error: NOTE: arg. 'edgeType' with one or more inconsistent ids!")
				message("*List of edgetype names and ids handled by 'att.sete' function:")
				print(cbind(shape= validnames,id=1:4),quote=F)
				return(g)
			} 
			shapes=validnames[shapes]
		} else {
			if(sum(shapes%in%validnames)!=length(shapes)){
				message("Error: NOTE: arg. 'edgeType' with one or more inconsistent names!")
				message("*List of edgetype names and ids handled by 'att.sete' function:")
				print(cbind(shape= validnames,id=1:4),quote=F)
				return(g)
			}			
		}
		if(sum(is.na(x))>0){
			stop("NOTE: 'NA' not supported for edge type mapping!")
		}
		if(isrev)shapes=rev(shapes)
		x=as.factor(x)
		if(length(levels(x))>length(shapes)){
			stop("NOTE: graph att. 'from' with more levels than edgetype options!")
		}
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			if(length(categvec)!=length(shapes)){
				if(length(categvec)>length(shapes)){
					stop("NOTE: arg. 'categvec' with more levels than edge types!")
				} else {
					shapes=shapes[1:length(categvec)]
				}
			}
			x=factor(x,levels=categvec)
		}
		att=shapes[x]
		# get shapes (for any legend) and return results
		leg=list(shape=shapes,legend=levels(x))
		res=list(res=att,leg=leg)
		return(res)
	}	
	# end of main functions!
	#-----------------------
	# map attribute to default names!---------------------------------------
	att=NULL
	if(to%in%coltype){
		if(is.null(breaks) && is.null(nquant)){
			att=colorcategory(fromatt,cols,na.col,categvec,isrev)
		} else {
			if(pal==1){
				att=colorscale1(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else if(pal==2){
				att=colorscale2(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else {
				att=colorcategory(fromatt,cols,na.col,categvec,isrev)
			}
		}
	} else if(to%in%numtype){
		c('edgeWidth','edgeWeight','arrowDirection')
		szmin=xlim[1]
		szmax=xlim[2]
		na.sz=xlim[3]
		szmin=max(0,szmin)
		szmax=max(0,szmax)
		na.sz=max(0,na.sz)			
		if(to=='arrowDirection'){
			szmin=min(3,szmin)
			szmax=min(3,szmax)
			na.sz=min(3,na.sz)
		}
		if(is.null(breaks) && is.null(nquant)){
			att=xcategory(fromatt,szmin,szmax,na.sz,categvec,isrev)
		} else {
			att=xscale(fromatt,breaks,szmin,szmax,na.sz,isrev,nquant,roundleg)
		}
	} else if(to=='edgeType'){
		att=shapecategory(fromatt,shapes,categvec,isrev)
	}
	# return updated graph
	if(!is.null(att)){
		g=igraph::set.edge.attribute(graph=g, name=to, value=att$res)
		if(is.logical(getleg) && getleg){
			to=gsub("\\b(\\w)","\\U\\1",to,perl=TRUE)
			leg=paste("leg",to,sep="")
			att$leg$title<-title
			g=igraph::set.graph.attribute(graph=g, name=leg, value=att$leg)
		}
	} else {
		message("...unable to conclude the command!")
	}
	return(g)
}
	
#------------------------------------------------------------------------------
# map vertices att. to an igraph object
att.mapv=function(g, dat, refcol=1){
  #check loaded igraph
  igraph.check()
	if(!is.data.frame(dat)){
		stop("not a data frame!")
	}
	# check igraph object and main args---------------------------------
	if(!igraph::is.igraph(g)){
	  stop("Not an igraph object!")
	}
	# get vecs to match!
	nodes=V(g)$name
	if(is.null(nodes) || igraph::vcount(g)!=length(nodes)){
		stop("NOTE: require 'name' attribute in igraph vertices!")
	}
	dc=dat[[refcol[1]]]
	# check attribute data class
	if(is.factor(dc)){
		if(class(levels(dc))=="character"){
			dc=as.character(dc)
		} else if(class(levels(dc))=="numeric"){
			dc=as.numeric(dc)
		} else if(class(levels(dc))=="integer"){
			dc= as.integer(dc)
		} else {
			stop("NOTE: invalid ref. attribute! supported data classes: character, numeric or integer!")
		}
	}
	# check node data class and map attributes to nodes!
	if(class(nodes)==class(dc)){
		dat=dat[match(nodes,dc),]
	} else if(class(nodes)=="character"){
		dat=dat[match(nodes,as.character(dc)),]
	} else if(class(nodes)=="numeric"){
		dat=dat[match(nodes,as.numeric(dc)),]
	} else if(class(nodes)=="integer"){
		dat=dat[match(nodes,as.integer(dc)),]
	} else {
		stop("NOTE: invalid node names! supported data classes: character, numeric or integer!")
	}
	# check whether resulting dataset size matches nodes!
	if(nrow(dat)!=length(nodes)){
		stop("NOTE: one or more graph nodes are not listed in the dataset (i.e. 'refcol' ids)!")
	}
	# transfer data to graph vertices
	for(i in 1:ncol(dat)){
		if(i!=refcol){
			att=dat[[i]]
			if(is.factor(att))att=levels(att)[att]
			g=igraph::set.vertex.attribute(graph=g, name=names(dat)[i],value=att)
		}
	}
	return(g)
}

#------------------------------------------------------------------------------
# map edge att. to an igraph object
att.mape=function(g, dat, refcol=c(1,2)){
  #check loaded igraph
  igraph.check()
	if(!is.data.frame(dat)){
		stop("not a data frame!")
	}
    # check igraph object
    if(!igraph::is.igraph(g)){
        stop("Not an igraph object!")
    }
	# get vecs to match!
	nodes=V(g)$name
	if(is.null(nodes) || igraph::vcount(g)!=length(nodes)){
		stop("NOTE: require 'name' attribute in igraph vertices!")
	}
	edges=igraph::get.edgelist(g)
	edgevec=array(NA,dim=nrow(edges))
	for(i in 1:nrow(edges)){
		edgevec[i]=paste(edges[i,],collapse=",")
	}
	dc=as.matrix(dat[,refcol])
	dcvec=array(NA,dim=nrow(dc))
	for(i in 1:nrow(dc)){
		dcvec[i]=paste(dc[i,c(1,2)],collapse=",")
	}
	idx1=match(edgevec, dcvec)
	dcvec=array(NA,dim=nrow(dc))
	for(i in 1:nrow(dc)){
		dcvec[i]=paste(dc[i,c(2,1)],collapse=",")
	}
	idx2=match(edgevec, dcvec)
	idx1[is.na(idx1)]=idx2[is.na(idx1)]
	# check whether resulting dataset size matches nodes!
	if(sum(is.na(idx1))>0){
		stop("NOTE: one or more edges are not listed in the dataset (i.e. 'refcol' ids)!")
	}	
	dat=dat[idx1,]
	# check whether resulting dataset size matches nodes!
	if(nrow(dat)!=nrow(edges)){
		stop("NOTE: one or more edges are not listed in the dataset (i.e. 'refcol' ids)!")
	}
	# transfer data to graph edges
	for(i in 1:ncol(dat)){
		if(!i%in%refcol){
			att=dat[[i]]
			if(is.factor(att))att=levels(att)[att]
			g=igraph::set.edge.attribute(graph=g, name=names(dat)[i],value=att)
		}
	}
	return(g)
}

#------------------------------------------------------------------------------
# get subg from an igraph object
subg=function(g, dat, refcol=1, maincomp=TRUE, connected=TRUE, transdat=TRUE){
	if(is.data.frame(dat)){
		allids=as.character(dat[[refcol[1]]])
	} else if(is.vector(dat)){
		allids=as.character(dat)
	} else {
		stop("not a data frame!")
	}
	if(!igraph::is.igraph(g)){
	  stop("Not an igraph object!")
	}
	if(is.null(V(g)$name))V(g)$name=as.character(V(g))
	ids=allids[allids%in%V(g)$name]
	if(length(ids)!=length(allids)){
	  message("...note: not all genes found in the network!")
	}
	sg=igraph::induced.subgraph(graph=g,vids=ids)
	if(maincomp){
		comp <- igraph::clusters(sg)
		cids <- which.max(comp$csize)
		sg <- igraph::induced.subgraph(graph=sg, vids=V(sg)[comp$membership == cids])
	} else if(connected){
		dg=igraph::degree(sg)>0
		nodes=V(sg)$name[dg]
		sg=igraph::induced.subgraph(graph=sg,vids=nodes)
	}
	if(transdat && is.data.frame(dat)){
		sg <- att.mapv(g=sg, dat=dat, refcol=refcol)
	}
	return(sg)
}

#Simple function to generate random null distributions for co-expression analysis
#------------------------------------------------------------------------------
cea=function(x, sig=0.01, p.adj.method="fdr", cor.method="spearman", nper=1000, plotcea=TRUE,...){
	if(is.data.frame(x)){
		x=as.matrix(x)
	} else {
		if(!is.matrix(x))stop("NOTE: not a matrix!")
	}
	cat("Step 1 ...computing correlation",fill=TRUE)
	# get correlation matrix
	x=t(x)
	corrMt=cor(x, method = cor.method)
	diag(corrMt)=0	
	uniqueVec=unique(sort(corrMt))
	cat("Step 2 ...computing null distribution",fill=TRUE)
	# builds the null distribution via permutation
	ctsum=numeric(length(uniqueVec))
	nulldist=list()
	pb=txtProgressBar(min=0, max=nper, initial=0, char="=", style=1)
	for(i in 1:nper){
		permt=matrix(sample(x),nrow=nrow(x),ncol=ncol(x))
		permt=cor(permt,method=cor.method)
		diag(permt)=NA
		permt=sort(permt)
        #find intervals
		ct=findInterval(uniqueVec,permt)
		ctsum=ctsum+ct
		nl=density(permt)
		nulldist$x=cbind(nulldist$x,nl$x)
		nulldist$y=cbind(nulldist$y,nl$y)		
		setTxtProgressBar(pb, value=i)
	}
	#close(pb)
	cat("\n")
	cat("Step 3 ...computing probs", fill=TRUE)
	# prepares matrices to compute probs
	probs=ctsum/(length(permt)*nper)
	probs[probs>0.5]=1-probs[probs>0.5]
	probs=probs[match(as.numeric(corrMt),uniqueVec)]
	cat("Step 4 ...adjusting pvals",fill=TRUE)
	# adjusts pvals
	pvalAdj=p.adjust(probs,method=p.adj.method)
	pvalAdj=matrix(pvalAdj,nrow=nrow(corrMt),ncol=nrow(corrMt))
	# decides on the significance
	decision=pvalAdj>sig
	decisionMt=corrMt
	decisionMt[decision]=0.0
	rescea=list(corr.mt=corrMt, decision.mt=decisionMt, pvalue.adj=pvalAdj, null.dist=nulldist)
	# function to plot decision matx
	ptcea=function(rescea, ptype=4, bk=0.2, n.breaks=100, plotnull=TRUE, avnull=TRUE, nullcol="black"){
	  ptype=4
		if(!is.numeric(bk))bk=0.2
		bk=min(1,max(0.1,bk))
		n.breaks=as.integer(n.breaks)
		if(!is.numeric(ptype) && !is.integer(ptype))ptype=1
		if(!ptype%in%c(1,2,3,4,5))ptype=1
		if(!is.numeric(n.breaks) && !is.integer(n.breaks))n.breaks=100
		if(n.breaks<2)n.breaks=2
		#
		if(ptype<=3){
			null.dist=rescea$null.dist
			decision.dist=as.numeric(rescea$decision.mt)
			decision.neg=density(decision.dist,to=-bk)	
			decision.pos=density(decision.dist,from=bk)	
			maxdeci=max(decision.neg$y, decision.pos$y)
			#
			corr.dist=as.numeric(rescea$corr.mt)
			corr.dist=density(corr.dist)	
			maxcorr=max(corr.dist$y)
			#maxdata=min(c(maxdeci,maxcorr))
			maxdata=maxcorr
		} else {
			null.dist=rescea$null.dist
			decision.dist=as.numeric(rescea$decision.mt)
			#
			decision.hist=hist(decision.dist, plot=FALSE, breaks=n.breaks)
			maxdeci=max(decision.hist$density)
			#
			decision.neg=density(decision.dist,to=-bk)	
			decision.pos=density(decision.dist,from=bk)	
			#
			corr.dist=as.numeric(rescea$corr.mt)
			corr.dist=hist(corr.dist, plot=FALSE, breaks=n.breaks)	
			maxcorr=max(corr.dist$density)
			maxdata=maxcorr		
		}
		#
		if(ptype==1){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#
			y=apply(null.dist$y,1,mean)
			y=y/max(y)
			x=apply(null.dist$x,1,mean)	
			polygon(x=x,y=y,col="grey", lty="blank")
			#
			x=decision.neg$x		
			y=decision.neg$y/maxdata
			polygon(x=x,y=y, col="blue",lty="blank")
			#
			x=decision.pos$x		
			y=decision.pos$y/maxdata
			polygon(x=x,y=y, col="red",lty="blank")		
			#  
			x=corr.dist$x  	
			y=corr.dist$y/maxdata
			lines(x=x,y=y, col="black", lty="dashed", lwd=2.0)
			#   
			legend("topleft",c("all associations","sig. neg. associations",
				"sig. pos. association","null distribution"), pch=c(NA_integer_ ,15, 15, 15), 
				pt.cex=1, lty=c(2, 0, 0, 0), merge=TRUE, col=c('black','blue','red','grey'), bty="n", cex=0.7)  
		} 
		if(ptype==2){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$x  	
			y=corr.dist$y/maxdata
			polygon(x=x,y=y, col="grey", lty="blank")
			#
			x=decision.neg$x		
			y=decision.neg$y/maxdata
			polygon(x=x,y=y, col="blue",lty="blank")
			#
			x=decision.pos$x		
			y=decision.pos$y/maxdata
			polygon(x=x,y=y, col="red",lty="blank")
			#	
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)				
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")					
				}		
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations","null distributions"), pch=c(15, 15, 15, NA_integer_), 
					pt.cex=1, lty=c(0, 0, 0, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all association","sig. neg. associations",
					"sig. pos. associations"), pch=c(15, 15, 15), 
					pt.cex=1, lty=c(0, 0, 0), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}
		if(ptype==3){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$x  	
			y=corr.dist$y/maxdata
			polygon(x=x,y=y, col="grey", lty="blank")
			#
			x=decision.neg$x		
			y=decision.neg$y/maxdata
			lines(x=x,y=y, col="blue",lty="dashed", lwd=3.0)
			#
			x=decision.pos$x		
			y=decision.pos$y/maxdata
			lines(x=x,y=y, col="red", lty="dashed", lwd=3.0)
			#
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)				
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")					
				}			
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations","null distributions"), pch=c(15, 15, 15, NA_integer_), 
					pt.cex=1, lty=c(0, 0, 0, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all association","sig. neg. associations",
					"sig. pos. associations"), pch=c(15, 15, 15), 
					pt.cex=1, lty=c(0, 0, 0), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}	
		if(ptype==4){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$breaks  	
			y=corr.dist$density/maxdata	
			rect(x[-length(x)], 0, x[-1], y,col='grey90',border='grey30')
			#	
			xd=decision.hist$breaks		
			yd=decision.hist$density/maxdata
			idx=xd>bk;
			x=c(0,xd[idx]);y=c(0,yd[idx])
			rect(x[-length(x)], 0, x[-1], y,col='red',border='darkred')
			idx=xd<(-bk);
			x=c(xd[idx],0);y=c(yd[idx],0)	
			rect(x[-length(x)], 0, x[-1], y,col='blue',border='darkblue')
			#
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")	
				}			
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations","null distribution"), pch=c(15, 15, 15, NA_integer_), 
					pt.cex=1, lty=c(0, 0, 0, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations"), pch=c(15, 15, 15), 
					pt.cex=1, lty=c(0, 0, 0), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}
		if(ptype==5){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
				 xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$breaks  	
			y=corr.dist$density/maxdata	
			rect(x[-length(x)], 0, x[-1], y,col='grey90',border='grey70')
			#
			x=c(decision.neg$x,0)		
			y=c(decision.neg$y/maxdata,0)
			lines(x=x,y=y, col="blue", lty="dashed", lwd=3.0)
			#
			x=c(0,decision.pos$x)		
			y=c(0,decision.pos$y/maxdata)
			lines(x=x,y=y, col="red", lty="dashed", lwd=3.0)
			#
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")	
				}			
				legend("topleft",c("all associations","sig. neg. associations",
								   "sig. pos. associations","null distribution"), pch=c(15, NA_integer_, NA_integer_, NA_integer_), 
					   pt.cex=1, lty=c(0, 2, 2, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all associations","sig. neg. associations",
								   "sig. pos. associations"), pch=c(15, NA_integer_, NA_integer_), 
					   pt.cex=1, lty=c(0, 2, 2), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}
	}
	if(plotcea){
		ptcea(rescea,...=...)
	}
	return(rescea$decision.mt)
}
##-----------------------------------------------------------------------------
#Use default igraph atts if available------------------
check.igraph.format<-function(g){
  if(!is.null(V(g)$color) && is.null(V(g)$nodeColor) )V(g)$nodeColor=V(g)$color
  if(!is.null(V(g)$frame.color) && is.null(V(g)$nodeLineColor) )V(g)$nodeLineColor=V(g)$frame.color
  if(!is.null(V(g)$size) && is.null(V(g)$nodeSize) )V(g)$nodeSize=V(g)$size*2.5
  if(!is.null(V(g)$label) && is.null(V(g)$nodeAlias) )V(g)$nodeAlias=V(g)$label
  if(!is.null(V(g)$label.cex) && is.null(V(g)$nodeFontSize) )V(g)$nodeFontSize=V(g)$label.cex*16
  if(!is.null(V(g)$label.color) && is.null(V(g)$nodeFontColor) )V(g)$nodeFontColor=V(g)$label.color
  if(!is.null(V(g)$shape) && is.null( ) ){
    shapes<-V(g)$shape
    shapes[shapes=="circle"]="ELLIPSE"
    shapes[shapes!="circle"]="RECTANGLE"
    V(g)$nodeShape<-shapes
  }    			
  if(!is.null(E(g)$width) && is.null(E(g)$edgeWidth) )E(g)$edgeWidth<-E(g)$width
  if(!is.null(E(g)$color) && is.null(E(g)$edgeColor) )E(g)$edgeColor<-E(g)$color
  if(!is.null(E(g)$weight) && is.null(E(g)$edgeWeight) )E(g)$edgeWeight<-E(g)$weight
  if(!is.null(E(g)$lty) && is.null(E(g)$edgeType) ){
    edgeType<-E(g)$lty
    tp<-c("blank","blank","solid","dashed","dotdash","longdash","twodash")
    if(is.numeric(edgeType))tp=c(0:6)
    edgeType[edgeType==0]="SOLID"
    edgeType[edgeType==1]="SOLID"
    edgeType[edgeType==2]="DOTTED"	
    edgeType[edgeType==3]="DOTTED_SHORT"
    edgeType[edgeType==4]="DOTTED"
    edgeType[edgeType==5]="LONG_DASH"	
    edgeType[edgeType==6]="DOTTED"
    E(g)$edgeType=edgeType
  }
  if(is.null(V(g)$nodeLineColor) && !is.null(V(g)$color) ) V(g)$nodeLineColor="black"
  return(g)
}
##-----------------------------------------------------------------------------
check.igraph.direction<-function(g){
  #Check direction and type
  #---set mode if available---#
  arrowType = E(g)$arrowType
  if(!is.null(arrowType) && length(arrowType)>0){
    c1=!is.integer(arrowType)
    c2=!is.numeric(arrowType)
    c3=(sum(arrowType< -1)>0 || sum(arrowType>1)>0)
    if(c1 && c2){
      stop("NOTE: 'arrow type' must be provided as integers!")
    } else if(sum(is.na(arrowType))>0){
      stop("NOTE: invalid 'arrow type' declaration: 'NA' found'!")
    } else if(c3){
      stop("NOTE: invalid 'arrow type' input (options: -1, 0 or 1)")
    }
    g<-arrowtype4reder(g)
    #---
    #E(g)$arrowLength<-5
    #E(g)$arrowAngle<-15
    #idx<-E(g)$arrowType<0
    #E(g)$arrowLength[idx]<-3   
    #E(g)$arrowAngle[idx]<-90
    E(g)$arrowDirection<-E(g)$arrowType
  } else {
    # set direction to edge attributes
    idxmutual<-igraph::is.mutual(g)
    E(g)$arrowDirection=1
    E(g)$arrowDirection[idxmutual]=3
    #g=igraph::as.undirected(g, mode="collapse") //essa funcao nao retorno ordem correta!!! controlado agora em J!
    c1=length(igraph::list.edge.attributes(g))>0
    c2=sum(idxmutual)>0
    if(c1 && c2){
      warning("NOTE: attributes from mutual edges were collapsed to unique edges (see 'addGraph' doc).")
      #isso sera feito no lado Java, ultimo link define valor final!!!
    }
    #Remove multiple edges and loops (obs. for directed graphs order does matter)
    if(!igraph::is.simple(g)){
      g=igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
      warning("NOTE: loops and/or multiple edges were removed from your graph (see 'addGraph' doc)!")
    }
  }
  return(g)
}
##-----------------------------------------------------------------------------
##format arrowType
arrowtype4reder<-function(g){
  #---get edges and mode
  edgeMtx<-igraph::get.adjacency(g, sparse=FALSE,attr=NULL, names=FALSE)
  modeMtx<-igraph::get.adjacency(g, sparse=FALSE,attr="arrowType", names=FALSE)
  ix<-igraph::get.edgelist(g,names=FALSE)
  #---differentiate upper.tri/lower.tri
  modeMtx[upper.tri(modeMtx)]<-modeMtx[upper.tri(modeMtx)]*10
  edgeMtx[modeMtx!=0]<-modeMtx[modeMtx!=0]*10
  #---set arrowType for reder
  arrowType<-NULL
  sapply(1:nrow(ix),function(i){
    tp<-edgeMtx[ix[i,1],ix[i,2]]+edgeMtx[ix[i,2],ix[i,1]]
    arrowType<<-c(arrowType,tp)
  })
  arrowType[arrowType==1 | arrowType==2]<-0
  #---map mutual edges and remove duplicated
  idx<-is.mutual(g) & ix[,1]>ix[,2]
  arrowType<-arrowType[!idx]
  g<-delete.edges(g,edges=which(idx))
  #---set mode---#
  # arrow key, related to 'A->B' orientation
  #  0 = undirected:  0 (A-B or B-A)
  # +1 = simple: +10 (A->B)
  # -1 = simple: -10 (A-/B)  
  # +2 = simple: +100 (B->A)
  # -2 = simple: -100 (B-/A)
  # +3 = double: +110 (same mode, A->B and B->A)
  # -3 = double: -110 (same mode, A-\B and B-\A)
  # +4 = double: +90 (inverse mode, A->B and B-\A)
  # -4 = double: -90 (inverse mode, A-\B and B->A)
  key<-c(0,10,-10,100,-100,110,-110,90,-90)
  lab<-c(0,1,-1,1,-1,3,-3,4,-4)
  arrowType<-sapply(1:length(arrowType),function(i){
    tp<-arrowType[i]
    lab[which(key==tp)]
  })
  E(g)$arrowType<-arrowType
  return(g)
}
# chech igraph compatibility
igraph.check<-function(){
  b1<-"package:igraph0" %in% search()
  b2<- "igraph0" %in%  loadedNamespaces()
  if( b1 || b2) {
    stop("\n\n ...conflict with 'igraph0': please use the new 'igraph' package!")
  }
}
##-----------------------------------------------------------------------------
##build nested maps from hclust objects
treemap<-function(hc){
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
  obj<-list(nest=nest,compids=B,labels=hc$labels,parent=D,leafdist=C,
            rootdist=E,height=hc$height,nnest=N, xyaxis=xyaxis)
  #---get unified edges
  N<-nrow(hc$merge);nn<-N+1
  hcEdges<-NULL
  eLength<-NULL
  junk<-sapply(1:N,function(i){
    y1<-hc$merge[i,1]
    y2<-hc$merge[i,2]
    if(y1>0){
      l1<-hc$height[i] - hc$height[y1]
    } else {
      l1<-hc$height[i]
    }
    if(y2>0){
      l2<-hc$height[i] - hc$height[y2]
    } else {
      l2<-hc$height[i]
    }    
    tp<-cbind(rbind(c(i,y1),c(i,y2)),c(l1,l2))
    hcEdges<<-rbind(hcEdges,tp)
    NULL
  })
  colnames(hcEdges)<-c("parentNode","childNode","length")
  hcEdges<-data.frame(hcEdges,stringsAsFactors=FALSE)
  #---get unified nodes
  hcl<-data.frame(node=hc$labels,mergeId=-c(1:nn),hcId=c(1:nn), type="leaf",stringsAsFactors=FALSE)
  hcn<-data.frame(node=paste("N",c(1:N),sep=""),mergeId=c(1:N),hcId=c(1:N),type="nest",stringsAsFactors=FALSE)
  hcNodes<-rbind(hcl,hcn)
  hcEdges$parentNode<-hcNodes$node[match(hcEdges$parentNode,hcNodes$mergeId)]
  hcEdges$childNode<-hcNodes$node[match(hcEdges$childNode,hcNodes$mergeId)]
  #---update and return
  obj$hcNodes<-hcNodes;obj$hcEdges<-hcEdges
  return(obj)
}

##-----------------------------------------------------------------------------
##build an igraph object from hclust
hclust2igraph<-function(hc,length.cutoff=NULL){
  if(class(hc)!="hclust")stop("'hc' should be an 'hclust' object!")
  if(is.null(hc$labels))hc$labels=as.character(sort(hc$order))
  #get treemap
  tmap<-treemap(hc)
  hcNodes<-tmap$hcNodes
  hcNests<-hcNodes[hcNodes$type=="nest",]
  hcEdges<-tmap$hcEdges
  nestList<-tmap$nest
  #update nest names
  names(nestList)[hcNests$hcId]<-hcNests$node
  #remove nests based on length cutoff
  if(!is.null(length.cutoff)){
    #identify rmnodes
    rmnodes<-hcEdges[hcEdges$length<length.cutoff,]
    rmnodes<-rmnodes$childNode[rmnodes$childNode%in%hcNests$node]
    #update hcEdges and nestList
    hcEdges<-hcEdges.filter1(hcEdges,hcNodes,rmnodes)
    nestList<-nestList[!names(nestList)%in%rmnodes]
  }
  #build igraph and return assigments
  g<-graph.edgelist(as.matrix(hcEdges[,1:2]), directed=TRUE)
  E(g)$edgeWeight<-hcEdges$length
  list(g=g,nest=nestList)
}
##-----------------------------------------------------------------------------
##build an igraph object from pvclust
pvclust2igraph<-function(hc,alpha=0.95, max.only=TRUE){
  if(class(hc)!="pvclust")stop("'hc' should be an 'pvclust' object!")
  if(is.null(hc$hclust$labels))hc$hclust$labels=as.character(sort(hc$hclust$order))
  #get treemap
  tmap<-treemap(hc$hclust)
  hcNodes<-tmap$hcNodes
  hcNests<-hcNodes[hcNodes$type=="nest",]
  hcEdges<-tmap$hcEdges
  nestList<-tmap$nest
  #update nest names
  names(nestList)[hcNests$hcId]<-hcNests$node
  #remove nests
  rmnodes<-pvpick(hc,alpha=alpha,max.only=max.only)$edges
  rmnodes<-hcNests[hcNests$hcId%in%rmnodes,"node"]
  if(length(rmnodes)>0){
    #update hcEdges and nestList
    if(max.only){
      hcEdges<-hcEdges.filter3(hcEdges,nests=nestList[rmnodes])
    } else {
      hcEdges<-hcEdges.filter2(hcEdges,hcNodes,rmnodes)
    }
    nestList<-nestList[names(nestList)%in%hcEdges$parentNode]
  }
  #build igraph and return assigments
  g<-graph.edgelist(as.matrix(hcEdges[,1:2]), directed=TRUE)
  E(g)$edgeWeight<-hcEdges$length
  list(g=g,nest=nestList)
}
hcEdges.filter1<-function(hcEdges,hcNodes,rmnodes){
  #get the correct order
  rmNests<-hcNodes[hcNodes$node%in%rmnodes,]
  rmEdges<-hcEdges[hcEdges$childNode%in%rmnodes,]
  #get filtered hcEdges
  hcEdges<-hcEdges[!hcEdges$childNode%in%rmNests$node,]
  #update parents'ids
  for(oldid in rmNests$node){
    newid<-rmEdges$parentNode[which(rmEdges$childNode==oldid)]
    idx<-which(hcEdges$parentNode==oldid)
    hcEdges$parentNode[idx]<-newid
  }
  rownames(hcEdges)<-NULL
  hcEdges
}
hcEdges.filter2<-function(hcEdges,hcNodes,rmnodes){
  #get the correct order
  rmNests<-hcNodes[hcNodes$node%in%rmnodes,]
  rmEdges<-hcEdges[hcEdges$childNode%in%rmnodes,]
  #check nest's child nodes
  checkEdges<-hcEdges
  lt<-sapply(rmNests$node,function(id){
    idx<-which(checkEdges$parentNode==id)
    child<-checkEdges$childNode[idx]
    lt<-sum(hcNodes$type[hcNodes$node%in%child]=="nest")==0
    if(lt){
      newparent<-rmEdges$parentNode[rmEdges$childNode==id]
      checkEdges$parentNode[idx]<<-newparent
      checkEdges<<-checkEdges[-which(checkEdges$childNode==id),]
    }
    lt
  })
  rmNests<-rmNests[lt,]
  #check nest's parent node
  checkEdges<-hcEdges
  lt<-sapply(rmNests$node,function(id){
    idx<-which(checkEdges$childNode==id)
    newparent<-checkEdges$parentNode[idx]
    lt<-newparent%in%rmNests$node
    if(lt){
      idx<-which(checkEdges$parentNode==id)
      checkEdges$parentNode[idx]<<-newparent
      checkEdges<<-checkEdges[-which(checkEdges$childNode==id),]
    }
    lt
  })
  rmNests<-rmNests[lt,]
  hcEdges<-checkEdges
  rownames(hcEdges)<-NULL
  hcEdges
}
hcEdges.filter3<-function(hcEdges, nests){
  rmnodes<-lapply(names(nests),function(nest){
    idx<-hcEdges$childNode%in%nests[[nest]] & !hcEdges$parentNode==nest
    hcEdges$parentNode[idx]
  })
  names(rmnodes)<-names(nests)
  rmnodes<-rmnodes[sapply(rmnodes,length)>0]
  if(length(rmnodes)>0){
    sapply(names(rmnodes),function(parent){
      idx<-hcEdges$parentNode%in%rmnodes[[parent]]
      hcEdges$parentNode[idx]<<-parent
      idx<-hcEdges$childNode%in%rmnodes[[parent]]
      hcEdges<<-hcEdges[-which(idx),]
      NULL
    })
    rownames(hcEdges)<-NULL
  }
  hcEdges
}
