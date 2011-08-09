#Simple function to generate random graphs with a modular structure------------------
gtoy.rm=function(m=3, nmax=30, nmin=3, p1=0.5, p2=0.05, p3=0.9){
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
	gg=graph.empty(n=0, directed=FALSE)
	mdmap=c()	
	for(i in 1:m){
		v=max(nmin,nver(nmax,p1))
		g=erdos.renyi.game(n=v, p.or.m=p3, type="gnp", directed=FALSE)
		gg=graph.disjoint.union(gg,g)
		mdmap=c(mdmap,rep(i,v))		
	}
	adj=get.adjacency(gg)
	adj[,]=0
	adj[,]=runif(nrow(adj)*ncol(adj))
	adj[adj<(1-(p2/m))]=0
	for(i in 1:m){
		adj[mdmap==i,mdmap==i]=0
	}
	adj[adj>0]=1
	adj=adj+get.adjacency(gg)
	gg=graph.adjacency(adj, mode="undirected",diag=FALSE)
	gg=simplify(gg, remove.multiple = TRUE, remove.loops = TRUE)
	V(gg)$name=paste("n",1:vcount(gg),sep="")
	cols=terrain.colors(m)
	V(gg)$nodeColor=cols[mdmap]
	V(gg)$module.id=mdmap
	return(gg)
}




