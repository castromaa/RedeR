###############################################################################
##Source Codes for enrichment anlyses and subnetwork identification 
###############################################################################
library(RedeR)
library(BioNet)
library(DLBCL)
library(ALL)
library(limma)
library(graph)
library(org.Hs.eg.db)
library(KEGG.db)
library(GO.db)
library(AnnotationDbi)
library(snow)
library(biomaRt)
library(HTSanalyzeR)
library(igraph)

###############################################################################
############################Analyses pipeline##################################
###############################################################################
analysesPipeline<-function() {
	##load data
	data(ER.limma)
	restab<-ER.limma$resdeg
	##preprocessing
	pval<-list()
	pval.adj<-list()
	pheno<-list()
	hits<-list()
	for(i in c("t3", "t6", "t12")) {
		pheno[[i]]<-restab[, paste("coef.", i, " - t0", sep="")]
		names(pheno[[i]])<-restab[, "ENTREZ"]
		pval[[i]]<-restab[, paste("p.value.", i, " - t0", sep="")]
		names(pval[[i]])<-restab[, "ENTREZ"]
		pval.adj[[i]]<-restab[, paste("p.value.adj.", i, " - t0", sep="")]
		names(pval.adj[[i]])<-restab[, "ENTREZ"]
		hits[[i]]<-as.character(restab[restab[, 
			paste("degenes.", i, " - t0",sep="")]!=0, "ENTREZ"])
	}
	##enrichment analyses
	gsca<-gscaAnalyses(hits, pheno) 
	gsca4RedeR<-list()
	gsca4RedeR$gsca<-gsca
	gsca4RedeR$hits <-hits
	gsca4RedeR$pheno <-pheno	
	save(gsca4RedeR, file="gsca4RedeR.RData")
	##bionet pipeline
	##load HGPD interactome and data
	data(hs.inter)
	subnets<-BioNetAnalyses(network=hs.inter, pval, pheno)
	bionet4RedeR<-list()
	bionet4RedeR$pheno<-pheno
	bionet4RedeR$subnets <-subnets
	save(bionet4RedeR, file="bionet4RedeR.RData")
}
################################Enrichment analyses############################
##gene set collection analyses
gscaAnalyses<-function(hits, pheno) {
	PW_KEGG <- KeggGeneSets(species = "Hs")
	GO_BP <- GOGeneSets(species = "Hs", ontologies="BP")
	GO_MF <- GOGeneSets(species = "Hs", ontologies="MF")
	gscs <- list(PW_KEGG=PW_KEGG, GO_BP=GO_BP, GO_MF=GO_MF)
	
	gsca.rslt<-list()
	options(cluster=makeCluster(4, "SOCK"))
	for(i in 1:length(pheno)) {
		##create an object of class 'GSCA'
		gsca <- new("GSCA", listOfGeneSetCollections=gscs, geneList = 
						pheno[[i]], hits = hits[[i]])
		##do preprocessing (KcViab_Data4Enrich has already been preprocessed)
		gsca <- preprocess(gsca, species="Hs", initialIDs = "Entrez.gene", 
				keepMultipleMappings = TRUE, duplicateRemoverMethod = "max", 
				orderAbsValue = FALSE)
		##do hypergeometric tests and GSEA
		gsca.rslt[[i]] <- analyze(gsca, para = list(pValueCutoff = 0.05, 
						pAdjustMethod = "BH", nPermutations = 1000, 
						minGeneSetSize = 15, exponent = 1))
	}
	if(is(getOption("cluster"), "cluster")) {
		stopCluster(getOption("cluster"))
		options(cluster=NULL)
	}
	gsca.rslt
}
################################BioNet analyses################################
BioNetAnalyses<-function(network, pval, pheno, fdr=1e-6) {
	bionet.rslt<-list()
	for(i in 1:length(pval)) {
		fb <- fitBumModel(pval[[i]], plot = FALSE)
		scores <- scoreNodes(network = network, fb = fb, fdr = fdr)
		module <- runFastHeinz(network = network, scores = scores)	
		bionet.rslt[[i]]<-list(module=module, pheno=pheno[[i]][nodes(module)], 
				score=scores[nodes(module)])
	}
	bionet.rslt
}
###############################################################################
##############################Figure pipeline##################################
###############################################################################
gscoPipeline4RedeR <-function() {
	ER.gsco <-gscaFigures()
	save(ER.gsco,file="ER.gsco.RData")
}
ppiPipeline4RedeR <-function() {
	ER.ppi<-BioNetFigures()
	save(ER.ppi,file="ER.ppi.RData")
}
##################################GSCA figures#################################
##gsca figures
gscaFigures<-function() {
	data(gsca4RedeR)
	gsca<-gsca4RedeR$gsca
	hits<-gsca4RedeR$hits
	pheno<-gsca4RedeR$pheno	
	time.series<-c("3h", "6h", "12h")
	#rdp <- RedPort('MyPort')
	#calld(rdp)
	##append GO Terms 
	for(i in 1:3) {
		gsca[[i]]<-appendGSTerms(gsca[[i]], keggGSCs="PW_KEGG", goGSCs=c("GO_BP", "GO_MF"))
	}
	igList<-list()
	for(rslt in c("HyperGeo.results","GSEA.results")){
		igList[[rslt]]<-list()
		##combine all gene set collection results
		for(i in 1:3) {
			for(j in 1:length(gsca[[i]]@result[[rslt]])) {
				gsca[[i]]@result[[paste("all",rslt,sep="")]]<-
						rbind(gsca[[i]]@result[[paste("all",rslt,sep="")]], 
								gsca[[i]]@result[[rslt]][[j]])
			}
		}
		##estract adjusted p-values from results
		pval<-matrix(0, nrow(gsca[[1]]@result[[paste("all",rslt,sep="")]]), 3)
		rownames(pval)<-rownames(gsca[[1]]@result[[paste("all",rslt,sep="")]])

		for(i in 1:3) {
			pval[, i]<-gsca[[i]]@result[[paste("all", rslt, sep="")]][rownames(pval), 
				"Adjusted.Pvalue"]
		}
		##select gene sets with significant p-values in >=1 time points  
		pval.forHeatmap<-pval
		colnames(pval.forHeatmap)<-c("t3", "t6", "t12")
		pval.forHeatmap[pval.forHeatmap==0]<-rnorm(n=sum(pval.forHeatmap==0), 
				mean=1e-26, sd=1e-26)
		pval.forHeatmap<-data.frame(GSTerm=gsca[[1]]@result[[paste("all",rslt,sep="")]][, 
			"Gene.Set.Term"], pval.forHeatmap)
		
		sel<-which(rowSums(pval.forHeatmap[, 2:4]<0.05)>=1)
		maxlen<-max(unlist(lapply(as.character(pval.forHeatmap[sel,1]), nchar)))
		##plot heatmaps of  p-value
		pdf(file=paste(rslt, ".pval.heatmap.pdf",sep=""),width=12+maxlen*0.1, height=5+length(sel)*15/30)
		gseaHeatmap(pval.forHeatmap[sel, , drop=FALSE],  method="average", colorBarLabel="p-value", col.dens="cyan")
		graphics.off()		
		
		##call RedeR to view enrichment maps of significant gene sets
		gscs.names<-c("GO_BP", "GO_MF", "PW_KEGG")
		##define the range of size of gene sets
		gs.size.range<-c(seq(5, 25, by=1), 30, 40)*2
		gs.size.breaks<-c(0, seq(5, 100, by=5), 200, 500, Inf)
		for(i in 1:3) {
			igList[[rslt]][[i]]<-list()
			for(j in 1:length(gscs.names)) {
				ig<-viewEnrichMap(gsca[[i]], resultName=rslt, gscs=gscs.names[j], 
						allSig=TRUE, ntop=NULL, gsNameType="term", displayEdgeLabel=FALSE,
						layout="layout.fruchterman.reingold")
				graphics.off()
				##Rescale size of gene sets in enrichment maps to make them comparable
				V(ig)$size<-gs.size.range[as.integer(cut(x=V(ig)$geneSetSize,breaks=gs.size.breaks))]
				V(ig)$nodeColor<-V(ig)$color
				V(ig)$nodeLineColor<-V(ig)$color
				V(ig)$nodeSize<-V(ig)$size
				V(ig)$nodeAlias<-V(ig)$label
				V(ig)$nodeFontSize<-10
				V(ig)$nodeFontColor<-"#000000"
				if(length(V(ig)$name)>=2) {
					E(ig)$edgeWidth<-E(ig)$width*2
					E(ig)$edgeColor<-E(ig)$color
				}
				ig$bgColor<-"#ffffff"
				ig$gscale<-30
				ig$isNest<-TRUE
				ig$isAssign<-TRUE
				ig$zoom<-20
				ig$nestShape<-"ROUNDED_RECTANGLE"
				ig$nestColor<-"#ffffff"
				ig$nestLineColor<-"#BFBFBF"
				ig$nestAliases<-paste(time.series[i], gscs.names[j],sep=" ")
				ig$nestFontSize<-80
				ig$nestLineWidth<-10
				ig$nestFontX<-5
				ig$nestFontY<-10.8
				ig$nestFontColor<-"#000000"
				igList[[rslt]][[i]][[j]]<-ig
				names(igList[[rslt]][[i]])[j]<-gscs.names[j]
				#ct<-addGraph( rdp, ig, layout.fruchterman.reingold(ig), 
				#			nest=TRUE,gcoord=c(15+(i-1)*35, 20+(j-1)*35))
			}
			names(igList[[rslt]])[i]<-c("t3", "t6", "t12")[i]
		}
		#readline(prompt = "Press <Enter> to continue...") 
		##reset RedeR to load another set of enrichment maps
		#resetd(rdp)
	}
	names(igList)<-c("hypergeo","gsea")
	return(igList)	
}
##A function to plot heatmaps of gene set enrichment or hypergeometric test p-values
gseaHeatmap<-function(pval, method, colorBarLabel="p-value", col.dens="cyan") {
	scale01 <- function(x, low = min(x), high = max(x)) {
		x <- (x - low)/(high - low)
		x
	}
	cols <- c(colorRampPalette(c("darkred", "orange"))(9), colorRampPalette(c("white", "darkblue"))(9))
	nr<-nrow(pval)
	dcols<-2:ncol(pval)
	m1 <- 1
	m2 <- 1
	m3 <- 1
	m4 <- 3
	##layout
	layout(matrix(1:4,2),width=c(2, 8),height=c(1, 0.1*nr))
	##clustering
	cdist<-as.dist(1 - cor(pval[, dcols]))
	rdist<-as.dist(1 - cor(t(pval[, dcols])))
	c.cl <- hclust(cdist, method=method)
	r.cl <- hclust(rdist, method=method)
	c.de <- as.dendrogram(c.cl)
	c.ord <- c.cl$order
	r.de <- as.dendrogram(r.cl)
	r.ord <- r.cl$order
	
	## field 1 : colorbar correlation matrix
	par(mar=c(12,m2,12,m4))
	z <- seq(0, 1, length = length(cols))
	image(z=matrix(z, ncol = 1), col = cols, axes=FALSE, xaxs="i", 
			xlab="",ylab="",ylim=c(0,1))
	box()
	
	lab <- sort(c(0, 10^(-rev(1:5)), 0.5*10^(-rev(1:5)), c(2:8)*0.1, 1))

	axis(1,at=seq(0,1,length=length(lab)),labels=lab,cex.axis=1)
	mtext(colorBarLabel,side=3,line=.3,at=0,adj=0,cex=1.5)

	hx<-0:length(lab)/length(lab)
	hy<-c(0, as.integer(table(cut(as.numeric(as.matrix(pval[, dcols])), breaks=lab))), 0)
	lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", col = col.dens)
	
	## field 2 : left dendrogram    
	par(mar=c(10,m2,m3,2),cex=.1)
	plot(rev(r.de),horiz=TRUE,axes=FALSE,yaxs="i")
	## field 3 : top dendrogram
	righttexts<-rev(as.character(pval[,1])[r.ord])
	maxlen<-max(unlist(lapply(righttexts, nchar)))
	par(mar=c(20,m1,m2,maxlen*600/80),cex=0.1)  
	plot(c.de,axes=FALSE,xaxs="i")
	mtext(text=colnames(pval[,dcols])[c.ord], side=1, at=1:length(dcols), 
			cex=2.5, line=10)
	## field 4 : Correlation matrix
	par(mar=c(10,m2,m3,maxlen*600/80), cex=0.1)
	image(x=1:length(dcols),y=1:nr,as.matrix(t(pval[rev(r.ord),c.ord+1])), 
			col=cols, breaks=lab, xaxt="n",yaxt="n", axes=FALSE)
	mtext(side=4, at=seq(1, nr, by=1), text=righttexts, las=2, cex=1.4, line=2)
	box()
}
##################################Bionet figures################################
BioNetFigures<-function() {
	data(hs.inter)
	data(bionet4RedeR)
	pheno<-bionet4RedeR$pheno
	subnets<-bionet4RedeR$subnets	
	data(ER.limma)
	restab<-ER.limma$resdeg
	time.series<-c("3h", "6h", "12h")
	##build igraphs for network and module
	##coloring nodes
	ig.net<-hs.inter
	#ig.net<-igraph.from.graphNEL(graphNEL=hs.inter, weight=FALSE, unlist.attrs =FALSE)
	##commom node attributes
	V(ig.net)$nodeAlias<-""
	inds<-match(V(ig.net)$name, as.character(restab[, "ENTREZ"]))
	V(ig.net)$nodeAlias[which(!is.na(inds))]<-as.character(restab[inds[!is.na(inds)], "Symbol"])
	V(ig.net)$nodeFontSize<-25
	##commom edge attribute
	E(ig.net)$edgeColor<-gray(0.8)
	igList<-list()
	for(i in 1:length(subnets)) {
		##coloring nodes
		ig.net<-ColoringNodes(ig.net,pheno[[i]],neg.col="darkblue", pos.col="darkred", sep.col="white", col.default=gray(0.7))
		subnet<-igraph.from.graphNEL(graphNEL=subnets[[i]]$module, weight=FALSE,unlist.attrs=FALSE)
		subnet<-subgraph(ig.net,V(subnet)$name)
		##container attributes
		V(subnet)$nodeFontSize<-60
		E(subnet)$edgeColor<-gray(0.8)
		E(subnet)$edgeWidth<-10
		V(subnet)$nodeSize<-75
		subnet$nestAliases<-time.series[i]
		subnet$isNest<-TRUE
		subnet$isAssign<-TRUE
		subnet$zoom<-10
		subnet$nestShape<-"ROUNDED_RECTANGLE"
		subnet$nestColor<-"#ffffff"
		subnet$nestFontSize<-160
		subnet$nestLineWidth<-10
		subnet$nestFontX<-5
		subnet$nestFontY<-10.8		
		##save subgraphs for RedeR
		igList[[i]]<-subnet
		names(igList)[i]<-c("t3", "t6", "t12")[i]
	}
	return(igList)
}
##coloring nodes according to differential expression levels (log fold changes)
##input igraph (ig) and phenotypes (pheno), coloring template; output updated igraph
##color templates
ColoringNodes<-function(ig, pheno,  neg.col="darkblue", pos.col="darkred", 
		sep.col="white", breaks=1:15*0.2, col.default=gray(0.7)) {
	myNodeColScale<-function(x, breaks=1:10, neg.col="darkblue",pos.col="darkred", sep.col) {
		negColPl<-colorRampPalette(colors = c(neg.col, sep.col))
		posColPl<-colorRampPalette(colors = c(sep.col, pos.col))
		x.col<-rep("", length(x))
		names(x.col)<-names(x)
		##n means the number of color breaks in each half of the color palette
		negCols<-negColPl(length(breaks)+2)[1:(length(breaks)+1)]
		x.col[x<0]<-negCols[as.integer(cut(x[x<0], breaks=c(-Inf, rev(-breaks), 0)))]
		posCols<-posColPl(length(breaks)+2)[2:(length(breaks)+2)]
		x.col[x>0]<-posCols[as.integer(cut(x[x>0], breaks=c(0, breaks, Inf)))]
		x.col
	}
	##colors of nodes
	V(ig)$nodeColor<-col.default
	inds<-match(names(pheno), V(ig)$name)
	V(ig)$nodeColor[inds[!is.na(inds)]]<-myNodeColScale(pheno[which(!is.na(inds))], 
			breaks=breaks, neg.col=neg.col, pos.col=pos.col, sep.col=sep.col)
	V(ig)$nodeLineColor<-V(ig)$nodeColor
	##shape of nodes
	V(ig)$nodeShape<-"ELLIPSE"
	V(ig)$nodeShape[inds[is.na(inds)]]<-"DIAMOND"
	ig
}
####Retrieve legends for Enrichmaps
enrichMapLegendRetrieve<-function() {
	##node Color
	p.cutoff.vec<-c(0, 10^c(-3, -2.5), 0.01, 10^(-1.5), 0.05, 10^(-c(1.0, 0.5, 0)))
	
	redCols<-colorRampPalette(colors = c("red", "white"))
	redVec<-redCols(length(p.cutoff.vec))
		
	blueCols<-colorRampPalette(colors = c("blue", "white"))
	blueVec<-blueCols(length(p.cutoff.vec))
		
	colVec<-c(redVec[1:length(redVec)],rev(blueVec))
		
	p.cutoff.labels<-rep("",length(colVec))
	p.cutoff.labels[c(1,4,6, 9,10, 13,15,18)]<-c(0,0.01,0.05,1,1,0.05,0.01,0)

	##node Size
	##define the range of size of gene sets
	gs.size.range<-c(seq(5, 25, by=1), 30, 40)*2
	gs.size.breaks<-c(0, seq(5, 100, by=5), 200, 500, Inf)
	
	##edge width--Jaccard indices
	edge.width.range<-1:14
	edge.width.breaks<-round(c(seq(0, 100, by=100/14)))
	
	legendList<-list()
	legendList$nodeColor<-list(nodeColor=colVec, breaks=p.cutoff.labels)
	legendList$nodeSize<-list(nodeSize=gs.size.range, breaks=gs.size.breaks)
	legendList$edgeWidth<-list(edgeWidth=edge.width.range, breaks=edge.width.breaks)
	return(legendList)
}
####Retrieve legends for bionet
bioNetLegendRetrieve<-function() {
	neg.col="darkblue"
	pos.col="darkred"
	sep.col="white"
	breaks=1:15*0.2
	col.default=gray(0.7)
	negColPl<-colorRampPalette(colors = c(neg.col, sep.col))
	posColPl<-colorRampPalette(colors = c(sep.col, pos.col))
	negCols<-negColPl(length(breaks)+2)[1:(length(breaks)+1)]
	posCols<-posColPl(length(breaks)+2)[2:(length(breaks)+2)]
	colVec<-c(negCols, posCols)
	breakVec<-c(rev(-breaks), 0, 0, breaks)
	legendList<-list(nodeColor=colVec, breaks=breakVec)
	return(legendList)
}
















