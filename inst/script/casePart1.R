
################################RedeR case study############################### 
###source codes for limma, clustering, and co-expression gene network anlysis##
###############################################################################
# packages
library(RedeR)
library(limma)
library(snow)
library(pvclust)
library(gplots)
##########################run limma analysis pepeline##########################
deaPipeline<-function(){
	data(Carroll2006)
	limmaAnalysis()
}
# plot some checks for limma analysis
deaFigures<-function(){
	data(ER.limma)
	limmaFigures(Carroll2006, ER.limma)
}
########################run clustering analysis pepeline########################
#...ps. check parallel computing carefully at pvclust package! 
#...this may take a long time!
clustPipeline<-function(nboot=100){
	pvclustAnalysis(ER.limma,nboot=nboot, spec=4) #set nboot to 1000 or 10000
}
# plot figs from pvclust analysis
clustFigures<-function(){
	data(ER.pvclust)
	pvclustFigures(ER.pvclust$pvclust)
}
######################run co-expression analysis pepeline#######################
ceaPipeline<-function(){
	data(ER.pvclust)
	#ER.correl<-cea(ER.pvclust$expclt, nper=1000, plot=FALSE)
	#plotcea(ER.correl)
	ER.correl<-cea.global(ER.pvclust$expclt)
	return(ER.correl)
}
ceaFigures<-function(ER.correl){
	data(ER.pvclust)
	ceaHeatmap(ER.correl,ER.pvclust$expclt)
}
######################run ChIP-on-chip mapping pepeline#########################
##map ER binding sites for co-expressed gene clusters
pvchipPipeline<-function(){
	data(Carroll2006)
	data(TSS.map)
	data(ER.pvclust)
	pvChipExp<-chipchip(Carroll2006, ER.pvclust, TSS.map)
	return(pvChipExp)
}
pvchipFigures<-function(pvChipExp){
	tssplot(pvChipExp)
}
####################run final pre-processed data pepeline#######################
# save final pre-processed data
dataPipeline4RedeR<-function(ER.correl,pvChipExp){
	ER.endata <-list()
	ER.endata$adjMt<-ER.correl$decision.mt
	ER.endata$data<-pvChipExp
	save(ER.endata,file=file.path("ER.endata.RData"))
}
###################run final pre-processed graph pepeline######################
# save pre-processed graph
graphPipeline4RedeR<-function(){
	data(ER.endata)
	ER.glist<-glist.processed(ER.endata$adjMt,ER.endata$data, clusters=c(1:3,10,20,30,40))
	save(ER.glist,file=file.path("ER.glist.RData"))
	ER.glistall<-glist.processed(ER.endata$adjMt,ER.endata$data, clusters=c(1:42))
	save(ER.glistall,file=file.path("ER.glistall.RData"))
	# save pre-formated series
	ER.gseries<-list()
	ER.gseries$g3h<-subg.processed(ER.endata$adjMt,ER.endata$data,cluster=2,fctime=3)
	ER.gseries$g6h<-subg.processed(ER.endata$adjMt,ER.endata$data,cluster=2,fctime=6)
	ER.gseries$g12h<-subg.processed(ER.endata$adjMt,ER.endata$data,cluster=2,fctime=12)
	ER.gseries <-setCommomAttribs(ER.gseries)
	save(ER.gseries,file=file.path("ER.gseries.RData"))
}
###############################################################################
################################limma analysis#################################
###############################################################################
limmaAnalysis<-function(){
	# build design matrix
	t<-factor(Carroll2006$tgs$Time)
	design<-model.matrix(~0+t)
	# fit model
	fit<-lmFit(as.matrix(Carroll2006$exp),design)
	# set contrasts
	contrasts<-makeContrasts(t3-t0,t6-t0,t12-t0,levels=design)                                                                                                               
	# eBayes correction
	ct.fit<-eBayes(contrasts.fit(fit, contrasts)) 
	# run decide test
	res.fit<-decideTests(ct.fit,method="global", adjust.method="BH",
	                     p.value=0.01,lfc=1)
	# ***MAIN REPORT FOR THE CASE STUDY ON 'CARROLL2006' DATASET***
	# get DE genes
	ER.deg<-list()
	ER.deg$DE3h<-rownames(res.fit)[res.fit[,1]!=0]
	ER.deg$DE6h<-rownames(res.fit)[res.fit[,2]!=0]
	ER.deg$DE12h<-rownames(res.fit)[res.fit[,3]!=0]
	ER.deg$DEall<-unique(c(ER.deg$DE3h, ER.deg$DE6h, ER.deg$DE12h))
	# get adjust p.value
	p.value <- as.matrix(ct.fit$p.value)
	p.value.adj <- p.value
	for (j in 1:ncol(p.value)){
		p.value.adj[, j] <- p.adjust(p.value[,j], method = "BH")
	}
	# set summary
	ER.resdeg<-list()
	ER.resdeg$ENTREZ       <- Carroll2006$ids[[1]]
	#ER.resdeg$RefSeqID     <- Carroll2006$ids[[2]]
	ER.resdeg$Symbol       <- Carroll2006$ids[[3]]
	ER.resdeg$coef         <- ct.fit$coef
	ER.resdeg$p.value      <- ct.fit$p.value
	ER.resdeg$p.value.adj  <- p.value.adj
	ER.resdeg$degenes      <- unclass(res.fit)
	ER.resdeg<-data.frame(ER.resdeg, check.names=FALSE)
	# get DE gene matrix
	ER.expdeg<-Carroll2006$exp[is.element(rownames(Carroll2006$exp),ER.deg$DEall),]
	# save report
	ER.limma<-list()
	ER.limma$deg<-ER.deg
	ER.limma$resdeg<-ER.resdeg
	ER.limma$expdeg<-ER.expdeg
	save(ER.limma, file=file.path("ER.limma.RData"))
}
###############################################################################
#####################Figures DE genes from limma analysis######################
###############################################################################
limmaFigures<-function(Carroll2006, ER.limma){
	pca1Figures(Carroll2006)
	pca2Figures(ER.limma)
	vennFigures(Carroll2006, ER.limma)
	heatmap1Figures(ER.limma, Carroll2006)
	heatmap2Figures(ER.limma)	
}
### PCA for all genes in the array
pca1Figures<-function(Carroll2006){
	dataset<-as.data.frame(t(na.omit(Carroll2006$exp)))
	p3scores = prcomp(dataset,retx=TRUE, center=TRUE, scale=TRUE)$x
	# plot
	pdf(file=file.path("pcaAll.pdf"))
	plot(p3scores[,1], p3scores[,2],xlab="PCA 1",ylab="PCA 2",type="p",pch=19,
	   col=c('orange','red','green','blue')[as.factor(Carroll2006$tgs$Time)], cex=1.5, 
	   xlim=c(min(p3scores[,1])*1.1, max(p3scores[,1])*1.1),
	   ylim=c(min(p3scores[,2]*1.1), max(p3scores[,2])*1.1) )
	legend("bottomright",c("0h","3h","6h","12h"),pch=19, 
	       col=c('orange','red','green','blue'),bty="n", cex=1.0)
	leg<-paste("All genes (n=",length(names(dataset)),")",sep="")
	mtext(leg,side = 3,adj = 0, cex = 1.2)
	dev.off()
}
### PCA for DE genes from limma
pca2Figures<-function(ER.limma){
	dataset<-as.data.frame(t(na.omit(ER.limma$expdeg)))
	p3scores = prcomp(dataset, retx=TRUE, center=TRUE, scale=TRUE)$x
	# plot
	pdf(file=file.path("pcaDEG.pdf"))
	plot(p3scores[,1],p3scores[,2],xlab="PCA 1",ylab="PCA 2",type="p",pch=19,
	   col=c('orange','red','green','blue')[as.factor(Carroll2006$tgs$Time)], cex=1.5, 
	   xlim=c(min(p3scores[,1])*1.1, max(p3scores[,1])*1.1),
	   ylim=c(min(p3scores[,2]*1.1), max(p3scores[,2])*1.1) )
	legend("topright",c("0h","3h","6h","12h"),pch=19, 
	       col=c('orange','red','green','blue'),bty="n", cex=1.0)
	leg<-paste("DE genes (n=",length(names(dataset)),")",sep="")
	mtext(leg,side = 3,adj = 0, cex = 1.2)
	dev.off()
}
### Venn diagram for DE genes from limma
vennFigures<-function(Carroll2006, ER.limma){
	universe <- rownames(Carroll2006$exp)
	Counts <- matrix(0, nrow=length(universe), ncol=3)
	for (i in 1:length(universe)){
  		Counts[i,1] <- universe[i] %in% ER.limma$deg$DE3h
  		Counts[i,2] <- universe[i] %in% ER.limma$deg$DE6h
  		Counts[i,3] <- universe[i] %in% ER.limma$deg$DE12h
	}
	pdf(file=file.path("venn.pdf"))
	vennDiagram(Counts,cex=1.2,circle.col=c("red","green","blue"),
	            names=c("t3-t0","t6-t0","t12-t0"),lwd=2)
	text(-0.15,3,"DE genes",cex=1.2)
	dev.off()
}
### heatmap for DE genes from limma (euclidean distance)
heatmap1Figures<-function(ER.limma, Carroll2006){
	geneName<-ER.limma$resdeg$Symbol[pmatch(rownames(ER.limma$expdeg), rownames(ER.limma$resdeg))]
	sampleName<-as.factor(paste("t", Carroll2006$tgs$Time, sep = ""))
	#color ramp
	cc<-c("cyan","black","red")
	color.code <- colorRampPalette(cc)(128)
	dend<-as.dendrogram(hclust(dist(ER.limma$expdeg, method='euclidean'),method="average"))
	#pdf(file=file.path("heatmapEuc.pdf"))
	png(file=file.path("heatmapEuc.png"), width = 480, height = 480)
	heatmap(as.matrix(ER.limma$expdeg),
  		Rowv=dend,
  		scale="row",
  		labCol=sampleName,cexCol=1.8,
  		labRow=geneName,cexRow=0.2,
  		col=color.code,
  		reorderfun=function(d,w) reorder(d,w,agglo.FUN=mean),
  		#xlab="samples", ylab="DE genes",
  		cex=1.4)
		#mtext("Blue-red scale: log2-expression level",side=1,line=(-1),outer=TRUE,cex=0.8,adj=0,font=2)
		dev.off()
}
### heatmap for DE genes from limma (correlation as diatance)
heatmap2Figures<-function(ER.limma){
	correl<-cor(t(ER.limma$expdeg), method="pearson")
	geneName<-ER.limma$resdeg$Symbol[pmatch(rownames(correl), rownames(ER.limma$resdeg))] 
	cc<-c("#00007F","blue","#007FFF","cyan","#7FFF7F", "yellow","#FF7F00","red","#7F0000")
	color.code <- colorRampPalette(cc, bias = 0.7)(128)
	dend<-as.dendrogram(hclust(as.dist(1-correl),method="average"))
	#pdf(file=file.path("heatmapCor.pdf"))
	png(file=file.path("heatmapCor.png"), width = 480, height = 480)
	heatmap(x=correl,
  		Rowv=dend,symm=TRUE,
  		scale="none",
  		labCol=geneName,cexCol=0.2,
  		labRow=geneName,cexRow=0.2,
  		col=color.code,
  		#xlab="samples", ylab="DE genes",
  		cex=1.4)
		#mtext("Blue-red scale: Pearson correlation",side=1,line=(-1),outer=TRUE,cex=0.8,adj=0,font=2)
		dev.off()
}
###############################################################################
#############################clustering analysis###############################
###############################################################################
### find optimal clustering with correlation as distance
pvclustAnalysis<-function(ER.limma,nboot=1000,spec=4){
	## parallel computation via snow package
	cl <- makeCluster(spec=spec, type="SOCK")
	# run parallel version of pvclust
	ER.pvclust <- parPvclust(cl,t(ER.limma$expdeg), method.hclust="average", 
	                         method.dist="correlation", nboot=nboot)
	stopCluster(cl)
	# plot pv clust
	#pdf(file=file.path("pvclust.pdf"),width=12, height=5)
	#plot(ER.pvclust, cex=0.5)
	#pvrect(ER.pvclust, type="geq", alpha=0.99)
	#dev.off()
	## get cluster assignments
	clt <-pvpick(ER.pvclust, type="geq", alpha=0.99)$clusters
	cltedges <-pvpick(ER.pvclust, type="geq", alpha=0.99)$edges
	clusters<-list(NA)
	pvalue<-list(NA)
	cltlab <-NA
	for(i in 1:length(clt)){
		if(length(clt[[i]])>3){
			clusters<-c(clusters,clt[i])
			cltlab <-c(cltlab, clt[[i]])
			edidx<-cltedges[i]
			pval.au <-1-ER.pvclust$edges$au[edidx]
			pvalue<-c(pvalue,pval.au)
		}			
	}
	ER.clt<-clusters[-1]
	ER.labclt<-cltlab[-1]
	ER.pvalue<-pvalue[-1]
	# get matched matrices
	ER.expclt<-ER.limma$expdeg[pmatch(ER.labclt,rownames(ER.limma$expdeg)),]
	ER.resclt<-ER.limma$resdeg[pmatch(ER.labclt,rownames(ER.limma$resdeg)),]
	pvCorrelation <-list()
	pvCorrelation$pvclust<-ER.pvclust
	pvCorrelation$clt<-ER.clt
	pvCorrelation$labclt<-ER.labclt
	pvCorrelation$expclt<-ER.expclt
	pvCorrelation$resclt<-ER.resclt
	pvCorrelation$pvalclt<-ER.pvalue
	ER.pvclust<-pvCorrelation
	save(ER.pvclust,file=file.path("ER.pvclust.RData"))
}
# plot clustering results from pvclust package
pvclustFigures<-function(ER.pvclust){
	pdf(file=file.path("pvclust.pdf"),width=30, height=4.5)
	plot(ER.pvclust, cex=0.4, cex.pv=0.4, print.num=FALSE)
	pvrect(ER.pvclust, type="geq", alpha=0.99)
	dev.off()
}
###############################################################################
###########################co-expression analysis##############################
###############################################################################
cea<-function(expdata, fdr=1e-6,method="pearson",type="p", 
nper=1000, plot=FALSE, gnames=NULL){
  #..for t-test option, type='t'
  #..for random permutation option, type='p'	
  dataMt<-t(expdata)
  if(type=="t"){
  	#t-tets option:
	#..it tests association/correlation between paired samples; it uses 'cor.test' 
	#..function from the 'stats' package; as input, accepts data matrix with features in 
	#..rows (genes) and samples in cols (microarrays). Computes adjusted p.values and
  	corrMt 	  <- cor(t(expdata), method = method)
  	nfeatures <- ncol(dataMt)
  	pvalueCor <- matrix(NA,nrow =nfeatures,ncol=nfeatures,
  	                    dimnames=list(colnames(dataMt),colnames(dataMt)))
  	pvalueAdj <- pvalueCor
  	pVec      <- array(NA, dim = nfeatures*nfeatures)
  	k<-0
  	for(i in 1:nfeatures){
		for(j in 1:nfeatures){	
			ct <- cor.test(dataMt[, i], dataMt[, j], method=method, exact=FALSE)
			k <- k+1
			pvalueCor[i,j] <- ct$p.value
			pVec[k] <- ct$p.value
		}
  	}
  	# adjusts pvalue
  	pVecAdj <- p.adjust(pVec,method="fdr")
  	k<-0
  	for(i in 1:nfeatures){
		for(j in 1:nfeatures){
			k<-k+1
			pvalueAdj[i,j]<-pVecAdj[k]
		}
  	}
  	# decides on the significance
  	decision<-pvalueAdj> fdr
  	decisionMt<-corrMt
  	decisionMt[decision]<-0.0
  	#..diagonal has no meaning here!
  	diag(corrMt) <- 0.0
  	diag(pvalueAdj) <- 1.0
  	diag(decisionMt) <- 0.0
  	# returns the result
  	return(list(correl.mt=corrMt, adj.pvalue=pvalueAdj, decision.mt=decisionMt))  
  } else if(type=="p"){
  	#random permutation option:
	#..it builds a null distribuions for each feature pair (e.g. for each
	#..correlation tested); sig. is assessed on local null distribuion; as input, accepts 
	#..data matrix with features in rows (genes) and samples in cols (microarrays)
  	nfeatures <- ncol(dataMt)  
  	perCorMt  <- array(NA, dim=c(nfeatures, nfeatures, nper)) 
  	# builds the null distribution
  	for (i in 1:nper){
  		perMt<-apply(dataMt,2,sample)
  		perMt<-apply(perMt,1,sample)
  		perMt<-t(perMt)
  		perCorMt[,,i] <- cor(perMt, method = method)
  		diag(perCorMt[,,i])<-NA
  	}
  	colnames(perCorMt)<-colnames(dataMt)
  	rownames(perCorMt)<-colnames(dataMt)  
  	# gets correlation matrix
  	corrMt    <- cor(dataMt, method = method)
  	# prepares matrix to compute probs (splits positive and negative cor.)
  	posCorrMt <- corrMt
  	negCorrMt <- corrMt
  	posCorrMt[posCorrMt<=0]<-NA
  	negCorrMt[negCorrMt>=0]<-NA
  	diag(posCorrMt)<-NA
  	diag(negCorrMt)<-NA
  	gtCorrMt  <- matrix(NA, ncol=nfeatures, nrow=nfeatures)
  	ltCorrMt  <- matrix(NA, ncol=nfeatures, nrow=nfeatures) 
  	# maps correlation values on the null distribution
  	for (i in 1:nper){
  		gtCorr  <- posCorrMt>perCorMt[,,i]
  		ltCorr  <- negCorrMt<perCorMt[,,i] 	
  		if(i==1){
  			gtCorrMt[,]  <- as.numeric(gtCorr)
  			ltCorrMt[,]  <- as.numeric(ltCorr)
  		} else {
  			gtCorrMt[,]  <- as.numeric(gtCorrMt) + as.numeric(gtCorr)
  			ltCorrMt[,]  <- as.numeric(ltCorrMt) + as.numeric(ltCorr)
  		}
  	}
  	# sets 'NAs' to 0 and combines results in 'probs' matrix
  	gtCorrMt[is.na(gtCorrMt)] <- 0.0
  	ltCorrMt[is.na(ltCorrMt)] <- 0.0
  	probs <- 1.0 - (gtCorrMt+ltCorrMt)/nper
  	# adjusts pvals
  	colnames(probs)<-colnames(dataMt)
  	rownames(probs)<-colnames(dataMt)
  	pvalueAdj <- probs
  	pVec      <- probs
  	pVecAdj   <- p.adjust(pVec, method ="fdr")
  	k<-0
  	for(i in 1:nfeatures){
		for(j in 1:nfeatures){
			k<-k+1
			pvalueAdj[i,j]<-pVecAdj[k]
		}
  	}
  	# decides on the significance
  	decision<-pvalueAdj>fdr
  	decisionMt<-corrMt
  	decisionMt[decision]<-0.0
  	if(plot==TRUE){
  		if(type=="p")plotcea(list(corr.mt=corrMt,decision.mt=decisionMt,null.mt=perCorMt))
  	}
  	# returns the result
  	return(list(corr.mt=corrMt, adj.pvalue=pvalueAdj, decision.mt=decisionMt, null.mt=perCorMt))
  } # end 'type' option
}
## random permutation (global null distribuion)
#..builds one global null distribuions for the whole analysis using the same approach as above
cea.global <- function(expdata, fdr=1e-6, method="pearson", peround=100){
  # transposes the input dataset matrix (sets features as cols!)
  dataMt <- t(expdata)
  nfeatures <- ncol(dataMt)  
  perCorMt  <- array(NA, dim=c(nfeatures, nfeatures, peround)) 
  # builds the null distribution via permutation
  for (i in 1:peround){
  	perMt<-apply(dataMt,2,sample)
  	perMt<-apply(perMt,1,sample)
  	perMt<-t(perMt)
  	perCorMt[,,i] <- cor(perMt, method = method)
  	diag(perCorMt[,,i])<-NA
  }
  colnames(perCorMt)<-colnames(dataMt)
  rownames(perCorMt)<-colnames(dataMt)  
  # gets correlation matrix
  corrMt    <- cor(dataMt, method = method)
  # prepares matrices to compute probs (splits positive and negative cor.)
  posCorrMt <- corrMt
  negCorrMt <- corrMt
  posCorrMt[posCorrMt<=0]<-NA
  negCorrMt[negCorrMt>=0]<-NA
  diag(posCorrMt)<-NA
  diag(negCorrMt)<-NA
  nullDist <- as.numeric(perCorMt)
  nullDist <- nullDist[!is.na(nullDist)]
  gtCorrMt  <- matrix(NA, ncol=nfeatures, nrow=nfeatures)
  ltCorrMt  <- matrix(NA, ncol=nfeatures, nrow=nfeatures)
  # maps correlation values on the null distribution 
  nullDist<-sort(nullDist)
  for(i in 1: nfeatures){	
  		gtCorrMt[i,] <- findInterval(posCorrMt[i,],nullDist)
  		ltCorrMt[i,] <- length(nullDist) - findInterval(negCorrMt[i,],nullDist)
  }
  # sets 'NAs' to 0 (..to compute properly the counts) and combines the results in 'probs' matrix
  gtCorrMt[is.na(gtCorrMt)] <- 0.0
  ltCorrMt[is.na(ltCorrMt)] <- 0.0
  probs <- 1.0 - (gtCorrMt+ltCorrMt)/length(nullDist)
  # ddjusts pvals
  colnames(probs)<-colnames(dataMt)
  rownames(probs)<-colnames(dataMt)
  pvalueAdj	<- probs
  pVec     	<- probs
  pVecAdj 	<- p.adjust(pVec, method ="fdr")
  k<-0
  for(i in 1:nfeatures){
	for(j in 1:nfeatures){
		k<-k+1
		pvalueAdj[i,j]<-pVecAdj[k]
	}
  }
  # decides on the significance
  decision<-pvalueAdj>fdr
  decisionMt<-corrMt
  decisionMt[decision]<-0.0
  return(list(corr.mt=corrMt, adj.pvalue=pvalueAdj, decision.mt=decisionMt))
}
##################################cea figures##################################
# plot decision matx from 'cea' function overlayed to the null distribution 
plotcea<-function(correl){
	if(!is.null(file))
	null.dist <-as.numeric( correl$null.mt[,,1:min(100,dim(correl$null.mt)[3])] )
	null.dist <- hist(null.dist, plot = FALSE,breaks = 100)
	cbreaks <- null.dist$breaks
	#
	decision.dist <-as.numeric(correl$decision.mt[correl$decision.mt!=0])
	decision.dist <- hist(decision.dist, plot = FALSE,breaks = 100)
	dbreaks <- decision.dist$breaks
	#
	corr.dist <-as.numeric(correl$corr.mt)
	corr.dist <- hist(corr.dist, plot = FALSE,breaks = 100)
	cbreaks <- corr.dist$breaks	
	#
	pdf(file=file.path("decision.pdf"))
	plot.new()
	
	plot.window(xlim = c(-1,1), ylim = range(0, null.dist$counts))
	rect(cbreaks[-length(cbreaks)], 0, cbreaks[-1], null.dist$counts,col='grey',border = 'grey40')
	axis(2,cex.axis=0.7) 
		
	plot.window(xlim = c(-1,1), ylim = range(0, corr.dist$counts))
	rect(cbreaks[-length(cbreaks)], 0, cbreaks[-1], corr.dist$counts,col='red',border='red4')
	#rect(dbreaks[-length(dbreaks)], 0, dbreaks[-1], decision.dist$counts,col='red',border='red4')  
	axis(4,cex.axis=0.7)
		
	axis(1,cex.axis=1.0)
	title(main="Gene co-expression analysis",xlab = "Correlation coefficient", 
	       ylab="Frequency")
	legend("topleft", max(decision.dist$counts*10), fill = c("grey", 'red'),
	        legend = c("null distribution","correlation distribution"),
	        bty="n", cex=0.7)
	dev.off()
}
# plot correlation and adjacency matxs from 'cea' function
ceaHeatmap<-function(ER.correl,resclt){
	geneName<-resclt$Symbol[pmatch(rownames(ER.correl$decision.mt),rownames(resclt))]
	dend<-as.dendrogram(hclust(as.dist(1-ER.correl$corr.mt),method="average"))
	cc<-c('blue','lightblue','cyan','white','white','yellow', 'orange','red')
	color.code <- colorRampPalette(cc, bias = 0.85)(128)
	png(file=file.path("correlationMtx.png"),width = 280, height = 280)
	heatmap(x=ER.correl$corr.mt,
	  Rowv=dend,symm=TRUE,
	  scale="none",
	  labCol=geneName,cexCol=0.4,
	  labRow=geneName,cexRow=0.4,
	  col=color.code,
	  #xlab="DE genes", ylab="DE genes",
	  cex=1.4)
	  #mtext("Correlation matrix", side = 1, line = -2, outer = T,cex=0.9,adj = 0,font=2)
	  #mtext("Blue-red scale: Pearson correlation", side=1, line= -1, outer=T, cex=0.9,adj=0, font=2)
	dev.off()
	png(file=file.path("adjacencyMtx.png"), width = 280, height = 280)
	heatmap(x=ER.correl$decision.mt,
	  Rowv=dend,symm=TRUE,
	  scale="none",
	  labCol=geneName,cexCol=0.4,
	  labRow=geneName,cexRow=0.4,
	  col=color.code,
	  #xlab="DE genes", ylab="DE genes",
	  cex=1.4)
	  #mtext("Adjacency matrix", side = 1, line = -2, outer = T,cex=0.9,adj = 0,font=2)
	  #mtext("Blue-red scale: Pearson correlation", side=1, line= -1, outer=T, cex=0.9,adj=0, font=2)
	dev.off()
}
# get network according to PCIT library
cea.pcit<-function(expdata, weighted=TRUE, absolute=TRUE){
    library(PCIT)
    #Calculate a correlation matrix of the transposed expression data
    corrMt<-cor(t(expdata), method = "pearson")
    #Perform PCIT on the correlation matrix
    resPcit<-pcit(corrMt,force.serial=TRUE,tol.type="max" )
    #Get indices for the meaningful correlations
    sigcorr<-(idx(resPcit))
    #Plot the distribution of meaningful correlations superimposed on all correlations
    #plotCorCoeff(corrMt, list("PCIT Meaningful" = sigcorr), col=c("red"))
    #Get the indices for the non-meaningful correlations
    nonsigcorr <- myIdxInvert(corrMt, sigcorr)
    #Set non-meaningful correlations to zero
    corrMt[nonsigcorr] <- 0
    if(weighted==FALSE){
      corrMt[sigcorr] <- 1
    }
    #Create an adjacency matrix from corrMt, e.g., by using the absolute correlation values
    if(absolute){
    	adjMt <- abs(corrMt)
    } else {
    	adjMt <- corrMt
    }
    return(decision.mt=corrMt)
}
myIdxInvert<-function(m, idx){
    if (is.numeric(m)) {
        nodes <- nrow(m)
    }
    else {
        nodes <- try(nrow(m), silent = TRUE)
        if (class(nodes) == "try-error") {
            cat("ERROR: argument 'm' must be a numeric OR an object on which nrow() can be performed.\n\n", 
                geterrmessage())
            return(FALSE)
        }
    }
    return(setdiff(1:(nodes^2), idx))
}
###############################################################################
#############################ER binding site mapping###########################
###############################################################################
chipchip<-function(Carroll2006, ER.pvclust, TSS.map){
	# get ER binding site from Carroll2006
	bdsites<-Carroll2006$bdsites
	# get limma results already mapped to genes in the clusters
	resclt<-ER.pvclust$resclt
	# map TSS to genes in the clusters
	tssmap<-TSS.map[TSS.map$ENTREZ %in% resclt$ENTREZ,]
	# find the closest ER binding site for each TSS
	idx=array(NA,dim=length(tssmap$TSS))
	delta_bd=array(NA,dim=length(tssmap$TSS))
	delta_bd_ori=array(NA,dim=length(tssmap$TSS))
	for(i in 1:length(tssmap$TSS)){
		chrom=as.character(tssmap$chrom[[i]])
		strand=as.character(tssmap$strand[[i]])	
		txstart=tssmap$TSS[i]
		absdelta=abs(txstart-bdsites$bdMean)
		absdelta[bdsites$bdChrom!=chrom]<-NA
		delta_bd[i]=absdelta[sort.list(absdelta)[1]]
		if(is.na(delta_bd[i])){ 
			idx[i]=NA 
			delta_bd_ori[i]=NA
		} else { 
			idx[i]=sort.list(absdelta)[1] 
			bds=bdsites[idx[i],]$bdMean
			temp=delta_bd[i]
			if(strand=="+"){
				if(bds<=txstart) {delta_bd_ori[i]=(-temp)} else {delta_bd_ori[i]=(temp)}
			} else {
				if(bds>=txstart) {delta_bd_ori[i]=(-temp)} else {delta_bd_ori[i]=(temp)}
			}
		}
	}
	# combine results in one data frame
	bdsite.map<-data.frame(tssmap, bdsites[idx,], bdDist=delta_bd, bdDistOriented=delta_bd_ori)
	bdsite.map<-bdsite.map[pmatch(resclt$ENTREZ, bdsite.map$ENTREZ),]
	resclt <-data.frame(resclt,bdDist=bdsite.map$bdDist, 
			bdDistOriented=bdsite.map$bdDistOriented, check.names=FALSE)
	ER.pvclust$resclt<-resclt
	# get summary and return res
	cltbs<-getSummary(ER.pvclust, expcol=4)
	cltbs<-cltbs$bdsummary
	rownames(cltbs)<-c(1:dim(cltbs)[1])
	ER.pvclust$clustDistSummary<-cltbs
	return(ER.pvclust)
}
# get summary for each cluster
getSummary<-function(ER.pvchip,expcol=1){
	# get list of clusters
	clt <- ER.pvchip$clt	
	resclt <- ER.pvchip$resclt
	# summarise ER bdsite data for each cluster (kb in clt list)
	bd.data <-list(NA)
	bd.summary <- unclass(summary(0))[1:6]
	for(i in 1:length(clt)){
		bd.data[[i]]<-as.numeric(resclt[pmatch(clt[[i]], resclt$ENTREZ),]$bdDistOriented)/1000
		sm<-summary(abs(resclt[pmatch(clt[[i]], resclt$ENTREZ),]$bdDist),na.rm = TRUE)
		sm<-unclass(sm)
		bd.summary <-rbind(bd.summary,sm[1:6])
	}	
	bd.summary<-bd.summary[-1,]
	ER.pvchip$bdsummary<-bd.summary
	ER.pvchip$bddata<-bd.data	
	# summarise diff. expression data for each cluster
	exp.data <-list(NA)
	exp.summary <- unclass(summary(0))[1:6]
	for(i in 1:length(clt)){
		exp.data[[i]]<-as.numeric(resclt[pmatch(clt[[i]], resclt$ENTREZ),][[expcol]])
		sm<-summary(abs(resclt[pmatch(clt[[i]], resclt$ENTREZ),][[expcol]]),na.rm = TRUE)
		sm<-unclass(sm)
		exp.summary <-rbind(exp.summary,sm[1:6])
	}
	exp.summary <-exp.summary[-1,]
	ER.pvchip$logFCsummary<-exp.summary
	ER.pvchip$logFCdata <-exp.data	
	return(ER.pvchip)
}
#plot figures for ChIP-on-chip mapping
tssplot<-function(pvchipexp,distrange=1000,splitExpression=TRUE,upperplot=TRUE, plotfit=TRUE){
	library(gplots)
	if(length(pvchipexp$clt)<=3){
		stop("NOTE: not enough clusters to plot this figure for RedeR case study!")
	}
	# set file dim
	pdf(file=file.path("bdsite_tss.pdf"),width=12, height=4.75)
	lhei=c(0.1,1.0)
	layout(matrix(c(1,0,0,2,3,4), 2, 3, byrow=TRUE), heights = lhei)
	# plot legend (gradient rank)
	par(mar=c(0,5,2.3,10))
	z <- seq(0,1,length.out=42)
	z<-1:42
	cc<-colorRampPalette(c("darkred","red","orange","cyan","blue","darkblue"))(64)
	image(x=z,z=matrix(z,ncol = 1),col=cc,axes=FALSE, xlim=c(1,42),xlab="", ylab="")
	mtext("cluster rank (abs distance from TSS)",cex=0.75,adj=0)
	axis(1,cex.axis=0.8, tick=FALSE, padj=-2)
	box()
	par(mar=c(5, 5, 1, 2) + 0.1)
	# plot fig 3h, 6h, 12h time course analysis
	bdsummary<-list()
	for(k in 4:6){
	# get summary tables and split data according to TSS position (positive, negative)
	pvchipexp<-getSummary(pvchipexp, expcol=k)
	#checks!!!
	#boxplot(t(pvchipexp$bdsummary[sort.list(pvchipexp$bdsummary[,3]),]))
	#pvchipexp$clt[sort.list(pvchipexp$bdsummary[,3])]
	#pvchipexp$bddata[sort.list(pvchipexp$bdsummary[,3])]
	#
	ccp<-pvchipexp$bddata[sort.list(pvchipexp$bdsummary[,3])]
	ccn<-pvchipexp$bddata[sort.list(pvchipexp$bdsummary[,3])]
	dep<-pvchipexp$logFCdata[sort.list(pvchipexp$bdsummary[,3])]
	den<-pvchipexp$logFCdata[sort.list(pvchipexp$bdsummary[,3])]
	pospos<-pvchipexp$logFCdata[sort.list(pvchipexp$bdsummary[,3])]
	negpos<-pvchipexp$logFCdata[sort.list(pvchipexp$bdsummary[,3])]
	posneg<-pospos
	negneg<-negpos
	for(i in 1:length(ccp)){
		pospos[[i]][ccp[[i]]<0]<-NA
		posneg[[i]][ccp[[i]]>0]<-NA
	}
	for(i in 1:length(ccn)){
		negpos[[i]][ccn[[i]]>0]<-NA
		negneg[[i]][ccn[[i]]<0]<-NA
	}
	if(splitExpression){
		for(i in 1:length(dep)){
			pospos[[i]][dep[[i]]<0]<-NA
			posneg[[i]][dep[[i]]>0]<-NA
		}
		for(i in 1:length(den)){
			negpos[[i]][den[[i]]<0]<-NA
			negneg[[i]][den[[i]]>0]<-NA
		}
	} else {
		for(i in 1:length(dep)){
			pospos[[i]]<-abs(pospos[[i]])
			posneg[[i]]<-abs(posneg[[i]])
		}
		for(i in 1:length(den)){
			negpos[[i]]<-abs(negpos[[i]])
			negneg[[i]]<-abs(negneg[[i]])
		}
	}
	if(upperplot){
		posx<-pospos
		negx<-negpos
	} else {
		posx<-posneg
		negx<-negneg
	}
	pp <-data.frame()
	pp2 <-data.frame()
	for(i in 1:length(posx)){
		logfc<-posx[[i]]
		logfc<-logfc[!is.na(posx[[i]])]
		lfmd<-median(logfc,na.rm = TRUE)
		qq<-quantile(logfc, probs = seq(0, 1, 0.25), na.rm = TRUE)
		lfsd<-abs(qq[[4]]-lfmd)	
		qqfc <-logfc
		qqfc[logfc <=qq[[2]]]<-25
		qqfc[logfc>qq[[2]] & qqfc<=qq[[3]]]<-50
		qqfc[logfc>qq[[3]] & qqfc<=qq[[4]]]<-75
		qqfc[logfc>qq[[4]]]<-100			
		#lfsd<-sd(logfc,na.rm = TRUE)
		dt<-ccp[[i]]
		dt<-dt[!is.na(posx[[i]])]
		dtmd<-median(dt,na.rm = TRUE)
		qq<-quantile(dt, probs = seq(0, 1, 0.25), na.rm = TRUE)
		dtsd<-abs(qq[[4]]-dtmd)
		#dtsd<-sd(dt,na.rm = TRUE)
		if(length(logfc)>0 && dtmd<=distrange){
			clus<-i
			tp<-cbind(clus,logfc,lfmd,lfsd,dt,dtmd,dtsd,qqfc)
			pp<-rbind(pp,tp)
			tp<-cbind(clus,lfmd,lfsd,dtmd,dtsd)
			pp2 <-rbind(pp2,tp)		
		}
	}
	np <-data.frame()
	np2 <-data.frame()
	for(i in 1:length(negx)){
		logfc<-negx[[i]]
		logfc<-logfc[!is.na(negx[[i]])]
		lfmd<-median(logfc,na.rm = TRUE)
		qq<-quantile(logfc, probs = seq(0, 1, 0.25), na.rm = TRUE)
		lfsd<-abs(qq[[4]]-lfmd)	
		qqfc <-logfc
		qqfc[logfc<=qq[[2]]]<-25
		qqfc[logfc>qq[[2]] & qqfc<=qq[[3]]]<-50
		qqfc[logfc>qq[[3]] & qqfc<=qq[[4]]]<-75
		qqfc[logfc>qq[[4]]]<-100
		#lfsd<-sd(logfc,na.rm = TRUE)
		dt<-ccn[[i]]
		dt<-dt[!is.na(negx[[i]])]
		dtmd<-median(dt,na.rm = TRUE)
		qq<-quantile(dt, probs = seq(0, 1, 0.25), na.rm = TRUE)
		dtsd<-abs(qq[[4]]-dtmd)
		#dtsd<-sd(dt,na.rm = TRUE)
		if(length(logfc)>0 && dtmd>=(-distrange)){
			clus<-i
			tp<-cbind(clus,logfc,lfmd,lfsd,dt,dtmd,dtsd,qqfc)
			np<-rbind(np,tp)
			tp<-cbind(clus,lfmd,lfsd,dtmd,dtsd)
			np2<-rbind(np2,tp)
		}
	}
	clust=TRUE
	# combine summaries in the same table
	dataplot<-rbind(np,pp)
	dataplot2<-rbind(np2,pp2)
	# plot data
	plot.new()
	ylim=c(-0.15,3)
	if(!upperplot)ylim=c(-3,0)
	plot.window(ylim=ylim, xlim =c(-distrange, distrange))
	axis(2,cex.axis=1.3) 
	axis(1,cex.axis=1.3)
	title(xlab="Distance from TSS (kb)",ylab="Differential gene expression (logFC)",cex.lab=1.4)
	legend("topright",c("3h","6h","12h")[k-3],bty="n",cex=2)
	# plot data points -- and legend in the 1st graph
	if(clust){
		points(dataplot[,6], dataplot[,2],pch=19,cex=0.3,col="grey50")
	} else {
		points(dataplot[,5], dataplot[,2],pch=19,cex=0.3,col="grey50")
	}
	if(k>=4){
		legend("topleft",c("75th quantile","spline fit","25th quantile"),
		pch=c("_","_","_"), pt.cex=3, 
	    col=c('orange','red','orange'),bty="n", cex=1.2)
	}
	# plot predicted lines and boundaries (fit spline to the data)
	if(clust){
		datax<-6
		df=5
	} else {
		datax<-5
		df=10
	}
	if(length(unique(dataplot[, datax]))>3 && plotfit){ # minimum number of cluster to smooth.spline function 
		xx <- seq(-distrange,distrange, len=1000)
		sp <- smooth.spline(dataplot[, datax], dataplot[,2], df= df)
		lines(predict(sp, xx), col = 2, lwd =2.5)
		#..75th qq
		temp<-dataplot[dataplot[,8]>25,]
		sp <- smooth.spline(temp[, datax], temp[,2], df=df)
		lines(predict(sp, xx), col ="orange",lwd =2.0, lty='longdash')
		#..25th qq
		temp<-dataplot[dataplot[,8]<75,]
		sp <- smooth.spline(temp[, datax], temp[,2], df=df)
		lines(predict(sp, xx), col ="orange",lwd =2.0, lty='longdash')
		if(clust){
			# plot predicted model (spline) combined in clusters
			sp <- smooth.spline(dataplot[,6], dataplot[,2], df=df)
			pred<-predict(sp, dataplot2[,4])
			uiw<-dataplot2[,3]
			uiw[uiw<0.1]<-0.1
			plotCI(x=pred$x,y=pred$y,uiw=uiw, add=TRUE, 
				barcol="grey25", 
				pch=22,col='grey10',
				cex=1.4)
			if(k>=4){
				dd<-dataplot2[,1]
				att<-factor(dd)
				cc<-colorRampPalette(c("darkred","red","orange","cyan","blue","darkblue"))(length(levels(att)))
				col<-cc[att]
				pred$y[]<-(-0.15)
				points(x=pred$x,y=pred$y,pch="|",cex=2.0,col=col)
				#text(x=pred$x,y=pred$y,labels=dd,cex=0.5,col="black")
			}
		}
	}
	}
	dev.off()
}

###############################################################################
#####################igraph objects for chip-on-chip data######################
###############################################################################
# igraph for binding site data
glist.processed<-function(ernet, erdata, clusters=c(1:5,8,16,24,32,40)){
	# get main graph
	g<-graph.adjacency(ernet, diag=FALSE, mode="undirected", weighted=TRUE)
	V(g)$nodeAlias<-as.character(erdata$resclt$Symbol)
	# set node color accordind to tss-er.binding distance
	#att<-factor(erdata$resclt$bdDist)
	#att[is.na(att)]<-levels(att)[length(levels(att))]
	#color.code<-colorRampPalette(c("darkred","red","orange","cyan","blue","darkblue"))(length(levels(att)))
	#V(g)$nodeColor<-color.code[att]
	#V(g)$nodeLineColor<-color.code[att]
	cols<-colorUniScale(erdata$resclt$bdDist,bounds=c(0,200000))
	V(g)$nodeColor<-cols
	V(g)$nodeLineColor<-cols
	# set node shape according to tss-er.binding orientation
	att<-erdata$resclt$bdDistOriented
	att[att<0]<-(-1)
	att[att>0]<-(1)
	att[is.na(att)]<-0
	V(g)$nodeShape<-c('TRIANGLE','ELLIPSE','ELLIPSE')[as.factor(att)]
	# set node size
	V(g)$nodeSize<-50
	V(g)$nodeFontSize<-45
	# set edge with
	E(g)$edgeWidth<-5
	# set edge color
	#att<-factor(E(g)$weight)
	#color.code<-colorRampPalette(c("grey","orange"))(length(levels(att)))
	#E(g)$edgeColor<-color.code[att]
	att<-E(g)$weight
	b1<-sort(att)[1]
	b2<-sort(att)[length(att)]
	att <-colorScale(att,neg.col="blue",pos.col="orange",sep.col="grey")
	E(g)$edgeColor<-att
	# set container attributes (in a dataframe)
	# ...for top clusters
	topclt <-erdata$clt[sort.list(erdata$clustDistSummary[,3])][clusters]
	gatt<-list()
	gatt$nestAliases<-paste("Cluster_",clusters,sep="")
	#gatt$nestShape<-"ROUNDED_RECTANGLE"
	gatt$nestLineWidth<-10
	gatt$nestFontSize<-80
	gatt$nestFontX<-2
	gatt$nestFontY<-(-12)	
	gatt$nestColor<-"#ffffff"
	gatt$isNest<-TRUE
	gatt$isAnchor<-FALSE
	# get subgraph
	v<-NA
	for(i in 1:length(clusters)){
		v<-c(v, topclt[[i]])
	}
	v<-v[-1]
	sg<-subgraph(g, v)
	# save final graph objects and attribs
	ER.graph<-list()
	ER.graph$graph<-sg
	ER.graph$clust<-topclt
	ER.graph$clustatt<-data.frame(gatt)
	#save(ER.graph, file=file.path("ER.graph.RData"))
	return(ER.graph)
}
# pre-igraph for DE gene series
subg.processed <-function(ernet, erdata, cluster=2, fctime=3){
	# get main graph
	g<-graph.adjacency(ernet, diag=FALSE, mode="undirected", weighted=TRUE)
	V(g)$nodeAlias <-as.character(erdata$resclt$Symbol)
	# load logFC to node size
	if(fctime==3){
		fcdatat <-erdata$resclt$"coef.t3 - t0"
	} else if(fctime==6){
		fcdatat <-erdata$resclt$"coef.t6 - t0"
	} else if(fctime==12){
		fcdatat <-erdata$resclt$"coef.t12 - t0"
	}
	V(g)$nodeSize<-fcdatat
	# load significance to node color
	if(fctime==3){
		fcdatat <-erdata$resclt$"p.value.adj.t3 - t0"
	} else if(fctime==6){
		fcdatat <-erdata$resclt$"p.value.adj.t6 - t0"
	} else if(fctime==12){
		fcdatat <-erdata$resclt$"p.value.adj.t12 - t0"
	}
	V(g)$nodeColor<-fcdatat
	V(g)$nodeLineColor<-fcdatat
	# set node font size
	V(g)$nodeFontSize<-65
	E(g)$edgeWidth<-8
	# load correlation value to edge color
	E(g)$edgeColor<-E(g)$weight
	# get selected container
	topclt <-erdata$clt[sort.list(erdata$clustDistSummary[,3])][cluster]
	# get subgraph
	sg<-subgraph(g, topclt[[1]])
	fctime <- paste(as.character(fctime),"h",sep="")
	sg<-set.nest(sg,fctime)
	# final subgraph and pre-attribs
	return(sg)
}
set.nest<-function(g,gname="3h"){
	g$isNest<-TRUE
	g$isAssign<-TRUE	
	g$nestAliases<-gname
	g$nestShape<-"ROUNDED_RECTANGLE"
	g$nestLineWidth<-10
	g$nestFontSize<-120
	g$nestFontX<-13
	g$nestFontY<-5
	g$nestColor<-"#ffffff"
	g$zoom<-16
	return(g)
}
setCommomAttribs<-function(ER.series){
	# set commom nodeSize
	att1<-V(ER.series[[1]])$nodeSize
	att2<-V(ER.series[[2]])$nodeSize
	att3<-V(ER.series[[3]])$nodeSize
	att<-c(att1, att2, att3)
	att<-as.factor(att)
	scale<-seq(40, 140, length.out=length(levels(att)))
	att<-scale[att]
	att1<-att[c(rep(T,length(att1)), rep(F,length(att2)), rep(F,length(att3)))]
	att2<-att[c(rep(F,length(att1)), rep(T,length(att2)), rep(F,length(att3)))]
	att3<-att[c(rep(F,length(att1)), rep(F,length(att2)), rep(T,length(att3)))]
	V(ER.series[[1]])$nodeSize<-att1
	V(ER.series[[2]])$nodeSize<-att2
	V(ER.series[[3]])$nodeSize<-att3
	# set commom nodeColor and nodeLineColor
	att1<-V(ER.series[[1]])$nodeColor
	att2<-V(ER.series[[2]])$nodeColor
	att3<-V(ER.series[[3]])$nodeColor
	att<-log(c(att1, att2, att3))
	#att<-factor(att)	
	#color.code<-colorRampPalette(c("darkred","red","grey"))(length(levels(att)))
	#att<-color.code[att]
	b1<-sort(att)[1]
	b2<-sort(att)[length(att)]
	att <-colorUniScale(att,bounds=c(b1,b2),bins=100,cols=c("darkred","red","grey"))
	att1<-att[c(rep(T,length(att1)), rep(F,length(att2)), rep(F,length(att3)))]
	att2<-att[c(rep(F,length(att1)), rep(T,length(att2)), rep(F,length(att3)))]
	att3<-att[c(rep(F,length(att1)), rep(F,length(att2)), rep(T,length(att3)))]
	V(ER.series[[1]])$nodeColor <-att1
	V(ER.series[[2]])$nodeColor <-att2
	V(ER.series[[3]])$nodeColor <-att3
	V(ER.series[[1]])$nodeLineColor <-att1
	V(ER.series[[2]])$nodeLineColor <-att2
	V(ER.series[[3]])$nodeLineColor <-att3
	# set commom edgeColor
	att1<-E(ER.series[[1]])$edgeColor
	att2<-E(ER.series[[2]])$edgeColor
	att3<-E(ER.series[[3]])$edgeColor
	att<-abs(c(att1, att2, att3))
	#att<-factor(att)
	#color.code<-colorRampPalette(c("grey","orange"))(length(levels(att)))
	#att<-color.code[att]
	b1<-sort(att)[1]
	b2<-sort(att)[length(att)]
	att <-colorUniScale(att,bounds=c(b1,b2),bins=10,cols=c("grey","orange"))
	att1<-att[c(rep(T,length(att1)), rep(F,length(att2)), rep(F,length(att3)))]
	att2<-att[c(rep(F,length(att1)), rep(T,length(att2)), rep(F,length(att3)))]
	att3<-att[c(rep(F,length(att1)), rep(F,length(att2)), rep(T,length(att3)))]
	E(ER.series[[1]])$edgeColor <-att1
	E(ER.series[[2]])$edgeColor <-att2
	E(ER.series[[3]])$edgeColor <-att3	
	return(ER.series)
}
colorScale<-function(x,breaks=seq(-1,1,length.out=100),neg.col="darkblue",pos.col="darkred",sep.col="white") {
	neg<-c(neg.col, sep.col)
	pos<-c(sep.col, pos.col)
	negColPl<-colorRampPalette(colors = neg)
	posColPl<-colorRampPalette(colors = pos)
	x.col<-rep("", length(x))
	names(x.col)<-names(x)
	##n means the number of color breaks in each half of the color palette
	negCols<-negColPl(length(breaks)+2)
	x.col[x<0]<-negCols[as.integer(cut(x[x<0], breaks=c(-(breaks[length(breaks)]+1), rev(-breaks), -(breaks[1]-1))))]
	posCols<-posColPl(length(breaks)+2)
	x.col[x>0]<-posCols[as.integer(cut(x[x>0], breaks=c(breaks[1]-1, breaks, breaks[length(breaks)]+1)))]
	x.col[x==0]<-negColPl(length(pos))[length(pos)]
	x.col
}
colorUniScale<-function(x,bounds=c(0,1),bins=100,cols=c("darkred","red","orange","cyan","blue","darkblue"),
na.col="grey70") {
	n=length(levels(as.factor(x)))
	bins<-min(n,bins,na.rm=TRUE)
	breaks<-seq(bounds[1], bounds[2], length.out=bins)
	breaks<-c(min(x,breaks,na.rm=TRUE)-1,breaks, max(x, breaks,na.rm=TRUE)+1)
	colPl<-colorRampPalette(colors = cols)
	cc <-colPl(bins+2)
	cc <-cc[as.integer(cut(x,breaks=breaks))]
	cc[is.na(cc)]<-colorRampPalette(colors = c(na.col,na.col))(1)
	cc
}
breaksUniScale<-function(x,bins=100) {
	bins=bins-2
	n=length(levels(as.factor(x)))
	bins<-min(n,bins,na.rm=TRUE)
	breaks<-seq(min(x), max(x), length.out=bins)
	breaks<-c(min(x,breaks,na.rm=TRUE)-1,breaks, max(x, breaks,na.rm=TRUE)+1)
	breaks
}
# bar sample!!!
barsample1<-function(bounds=c(0,1),bins=100,cols=c("darkred","red","orange","cyan","blue","darkblue")){
	x<-seq(bounds[1], bounds[2],length.out= bins)
	# call plot dev.!
	par(mar=c(5,5,5,5))
	plot.new()
	# set breaks and plot
	image(x=x,z=cbind(x),col=colorUniScale(x,bounds=bounds,bins=bins,cols=cols),axes=FALSE,xlab="", ylab="")
	axis(1,cex.axis=0.8, tick=FALSE, padj=-2)
	mtext("bar sample!",cex=0.75,adj=0)
	box()
}
# bar sample!!!
barsample2<-function(cols, labs){
	x<-seq(0, 1,length.out=length(cols))
	# call plot dev.!
	par(mar=c(5,5,5,5))
	plot.new()
	# set breaks and plot
	image(x=x,z=cbind(x),col= cols,axes=FALSE, xlab="", ylab="")
	axis(1,at=x, cex.axis=0.8, tick=FALSE, padj=-2, labels =labs)
	mtext("bar sample!",cex=0.75,adj=0)
	box()
}
nodesizeLeg<-function(sizeVec,labVec,space=1,col="#000000"){
	g <- graph.empty(n=length(sizeVec),directed=FALSE)
	V(g)$nodeSize<-sizeVec
	V(g)$nodeAlias<-as.character(labVec)
	V(g)$coordX<-cumsum(sizeVec+space)
	V(g)$coordY<-0
	V(g)$nodeColor<-col
	V(g)$nodeLineColor<-col
	g$isNest<-TRUE
	g$isAnchor<-TRUE
	g$isAssign<-TRUE
	g$nestColor<-"#FFFFFF"
	g$nestLineColor<-"#FFFFFF"	
	g$nestAliases<-""
	g
}
edgesizeLeg<-function(sizeVec,labVec,space=c(40,10),col="#808080"){
	g<-graph( c(0:(length(sizeVec)*2-1)), directed=FALSE )
	glist <- get.edgelist(g)
	source<-glist[,1]
	target<-glist[,2]
	E(g)$edgeColor<-col
	E(g)$edgeWidth<-sizeVec
	V(g)[source]$coordX<-0
	V(g)[target]$coordX<-space[1]
	V(g)[source]$coordY<-cumsum(sizeVec+space[2])
	V(g)[target]$coordY<-cumsum(sizeVec+space[2])
	V(g)$nodeColor<-col
	V(g)$nodeLineColor<-col
	V(g)$nodeSize<-0
	V(g)[source]$nodeAlias<-""	
	V(g)[target]$nodeAlias<-labVec
	g$isNest<-TRUE
	g$isAnchor<-TRUE
	g$isAssign<-TRUE
	g$nestColor<-"#FFFFFF"
	g$nestLineColor<-"#FFFFFF"
	g$nestAliases<-""
	g
}
