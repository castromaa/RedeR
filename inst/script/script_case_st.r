####################################################################################
## RedeR, dynamic networks in R
## Copyright (C) 2012  CRI-CRUK
##
## Source code to reproduce the preprocessed data included in RedeR software.
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## You should have received a copy of the GNU General Public License
## along with this program. If not, see <http://www.gnu.org/licenses/>.
##
## Mauro Castro - mauro.castro@cancer.org.uk
###################################################################################

#####################################################
# Chunk 1: differential gene expression analysis
#####################################################
library(RedeR)
library(limma)
library(biomaRt)
library(tools)

#---load Carroll2006 datase
data(Carroll2006)

#---build design matrix
t <- factor(Carroll2006$tgs$Time)
design <- model.matrix(~0+t)

#---fit lm model
fit <- lmFit(Carroll2006$exp,design)
fit$genes$RefSeq <- Carroll2006$ids$RefSeqID
fit$genes$Symbol <- Carroll2006$ids$GeneSymbol

#---set contrasts
contrasts <- makeContrasts(t3-t0, t6-t0, t12-t0, levels=design) 
                                                                                                              
#---eBayes correction and decision
ct.fit <- eBayes(contrasts.fit(fit, contrasts))
res.fit<-decideTests(ct.fit,method="global", adjust.method="BH", p.value=0.0001)
					
#---combine results in a data.frame 
ER.limma <- data.frame(
			ENTREZ  = ct.fit$genes$ID,
			Symbol  = ct.fit$genes$Symbol,
			logFC   = ct.fit$coef, 
			p.value = ct.fit$p.value, 
			degenes = unclass(res.fit),
			stringsAsFactors = FALSE)


#####################################################
# Chunk 2 ER binding sites mapping
#####################################################

#---get ER binding sites from  Carroll2006 datase
bdsites <- Carroll2006$bdsites

#---get transcript start sites from biomaRt
# p.s. make sure to access GRCh37 build ( e.g. check " listDatasets(useMart("ensembl")) " )
mart <- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))
tssmap <- getBM(attributes=c("entrezgene","chromosome_name","transcript_start"), 
				filters="entrezgene", values=ER.limma$ENTREZ, mart=mart)
tssmap <- data.frame(tssmap, chrom=paste("chr", tssmap$chromosome_name,sep=""), 
					stringsAsFactors=FALSE)

#---find the closest ER binding site for each TSS
delta_bd <- array(NA,dim=nrow(tssmap))
names(delta_bd) <- tssmap$entrezgene
for(i in 1:nrow(tssmap)){
	chrom    <- as.character(tssmap$chrom[[i]])
	txstart  <- tssmap$transcript_start[i]
	absdelta <- abs(txstart-bdsites$bdMean)
	absdelta[bdsites$bdChrom!=chrom] <- NA
	delta_bd[i] <- absdelta[sort.list(absdelta)[1]]
}

#---combine dataframes
delta_bd <- data.frame(ENTREZ=names(delta_bd), bdDist=round(delta_bd/1000,2), 
						stringsAsFactors=FALSE)	
delta_bd <- aggregate(bdDist ~ ENTREZ, data=delta_bd, min)
rownames(delta_bd) <- delta_bd$ENTREZ
ER.limma <- data.frame(ER.limma, ERbdist=delta_bd[ER.limma$ENTREZ,"bdDist"], 
						stringsAsFactors=FALSE)
	
#---save the same RData object included in the package!
save(ER.limma, file='ER.limma.RData')
resaveRdaFiles('./ER.limma.RData')				
				
							
#####################################################
# Chunk 3: get summary from limma results
#####################################################

#---extract results for DE genes
idx <- rowSums(ER.limma[,c(9,10,11)]!=0)
dat <- ER.limma[idx>0,]
			
#---extract gene expression matrix for DE genes
exp <- Carroll2006$exp[is.element(rownames(Carroll2006$exp), dat$ENTREZ),]


#####################################################
# Chunk 4: co-expression gene network analysis
#####################################################

#---compute a co-expression gene network sample (using early ER-responsive genes)
degt3 <- dat[dat$degenes.t3!=0, "ENTREZ"]
res <- cea(exp[degt3,], sig=1e-4, nper=1000, plotcea=FALSE, ptype=4)
ceg <- graph.adjacency(res, diag=FALSE, mode="undirected", weighted=TRUE)
ceg <- subg(g=ceg, dat=dat, transdat=FALSE)


#####################################################
# Chunk 5: save summary data for RedeR
#####################################################

#---save the same RData object included in the package!
ER.deg <- list(dat=dat, exp=exp, ceg=ceg)
save(ER.deg, file="ER.deg.RData")
resaveRdaFiles('./ER.deg.RData')	




