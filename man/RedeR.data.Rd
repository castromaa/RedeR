\name{RedeR.data}
\alias{Carroll2006}
\alias{hs.inter}
\alias{ER.limma}
\alias{ER.deg}
\docType{data}

\title{Pre-processed dataset for RedeR case study.}

\description{
Preprocessed data from a time-course gene expression and ChIP-on-chip analysis of estrogen receptor (ER) binding sites in MCF7 breast cancer cell line (Carroll et al, 2006).
}

\usage{data(Carroll2006)}

\format{
  \code{Carroll2006}{ List containing 'exp', 'tgs', 'ids', and 'bdsites' R objects.}
}

\details{

The gene expression dataset consists of 12 time-course Affymetrix U133Plus2.0 microarrays: 3 replicates at 0h, 3 replicates at 3h, 3 replicates at 6h and 3 replicates at 12h. The original dataset is available at GEO database (GSE11324). The gene ER binding site dataset consists of a Bed file of ER ChIP-on-chip experiment. The original dataset is available at http://research.dfci.harvard.edu/brownlab/datasets/index.php (ER sites from the Bed file '1E-5.bed').

\describe{
  \item{Carroll2006$exp}{data.frame with log2 gene expression dataset.}
  \item{Carroll2006$tgs}{data.frame with microarray details (e.g. targets for limma analysis).}
  \item{Carroll2006$ids}{data.frame with gene ids used in RedeR case study.}
  \item{Carroll2006$bdsites}{data.frame with ER binding sites mapped to genome build GRCh37.}
  \item{hs.inter}{Human interactome extracted from the Human Protein Reference Database (HPRD) in April 2011 <igraph object> ('name' attribute is mapped to ENTREZ ID).}
  \item{ER.limma}{data-frame containing pre-processed results from limma analysis and ER binding sites mapped to differentially expressed (DE) genes. Content: annotation (ENTREZ and Symbol), time-course fold change (logFC.t3, logFC.t6, logFC.t12), p values (p.value.t3, p.value.t6, p.value.t12), DE genes (degenes.t3, degenes.t6, degenes.t12) and distance of the closest ER bd site to the TSS -- in kb (ERbdist). }
  \item{ER.deg$dat}{Summary from ER.limma data object with extracted data for differentially expressed genes only.  }
  \item{ER.deg$exp}{Data matrix with log2 gene expression values of DE genes. }
  \item{ER.deg$ceg}{Co-expression gene network of early ER-responsive genes computed by the function cea \code{\link[RedeR:cea]{cea}}.}
}

}

\references{
  Carroll JS et al., Genome-wide analysis of estrogen receptor binding sites. Nat Genet. 38(11):1289-97, 2006.
}

\examples{
    data(Carroll2006) 
    data(hs.inter)
    data(ER.limma)
    data(ER.deg)
}

\keyword{dataset}

