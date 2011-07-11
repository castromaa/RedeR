\name{RedeR.data}
\alias{RedeR.data}
\alias{Carroll2006}
\alias{TSS.map}
\alias{ER.limma}
\alias{ER.pvclust}
\alias{ER.endata}
\alias{ER.glist}
\alias{ER.glistall}
\alias{ER.gseries}
\alias{hs.inter}
\alias{bionet4RedeR}
\alias{gsca4RedeR}
\alias{ER.gsco}
\alias{ER.ppi}
\docType{data}

\title{Pre-processed dataset for RedeR case study.}

\description{
Preprocessed data from a time-course gene expression and ChIP-on-chip analysis of estrogen receptor (ER) binding sites in MCF7 breast cancer cell line (Carroll et al, 2006).
}

\usage{data(Carroll2006)}

\format{
  \code{Carroll2006}{ Data list containing 'exp', 'tgs', 'ids', and 'bdsites' R objects.}
}

\details{

The gene expression dataset consists of 12 time-course Affymetrix U133Plus2.0 microarrays: 3 replicates at 0h, 3 replicates at 3h, 3 replicates at 6h and 3 replicates at 12h. The original dataset is available at GEO database (GSE11324). The gene ER binding site dataset consists of a Bed file of ER ChIP-on-chip experiment. The original dataset is available at http://research.dfci.harvard.edu/brownlab/datasets/index.php (ER sites from the Bed file '1E-5.bed').

\describe{
  \item{Carroll2006$exp}{data.frame with gene expression dataset.}
  \item{Carroll2006$tgs}{data.frame with microarray details (e.g. targets for limma analysis).}
  \item{Carroll2006$ids}{data.frame with gene ids used in RedeR case study.}
  \item{Carroll2006$bdsites}{data.frame with ER binding sites mapped to genome build GHCh37.}
  \item{TSS.map}{ dataset with transcription starting sites mapped to genome build GHCh37.}
  \item{ER.limma}{ dataset containing pre-processed results from 'limma' package.}
  \item{ER.pvclust}{ dataset containing pre-processed results from 'pvclust' package.}
  \item{ER.endata}{ dataset containing final results to rum RedeR case study.}
  \item{ER.glist}{ pre-processed subgraph list, clusters and attributes to rum RedeR case study.}
  \item{ER.glistall}{ pre-processed subgraph list, clusters and attributes to rum RedeR case study.}
  \item{ER.gseries}{ pre-processed subgraph series to rum RedeR case study.}
  \item{hs.inter}{Human interactome extracted from the Human Protein Reference Database (HPRD) in April 2011 <igraph object> ('name' attribute is mapped to Entrez ID).}
  \item{bionet4RedeR}{ pre-processed subgraphs and attributes to rum RedeR case study.}
  \item{gsca4RedeR}{ pre-processed subgraphs and attributes to rum RedeR case study.}
  \item{ER.gsco}{ pre-processed subgraphs from gene-set collection analysis with Hypergeometric and GSEA results to rum RedeR case study.}
  \item{ER.ppi}{ pre-processed subgraphs from BioNet package to rum RedeR case study.}
}

}

\references{
  Carroll JS et al., Genome-wide analysis of estrogen receptor binding sites. Nat Genet. 38(11):1289-97, 2006.
}

\examples{
    data(Carroll2006) 
    data(ER.graph)
}

\keyword{datasets}

