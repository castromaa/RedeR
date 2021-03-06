\name{cea}
\alias{cea}

\title{Co-expression analysis. }

\description{
Simple function for correlation analysis. This function computes a null distribution via permutation and returns the significant correlation values.
}

\usage{
cea(x, sig=0.01, p.adj.method="fdr", cor.method="spearman", nper=1000, plotcea=TRUE, ...)
}

\arguments{
  \item{x}{A matrix or data frame. }
  \item{sig}{Significance threshold. }
  \item{p.adj.method}{Correction method passed to "p.adjust" function. }
  \item{cor.method}{Correlation method passed to "cor" function. }
  \item{nper}{Number of permutations. }
  \item{plotcea}{Logical value, option to plot density and the null distributions. }
  \item{...}{Additional arguments passed to plotcea option. }
}

\details{       
Additional arguments: 
\describe{
  \item{n.breaks}{If plotcea=TRUE, 'n.breaks' sets the number of histogram breaks (Default=100 <integer>). } 
  \item{plotnull}{If plotcea=TRUE, 'plotnull' sets whether to plot the null distribution (Default=TRUE <logical>).}
  \item{avnull}{If plotcea=TRUE, 'avnull' takes the average null distribution (Default=TRUE <logical>). }
  \item{nullcol}{If plotcea=TRUE, 'nullcol' sets the color of the null distribution (Default="black" <character>). }
}
}

\value{
An adjacency matrix with significant correlation values.
}


\author{Mauro Castro}

\seealso{
\code{\link[stats:cor]{cor}}
\code{\link[stats:p.adjust]{p.adjust}}
}

\examples{
data(ER.deg)
#--- a gene expression matrix
exp <- ER.deg$exp
#--- a sample from gx!! 
idx <- sample(1:nrow(exp))[1:100] 
exp <- exp[idx,]

\dontrun{

res <- cea(x=exp, nper=100) #ps set 'nper' for at least 1000
}
}

\keyword{cea}
