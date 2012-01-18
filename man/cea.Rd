\name{cea}
\alias{cea}

\title{Co-expression analysis. }

\description{
Simple function for correlation analysis. This function computes a null distribution via permutation and returns the significant correlation values.
}

\usage{
cea(x, sig=0.01, p.adj.method="fdr", cor.method="spearman", nper=100, plotcea=TRUE, ...)
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
  \item{ptype}{If plotcea=TRUE, ptype provides 5 pre-defined plotting options: 1, 2, 3, 4, 5 (Default=1) <integer>. } 
  \item{bk}{If plotcea=TRUE, bk removes non-significant values from the density distribution (0 <= bk <=1 ) <numerics>. } 
  \item{n.breaks}{If plotcea=TRUE, n.breaks sets the number of histogram breaks <integer>. } 
  \item{plotnull}{Logical value, whether to plot the null distribution (Default=TRUE).}
  \item{avnull}{If plotnull=TRUE, avnull takes the average null distribution (Default=TRUE). }
  \item{nullcol}{If plotnull=TRUE, nullcol sets the color of the null distribution (Default="black"). }
}
}

\value{
Matrix with significant correlation values.
}


\author{Mauro Castro}

\seealso{
\code{\link[stats:cor]{cor}}
\code{\link[stats:p.adjust]{p.adjust}}
}


\examples{

data(ER.deg)
exp <- ER.deg$exp
idx <- sample(1:nrow(exp))[1:100]
exp <- exp[idx,]

res <- cea(x=exp, nper=100) #ps set 'nper' for at least 1000

}

\keyword{cea}
