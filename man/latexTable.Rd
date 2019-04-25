\name{latexTable}
\alias{latexTable}
\title{Generate a Latex Table Form Based on a Numerical Matrix}
\description{
  Generate a latex table form based on a numerical matrix
}
\usage{
  latexTable(res, paraname = 'beta',align=NULL, digits=NULL, nhline=3,
  lsub=as.character(1:(nrow(res)/nhline)), side=1)
}
\arguments{
  \item{res}{a numeric matrix with rownames and colnames, which will be regarded as rownames and colnames of latex table.}
  \item{parname}{a character string, specify the name of parameter in the estimate performance evaluation table, such as 'beta', 'gamma', 'alpha', etc. However, if it is NULL, then don't have the first column for parname.}
  \item{align}{a character vector, specify the way of alignment.}
  \item{digits}{an integer or a numeric vector, specify the decimal number rounded.}
  \item{nhline}{an integer, how many rows each parameter's imformation occupies.}
  \item{lsub}{a character vector, the subscript of parameters}
  \item{side}{an integer, how many parameters' imformation arrange in one line}
}
\details{
  This function is used to establish latex table. The colnames of res are usually  competitive methods, such 'model1', 'model2', 'lm', 'glm', etc. And the rownames of res are usually 'Bias', 'SD', 'RMSE', 'CP' and 'SE'.
}
\value{
  No value returned. Only print a format of latex table for a matrix with rownames and colnames.
}
\references{
  Lamport, L. (1994). LATEX: a document preparation system: user's guide and reference manual. Addison-wesley.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  xtable
}
\examples{
# example one
  library(MASS)
  res <- mvrnorm(12, rep(0,5), cov.mat(rep(3,5), 0.5))
  row.names(res) <- rep(c('Bias','SD', 'RMSE'), nrow(res)/3)
  colnames(res) <- paste0('method', 1:ncol(res))
  latexTable(res)
  latexTable(res, paraname = NULL)
  latexTable(res, digits=2, side=2)
# exmple two
  library(MASS)
  res <- mvrnorm(16, rep(0,5), cov.mat(rep(3,5), 0.5))
  row.names(res) <- rep(c('Bias','SD', 'RMSE', 'CP'), nrow(res)/4)
  colnames(res) <- paste0('method', 1:ncol(res))
  latexTable(res=res, paraname = 'alpha', align = rep('r', ncol(res)+2),nhline = 4,
           digits = rep(3,ncol(res)), lsub = c(paste0('2', 1:(nrow(res)/4))))
# example one
  library(MASS)
  res <- mvrnorm(27, rep(0,2), cov.mat(rep(3,2), 0.5))
  row.names(res) <- rep(c('Bias','SD', 'RMSE'), nrow(res)/3)
  colnames(res) <- paste0('method', 1:ncol(res))
  latexTable(res, digits=3, side=3, nhline=3)
}
\keyword{Latex Table}
