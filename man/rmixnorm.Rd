\name{rmixnorm}
\alias{rmixnorm}
\title{Generate mixed normal random number.}
\description{
  Random generation for the mixed normal distribution with mixed probability equal to P, mean vector equal to vmu and standard deviation vector equal to vsigma.
}
\usage{
  rmixnorm(n, P, vmu, vsigma)
}
\arguments{
  \item{n}{a positive integer, the number of random numbers.}
  \item{P}{a vector, the mixed probability vector.}
  \item{vmu}{a vector, the corresponding mean vector.}
  \item{vsigma}{a vector, the corresponding sd vector.}
  }
    \value{
    Return a vector with length equal to n.
    }
    \references{
   Nothing.
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
    N <- 300
    P <- c(0.2, 0.8)
    mu <- c(-5,5)
    sigma <- c(sqrt(6), sqrt(3))
    set.seed(1)
    samples <- rmixnorm(N,P, mu, sigma)
    samples
    plot(density(samples))
  }
  \keyword{mixnorm random number}
