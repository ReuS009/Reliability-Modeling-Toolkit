\name{rankadj}
\alias{rankadj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Rank Sort by Adjusted Rank Calculation
%%  ~~function to do ... ~~
}
\description{
Computes the rank of a data set xi based on the rank adjustment method.
}
\usage{
rankadj(xi, rc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xi}{
  A vector consisting of the failure or primary event data in a given set.  Data must consist of real and finite numbers.
  }
  \item{rc}{
  A vector consisting of the right censored data of a given set of data.  Data must consist of real and finite numbers.
  }
}

\details{
\strong{rankadj} uses the rank adjustement method in order to rank data.  The rank adjustment method is given as,
\deqn{i_(x_i) = i_(x_(i - 1)) + (n + 1 + i_(x_(i - 1)))/(n - i + 2)}
where \eqn{i} is the rank of at time \eqn{x_i} and \eqn{n} is the number of units at risk.  This method is standard for when both failed and censored units exist in the same data.
}
\value{
Returns the rank values of the unique, ascending values in input vector xi.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr. Reuel Smith (2020)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Example 1: Failure data
rankadj(xi = c(14,20,21,23,35,36,39))

## Example 2: Failure data with two failures at the same time (xi = 35)
rankadj(xi = c(14,20,21,23,35,35,36,39))

## Example 3: Failure data with two censored data
rankadj(xi = c(70,71,75,78,89), rc = c(80,84))


## The function is currently defined as
function (xi, rc)
{
    if (missing(rc)) {
        nxitot <- nxi.count(xi)
    }
    else {
        nxitot <- nxi.count(xi, rc)
    }
    faicen <- matrix.failcen(xi, rc, nx = nxitot[[1]])
    fail_and_cen <- count.failcen(faicen, nxitot[[1]])
    f <- fail_and_cen[, 2][which(fail_and_cen[, 2] > 0)]
    c <- fail_and_cen[, 3][which(fail_and_cen[, 2] > 0)]
    m <- f + c - 1
    og_i <- which(duplicated(faicen[, 1]) \%in\% FALSE)[which(fail_and_cen[,
        2] > 0)]
    iadj <- og_i + 0.5 * (m - c)
    i <- rep(1, length(iadj))
    for (i2 in 2:length(iadj)) {
        i[i2] <- i[i2 - 1] + ((nxitot[[1]] + 1) - i[i2 - 1])/(2 +
            nxitot[[1]] - og_i[i2])
    }
    return(i)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
