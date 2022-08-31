\name{plotposit.select}
\alias{plotposit.select}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting Position Selector
}
\description{
Computes nonparametric distribution analysis based on plotting position and data.
}
\usage{
plotposit.select(xi, rc, pp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xi}{
  A vector consisting of the failure or primary event data in a given set.  Data must consist of real and finite numbers.
  }
  \item{rc}{
  A vector consisting of the right censored data of a given set of data.  Data must consist of real and finite numbers.
  }
  \item{pp}{
Name of the plotting position to select for computation.  Choose from the following where \eqn{i =} rank of data point \eqn{x_i} and \eqn{n =} total number of data:
  \enumerate{
    \item \strong{Blom or Kimball} (Enter as "Blom"):
    \deqn{F_i(x_i) = (i - 0.375)/(n + 0.25)}

    \item \strong{Mean or Expected} (Enter as "Mean"):
    \deqn{F_i(x_i) = i/(n + 1)}

    \item \strong{Median (or "Bernard's Approximation")} (Enter as "Median"):
    \deqn{F_i(x_i) = (i - 0.3)/(n + 0.4)}

    \item \strong{Midpoint (or "Hazen")} (Enter as "Midpoint"):
    \deqn{F_i(x_i) = (i - 0.5)/n}

    \item \strong{Jenkinson's (or "Beard")} (Enter as "Beard"):
    \deqn{F_i(x_i) = (i - 0.31)/(n + 0.38)}

    \item \strong{Benard and Bos-Levenbach} (Enter as "BernardBosLevenbach"):
    \deqn{F_i(x_i) = (i - 0.3)/(n + 0.2)}

    \item \strong{Tukey} (Enter as "Tukey"):
    \deqn{F_i(x_i) = (i - (1/3))/(n + (1/3))}

    \item \strong{Grigorten} (Enter as "Grigorten"):
    \deqn{F_i(x_i) = (i - 0.44/(n + 0.12)}

    \item \strong{Nelson-Aalen} (Enter as "NelsonAalen"):
    \deqn{F_i(x_i) = 1 - exp[-\Sigma_(x_i <= x) d_i/(n_(i - 1) - d_(i - 1) - c_(i - 1))]} (\emph{NOTE}: \eqn{d} is the number of failed units and \eqn{c} is the number of censored.)

    \item \strong{Kaplan-Meier} (Enter as "KaplanMeier"):
    \deqn{F_i(x_i) = 1 - \Pi_(x_j <= x) (1 - d/j)} (\emph{NOTE}: \eqn{d} is the number of failed units and \eqn{j} is the reverse rank)
  }
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a matrix of nonparameter estimates for each failure data xi: Column 2 is the \emph{failure probability}, Column 3 is the \emph{reliability}, Column 4 is the \emph{hazard rate}, and Column 5 is the \emph{cumulative hazard}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr. Reuel Smith (2020-2022)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Example 1
plotposit.select(xi = c(70,71,75,78,89), rc = c(80,80,84), pp="Blom")
plotposit.select(c(70,71,75,78,89), c(80,80,84), "Blom")

## Example 2
plotposit.select(xi = c(70,71,75,78,89), rc = c(80,80,84), pp="KaplanMeier")
plotposit.select(c(70,71,75,78,89), c(80,80,84), "KaplanMeier")

## Example 3
plotposit.select(xi = c(70,71,75,78,89), pp="Tukey")
plotposit.select(c(70,71,75,78,89), NULL, "Tukey")

## The function is currently defined as
function (xi, rc, pp)
{
    if (pp == "Blom") {
        if (missing(rc)) {
            matFR <- plotposit.blom(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.blom(rankcalc(xi, rc), xi, rc)
        }
    }
    if (pp == "Mean") {
        if (missing(rc)) {
            matFR <- plotposit.mean(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.mean(rankcalc(xi, rc), xi, rc)
        }
    }
    if (pp == "Median") {
        if (missing(rc)) {
            matFR <- plotposit.median(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.median(rankcalc(xi, rc), xi, rc)
        }
    }
    if (pp == "Midpoint") {
        if (missing(rc)) {
            matFR <- plotposit.midpt(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.midpt(rankcalc(xi, rc), xi, rc)
        }
    }
    if (pp == "Beard") {
        if (missing(rc)) {
            matFR <- plotposit.beard(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.beard(rankcalc(xi, rc), xi, rc)
        }
    }
    if (pp == "BernardBosLevenbach") {
        if (missing(rc)) {
            matFR <- plotposit.bernbos(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.bernbos(rankcalc(xi, rc), xi,
                rc)
        }
    }
    if (pp == "Tukey") {
        if (missing(rc)) {
            matFR <- plotposit.tukey(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.tukey(rankcalc(xi, rc), xi, rc)
        }
    }
    if (pp == "Grigorten") {
        if (missing(rc)) {
            matFR <- plotposit.grigorten(rankcalc(xi), xi)
        }
        else {
            matFR <- plotposit.grigorten(rankcalc(xi, rc), xi,
                rc)
        }
    }
    if (pp == "NelsonAalen") {
        if (missing(rc)) {
            matFR <- plotposit.nelsonaalen(xi)
        }
        else {
            matFR <- plotposit.nelsonaalen(xi, rc)
        }
    }
    if (pp == "KaplanMeier") {
        if (missing(rc)) {
            matFR <- plotposit.kaplanmeier(xi)
        }
        else {
            matFR <- plotposit.kaplanmeier(xi, rc)
        }
    }
    return(matFR)
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
