\name{var.amp.loadingdamage.model}
\alias{var.amp.loadingdamage.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable Amplitude Loading Modeling with Damage
}
\description{
Calculates various values of a variable amplitude loading scenario with respect to the Palmgren-Miner linear damage rule.
}
\usage{
var.amp.loadingdamage.model(dat,stressunits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
      A list of loading conditions that is used to compute the missing element(s) of the scenario.  Primarily two input pairs are applied for computation:
      \itemize{
          \item \strong{(Pair 1)} the \emph{number of operation cycles} by stress block (\eqn{n_i} entered as \code{ni}) and the \emph{number of cycles to failure}  by stress block (\eqn{N_fi} entered as \code{Nf})
          \item \strong{(Pair 2)} the \emph{percentage of damage} (between \eqn{0} and \eqn{1}) by stress block (\eqn{D_i} entered as \code{Df}) and the \emph{number of cycles to failure}  by stress block
      }
      In lieu of \eqn{N_fi}, a list of stress ranges \code{sranges = list(c(Smin1,Smax1),...)} may be used in each pair instead as well as,
      \itemize{
          \item \strong{b}{
                Fatigue strength exponent \eqn{b}.
          }
          \item \strong{sig_f}{
                Fatigue strength coefficient \eqn{\sigma_f'} (in MPa or ksi).  This needs to be in the same units as that of \code{E}.
          }
          \item \strong{corr_rel} - The correlation relationship between mean stress \eqn{Sm} and stress amplitude \eqn{Sa} and equivalent stress amplitude \eqn{Sar} for non-zero mean stress (or non-fully reversed loading conditions) (see \code{\link{SN.diagram}} for descriptions).
      }
  }
  \item{stressunits}{
      Defines the units for the stress amplitude: \code{1} for \emph{'MPa'} and \code{2} for \emph{'ksi'}.  \emph{'MPa'} will be used as default if this heading is left empty.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Input of \emph{Pair 1} produces:
    \item{damagebyblock}{
        Percent damage by stress block \eqn{D_fi}
    }
    \item{repetitionstofailure}{
        Number of repetitions to failure \eqn{B_f}
    }
Input of \emph{Pair 2} produces:
    \item{opercyclesbyblock}{
        Operation cycles by stress block \eqn{n_i}
    }
    \item{totalopercycles}{
        Total number of operation cycles \eqn{n = \Sigma n_i}
    }
If stress ranges are used instead of \eqn{N_fi}, ouput will also include the cycles to failure by stress block \eqn{N_fi} and the reversals to failure by stress block \eqn{2N_fi}.
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Reuel Smith, (2022)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Example 1: Example 2.5 from the textbook.
var.amp.loadingdamage.model(list(Df = c(.9,.1), Nf = c(2e8,3e7)))

# Example 2: Example 2.6 from the textbook.
var.amp.loadingdamage.model(list(ni = c(200,1000,100), sranges = list(c(-80,80),c(0,100),c(-100,0)), sig_f = 220, b = -0.1, corr_rel = "Morrow"))

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
