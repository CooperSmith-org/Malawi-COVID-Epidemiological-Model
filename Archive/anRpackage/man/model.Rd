\name{model}
\alias{model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model(times, init, parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{
%%     ~~Describe \code{times} here~~
}
  \item{init}{
%%     ~~Describe \code{init} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (times, init, parms) 
{
    with(as.list(c(init, parms)), {
        beta <- (1 - reductionList[times]) * R0 * kappa/population
        dS <- -beta * S * I
        dE <- beta * S * I - E * kappa
        dI <- E * kappa - I * kappa2
        dH <- eta * I * kappa2 - tau * H
        dC <- eta2 * tau * H - tau2 * C
        dR <- (1 - eta) * I * kappa2 + (1 - eta2) * tau * H + 
            (1 - epsilon) * tau2 * C
        dD <- epsilon * tau2 * C
        hosp <- eta * I * kappa2
        crits <- eta2 * tau * H
        list(c(dS, dE, dI, dH, dC, dR, dD, hosp, crits))
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
