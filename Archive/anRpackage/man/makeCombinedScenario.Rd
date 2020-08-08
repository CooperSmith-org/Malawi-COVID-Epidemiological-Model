\name{makeCombinedScenario}
\alias{makeCombinedScenario}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeCombinedScenario(base_df, scenario_df, TAList)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base_df}{
%%     ~~Describe \code{base_df} here~~
}
  \item{scenario_df}{
%%     ~~Describe \code{scenario_df} here~~
}
  \item{TAList}{
%%     ~~Describe \code{TAList} here~~
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
function (base_df, scenario_df, TAList) 
{
    scenario_df$scenario <- "reduction"
    base_df$scenario <- "baseline"
    use1 <- subset(scenario_df, scenario_df$TA \%in\% TAList)
    use2 <- subset(base_df, base_df$TA \%in\% setdiff(unique(base_df$TA), 
        TAList))
    return(bind_rows(use1, use2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
