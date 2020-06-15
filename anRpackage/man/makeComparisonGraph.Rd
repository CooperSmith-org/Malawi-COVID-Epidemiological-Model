\name{makeComparisonGraph}
\alias{makeComparisonGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeComparisonGraph(df, diseaseState, title, fileName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{diseaseState}{
%%     ~~Describe \code{diseaseState} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{fileName}{
%%     ~~Describe \code{fileName} here~~
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
function (df, diseaseState, title, fileName) 
{
    ggplot(data = subset(df, df$State \%in\% c(diseaseState)) \%>\% 
        arrange(State), aes(x = X1, y = value, fill = Reduction, 
        color = Reduction)) + geom_line(stat = "identity", position = "identity") + 
        scale_colour_manual(values = c("lightblue4", "red", "blue", 
            "indianred4", "gray")) + scale_fill_manual(values = c("lightblue4", 
        "red", "blue", "indianred4", "gray")) + xlab("Day (from t=0)") + 
        ylab("Number of people") + ggtitle(title) + scale_y_continuous(label = comma) + 
        theme_minimal()
    ggsave(fileName, height = 4, width = 8)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
