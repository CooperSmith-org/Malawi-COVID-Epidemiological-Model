\name{makeSummaryGraphAll}
\alias{makeSummaryGraphAll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeSummaryGraphAll(df, title, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (df, title, filename) 
{
    names(df)[names(df) == "X1"] <- "Day"
    df <- df \%>\% group_by(Day) \%>\% summarise(Susceptible = sum(S), 
        Exposed = sum(E), Infected = sum(I), Recovered = sum(R), 
        Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))
    longData <- melt(df, id = c("Day"))
    longData$variable <- factor(longData$variable, levels = c("Susceptible", 
        "Recovered", "Exposed", "Infected", "Hospitalized", "Critical", 
        "Deaths"))
    names(longData)[names(longData) == "variable"] <- "State"
    options(scipen = 10000)
    ggplot(data = subset(longData, longData$State \%in\% c("Infected", 
        "Hospitalized", "Critical", "Deaths")) \%>\% arrange(State), 
        aes(x = Day, y = value, fill = State, color = State)) + 
        geom_line(stat = "identity", position = "identity") + 
        scale_colour_manual(values = c("lightblue4", "red", "blue", 
            "indianred4")) + scale_fill_manual(values = c("lightblue4", 
        "red", "blue", "indianred4")) + xlab("Day (from t=0)") + 
        ylab("Number of people") + ggtitle(title) + scale_y_continuous(label = comma) + 
        theme_minimal()
    ggsave(filename, height = 4, width = 8)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
