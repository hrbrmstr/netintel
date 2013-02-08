\name{SANS.ASN.Detail}
\alias{SANS.ASN.Detail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SANS.ASN.Detail(asn, sans.base.url = "http://isc.sans.edu/asdetailsascii.html?as=")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{asn}{
%%     ~~Describe \code{asn} here~~
}
  \item{sans.base.url}{
%%     ~~Describe \code{sans.base.url} here~~
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
function (asn, sans.base.url = "http://isc.sans.edu/asdetailsascii.html?as=") 
{
    src = readLines(sprintf("\%s\%s", sans.base.url, asn))
    asn.df = read.table(textConnection(src[grep("^#", src, invert = TRUE)]))
    names(asn.df) = c("Source.IP", "Reports.Count", "Targets.Count", 
        "First.Seen", "Last.Seen", "Updated.Date", "Updated.Time")
    return(asn.df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
