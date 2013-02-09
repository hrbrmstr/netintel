\name{BulkOrigin}
\alias{BulkOrigin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BulkOrigin
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BulkOrigin(ip.list, host = "v4.whois.cymru.com", port = 43)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ip.list}{
%%     ~~Describe \code{ip.list} here~~
}
  \item{host}{
%%     ~~Describe \code{host} here~~
}
  \item{port}{
%%     ~~Describe \code{port} here~~
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
function (ip.list, host = "v4.whois.cymru.com", port = 43) 
{
    cmd = "begin\nverbose\n"
    ips = paste(unlist(ip.list), collapse = "\n")
    cmd = sprintf("\%s\%s\nend\n", cmd, ips)
    con = socketConnection(host = host, port = port, blocking = TRUE, 
        open = "r+")
    cat(cmd, file = con)
    response = readLines(con)
    close(con)
    response = response[2:length(response)]
    response = lapply(response, function(n) {
        sapply(strsplit(n, "|", fixed = TRUE), trim)
    })
    response = adply(response, c(1))
    response = response[, 2:length(response)]
    names(response) = c("AS", "IP", "BGP.Prefix", "CC", "Registry", 
        "Allocated", "AS.Name")
    return(response)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
