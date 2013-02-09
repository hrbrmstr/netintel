\name{Alien.Vault.Reputation}
\alias{Alien.Vault.Reputation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alien.Vault.Reputation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Alien.Vault.Reputation(refresh = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{refresh}{
%%     ~~Describe \code{refresh} here~~
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
function (refresh = FALSE) 
{
    alien.valut.reputation.gzurl = "http://reputation.alienvault.com/reputation.snort.gz"
    av.dir = file.path(path.expand("~"), ".ipcache")
    av.file = file.path(av.dir, "alienvaultrepdf")
    av.gzfile = file.path(av.dir, "reputation.snort.gz")
    dir.create(av.dir, showWarnings = FALSE)
    if (refresh || file.access(av.file)) {
        download.file(alien.valut.reputation.gzurl, av.gzfile)
        src = gzfile(av.gzfile)
        raw = readLines(src)
        close(src)
        av.matrix = t(sapply(strsplit(raw[9:length(raw)], " +# +"), 
            c))
        av.df = data.frame(IP = pre[, 1], Reputation = factor(pre[, 
            2]))
        dput(av.df, av.file)
    }
    else {
        av.df = dget(av.file)
    }
    return(av.df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
