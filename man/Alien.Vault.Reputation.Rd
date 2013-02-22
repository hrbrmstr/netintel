\name{Alien.Vault.Reputation}
\alias{Alien.Vault.Reputation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alien.Vault.Reputation
}
\description{
Retrieves Alien Vault's IP reputation database
}
\usage{
Alien.Vault.Reputation(refresh = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{refresh}{
should the function refresh the database
  }
}
\details{
AlienValut refreshes every hour, but the onus is on the caller to force a refresh. First-time call will setup a cache directory & file in the user's home directory, download & generate the data frame then write the data frame out as an R object. Future calls will just re-read this data frame unless refresh == TRUE
}
\value{
Data frame with IP & Reputation factor
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
Please be kind to the Alien Valut servers & only refresh if you really need to.

Background on Alien Valut's IP rep db: http://labs.alienvault.com/labs/index.php/projects/open-source-ip-reputation-portal/download-ip-reputation-database/

Even more info: http://www.slideshare.net/alienvault/building-an-ip-reputation-engine-tracking-the-miscreants
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
