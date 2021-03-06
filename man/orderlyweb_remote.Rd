% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orderly.R
\name{orderlyweb_remote}
\alias{orderlyweb_remote}
\title{Create orderly remote}
\usage{
orderlyweb_remote(host, port, token, https = TRUE, prefix = NULL,
  name = NULL)
}
\arguments{
\item{host}{Fully qualified hostname for the OrderlyWeb instance}

\item{port}{Port to use}

\item{token}{Your application token for authentication.  The
appropriate value here will depend on the authentication support
that is built into the OrderlyWeb server that you are
communicating with.  Provide the token directly (as a string) or
provide a callback function that takes no arguments and returns
token.}

\item{https}{Optional logical, indicating if this is an https
connection - this should be \code{TRUE} in all production
settings or credentials will be sent in the clear!}

\item{prefix}{A prefix, if your OrderlyWeb server is mounted at a
path within some larger website.}

\item{name}{A friendly name for the server (e.g, "production" or
"testing") which may be printed when using the remote, or when
authenticating.  If not provided then a name will be constructed
from \code{host}, \code{port} and (if provided)
\code{prefix}.}
}
\description{
Implements an orderly "remote" using OrderlyWeb as a backend.  Use
this within an \code{orderly_config.yml} configuration.
}
\examples{
remote <- orderlyweb::orderlyweb_remote("example.com", 443, "mytoken")
remote
}
