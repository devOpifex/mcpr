% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jsonrpc.R
\name{parse_request}
\alias{parse_request}
\title{Parse and process a JSON-RPC 2.0 request}
\usage{
parse_request(json_text, mcp)
}
\arguments{
\item{json_text}{JSON request text}

\item{mcp}{MCP server object}
}
\value{
JSON response text or NULL for notifications
}
\description{
Parse and process a JSON-RPC 2.0 request
}
\keyword{internal}
