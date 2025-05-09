% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools-def.R
\name{tool_reject}
\alias{tool_reject}
\title{Reject a tool call}
\usage{
tool_reject(reason = "The user has chosen to disallow the tool call.")
}
\arguments{
\item{reason}{A character string describing the reason for rejecting the
tool call.}
}
\value{
Throws an error of class \code{ellmer_tool_reject} with the provided
reason.
}
\description{
Throws an error to reject a tool call. \code{tool_reject()} can be used within the
tool function to indicate that the tool call should not be processed.
}
\seealso{
Other tool calling helpers: 
\code{\link{tool}()},
\code{\link{tool_annotations}()}
}
\concept{tool calling helpers}
