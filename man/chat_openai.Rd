% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/provider-openai.R
\name{chat_openai}
\alias{chat_openai}
\alias{models_openai}
\title{Chat with an OpenAI model}
\usage{
chat_openai(
  system_prompt = NULL,
  base_url = "https://api.openai.com/v1",
  api_key = openai_key(),
  model = NULL,
  params = NULL,
  seed = lifecycle::deprecated(),
  api_args = list(),
  api_headers = character(),
  echo = c("none", "output", "all")
)

models_openai(base_url = "https://api.openai.com/v1", api_key = openai_key())
}
\arguments{
\item{system_prompt}{A system prompt to set the behavior of the assistant.}

\item{base_url}{The base URL to the endpoint; the default uses OpenAI.}

\item{api_key}{API key to use for authentication.

You generally should not supply this directly, but instead set the \code{OPENAI_API_KEY} environment variable.
The best place to set this is in \code{.Renviron},
which you can easily edit by calling \code{usethis::edit_r_environ()}.}

\item{model}{The model to use for the chat (defaults to "gpt-4.1").
We regularly update the default, so we strongly recommend explicitly specifying a model for anything other than casual use.
Use \code{models_openai()} to see all options.}

\item{params}{Common model parameters, usually created by \code{\link[=params]{params()}}.}

\item{seed}{Optional integer seed that ChatGPT uses to try and make output
more reproducible.}

\item{api_args}{Named list of arbitrary extra arguments appended to the body
of every chat API call. Combined with the body object generated by ellmer
with \code{\link[=modifyList]{modifyList()}}.}

\item{api_headers}{Named character vector of arbitrary extra headers appended
to every chat API call.}

\item{echo}{One of the following options:
\itemize{
\item \code{none}: don't emit any output (default when running in a function).
\item \code{output}: echo text and tool-calling output as it streams in (default
when running at the console).
\item \code{all}: echo all input and output.
}

Note this only affects the \code{chat()} method.}
}
\value{
A \link{Chat} object.
}
\description{
\href{https://openai.com/}{OpenAI} provides a number of chat-based models,
mostly under the \href{https://chat.openai.com/}{ChatGPT} brand.
Note that a ChatGPT Plus membership does not grant access to the API.
You will need to sign up for a developer account (and pay for it) at the
\href{https://platform.openai.com}{developer platform}.
}
\examples{
\dontshow{ellmer:::vcr_example_start("chat_openai")}
chat <- chat_openai()
chat$chat("
  What is the difference between a tibble and a data frame?
  Answer with a bulleted list
")

chat$chat("Tell me three funny jokes about statisticians")
\dontshow{ellmer:::vcr_example_end()}
}
\seealso{
Other chatbots: 
\code{\link{chat_anthropic}()},
\code{\link{chat_aws_bedrock}()},
\code{\link{chat_azure_openai}()},
\code{\link{chat_cloudflare}()},
\code{\link{chat_databricks}()},
\code{\link{chat_deepseek}()},
\code{\link{chat_github}()},
\code{\link{chat_google_gemini}()},
\code{\link{chat_groq}()},
\code{\link{chat_huggingface}()},
\code{\link{chat_mistral}()},
\code{\link{chat_ollama}()},
\code{\link{chat_openrouter}()},
\code{\link{chat_perplexity}()},
\code{\link{chat_portkey}()}
}
\concept{chatbots}
