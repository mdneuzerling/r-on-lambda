library(httr)

setwd(Sys.getenv("LAMBDA_TASK_ROOT"))
#
# Retrieve environnment variables and define Lambda API endpoints

lambda_runtime_api <- Sys.getenv("AWS_LAMBDA_RUNTIME_API")
if (lambda_runtime_api == "") {
  stop("AWS_LAMBDA_RUNTIME_API environment variable undefined")
}
next_invocation_endpoint <- paste0(
  "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/next"
)
initialisation_error_endpoint <- paste0(
  "http://", lambda_runtime_api, "/2018-06-01/runtime/init/error"
)

# These next two endpoints depend on the request ID which changes with every
# event. We define them as functions of this variable.
determine_invocation_response_endpoint <- function(aws_request_id) {
  paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/",
    aws_request_id, "/response"
  )
}
determine_invocation_error_endpoint <- function(aws_request_id) {
  paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/",
    aws_request_id, "/error"
  )
}

# If an error has occurred before now, we'd have no way to report it, since we'd
# need that `initialisation_error_endpoint`. The final part of the
# initialisation is to retrieve the handler, which is of the form
# "file.function". From this we can source the appropriate file by appending a
# ".R" extension.

tryCatch(
  {
    handler <- Sys.getenv("_HANDLER",)
    if (handler == "") stop("_HANDLER environment variable undefined")
    handler_split <- strsplit(handler, ".", fixed = TRUE)[[1]]
    file_name <- paste0(handler_split[1], ".R")
    function_name <- handler_split[2]
    source(file_name)
  },
  error = function(e) {
    POST(
      url = initialisation_error_endpoint,
      body = list(error_message = e),
      encode = "json"
    )
  }
)

# This infinite loop does the actual function work. It continuously checks for
# events.
while (TRUE) {

  event <- GET(url = next_invocation_endpoint)
  event_headers <- headers(event)

  # I've encountered a few issues with headers and mismatched cases. I suspect
  # that httr is converting header names to lower-case. Since HTTP headers are
  # _supposedly_ case-insensitive, I'll convert the names to lower-case myself
  # as a precaution.
  names(event_headers) <- tolower(names(event_headers))
  aws_request_id <- event_headers[["lambda-runtime-aws-request-id"]]
  if (is.null(aws_request_id)) {
    stop("Could not find lambda-runtime-aws-request-id header in event")
  }

  # The following is used by "X-Ray SDK". I'm suspicious that setting a trace ID
  # as an environment variable is suspicious --- I'm surprised we don't need to
  # forward it on as a header in the response.
  runtime_trace_id <- event_headers[["lambda-runtime-trace-id"]]
  if (!is.null(runtime_trace_id)) {
    Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id)
  }

  tryCatch(
    {
      # This is a likely source of errors --- converting the body of the
      # event/request and interpreting it as an R list.
      event_content <- jsonlite::fromJSON(
        httr::content(event, "text", encoding = "UTF-8")
      )

      result <- do.call(function_name, event_content)
      response_endpoint <- determine_invocation_response_endpoint(
        aws_request_id
      )
      POST(
        url = response_endpoint,
        body = result,
        encode = "json"
      )
    },
    error = function(e) {
      invocation_error_endpoint <- determine_invocation_error_endpoint(
        aws_request_id
      )
      POST(
        url = invocation_error_endpoint,
        body = list(error_message = e),
        encode = "json"
      )
    }
  )
}
