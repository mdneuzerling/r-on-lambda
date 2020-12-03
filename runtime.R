library(httr)

setwd(Sys.getenv("LAMBDA_TASK_ROOT"))

lambda_runtime_api <- Sys.getenv(
  "AWS_LAMBDA_RUNTIME_API",
  stop("AWS_LAMBDA_RUNTIME_API environment variable undefined")
)
next_invocation_endpoint <- paste0(
  "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/next"
)
initialisation_error_endpoint <- paste0(
  "http://", lambda_runtime_api, "/2018-06-01/runtime/init/error"
)

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

# If an error has occurred before now, we'd have no way to report it

tryCatch(
  {
    # Handler is of form "file.function"
    handler <- Sys.getenv(
      "_HANDLER",
      stop("_HANDLER environment variable undefined")
    )
    handler_split <- strsplit(handler, ".", fixed = TRUE)[[1]]
    file_name <- paste0(handler_split[1], ".R")
    function_name <- handler_split[2]
    source(file_name)
  },
  error = function(e) {
    POST(
      URL = initialisation_error_endpoint,
      body = list(error_message = e),
      encode = "json"
    )
  }
)


while (True) {

  event <- GET(next_invocation_endpoint)
  event_headers <- headers(event)
  aws_request_id <- event_headers[["Lambda-Runtime-Aws-Request-Id"]]
  runtime_trace_id <- event_headers[["Lambda-Runtime-Trace-Id"]]
  Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id) # used by X-Ray SDK
  invocation_error_endpoint <- determine_invocation_error_endpoint(aws_request_id)
  response_endpoint <- determine_invocation_response_endpoint(aws_request_id)

  tryCatch(
    {
      event_content <- content <- jsonlite::fromJSON(
        httr::content(event, "text", encoding = "UTF-8")
      )

      result <- do.call(function_name, event_content)
      POST(
        response_endpoint,
        body = result,
        encode = "json"
      )
    },
    error = function(e) {
      POST(
        URL = invocation_error_endpoint,
        body = list(error_message = e),
        encode = "json"
      )
    }
  )
}
