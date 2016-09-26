#' @importFrom httr GET user_agent content stop_for_status
#' @importFrom jsonlite fromJSON

check_token <- function() {
  token <- Sys.getenv("CANVAS_API_TOKEN")
  if (identical(token, "")) {
    stop("Please set env var CANVAS_API_TOKEN to your access token.",
         call. = FALSE)
  }
  token
}

canvas_url <- function() paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/")

canvas_query <- function(url, args) {
  resp <- httr::GET(url,
                    httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                    query = args)
  httr::stop_for_status(resp)
  json <- httr::content(resp, "text")
  if (json == "[]") stop("Nothing available for this course.")
  jsonlite::fromJSON(json, flatten = TRUE)
}

iter_args_list <- function(x, label) {
  ln <- list()
  for (i in seq_along(x)) {
    ln[[i]] <- x[i]
    names(ln)[[i]] <- label
  }
  ln
}

sc <- function(x) {
  Filter(Negate(is.null), x)
}

