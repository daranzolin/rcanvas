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

canvas_query <- function(url) {
  resp <- httr::GET(url,
                    httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                    query = list(access_token = Sys.getenv("CANVAS_API_TOKEN"),
                                 per_page = 500))
  httr::stop_for_status(resp)
  json <- httr::content(resp, "text")
  dat <- jsonlite::fromJSON(json, flatten = TRUE)
  return(dat)
}

