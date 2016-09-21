check_token <- function() {
  key <- Sys.getenv("CANVAS_ACCESS_TOKEN")
  if (identical(key, "")) {
    stop("Please set env var CANVAS_ACCESS_TOKEN to your access token.",
         call. = FALSE)
  }
  key
}

canvas_query <- function(url) {
  resp <- httr::GET(url,
                    httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                    query = list(access_token = Sys.getenv("CANVAS_API_TOKEN")))
  httr::stop_for_status(resp)
  json <- httr::content(resp, "text")
  dat <- jsonlite::fromJSON(json, flatten = TRUE)
  return(dat)
}
