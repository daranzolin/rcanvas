#' @importFrom httr GET user_agent stop_for_status headers
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_chr
#' @importFrom stringr str_split str_extract

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
  return(resp)
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

get_pages <- function(x) {
  pages <- httr::headers(x)$link
  if (!is.null(pages)) {
    pages <- stringr::str_split(pages, ";")[[1]]
    url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    pages <- purrr::map_chr(pages, stringr::str_extract, url_pattern)
    pages <- unique(pages[!is.na(pages)])
    return(pages)
  }
}
