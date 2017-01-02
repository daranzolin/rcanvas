#' @title Function to set Canvas API token
#'
#' @description Given a Canvas token string, this function adds it to R's
#' environment variables so it can be found by rcanvas.
#'
#' @param token your API token
#'
#' @return nothing
#' @export
#'
#' @examples
#' set_canvas_token("abc123")
set_canvas_token <- function(token) {
  Sys.setenv(CANVAS_API_TOKEN = token)
}

#' @title Function to set Canvas domain url
#'
#' @description Given a Canvas domain url, this function adds it to R's
#' environment variables so it can be found by rcanvas.
#'
#' @param domain Canvas domain
#'
#' @return nothing
#' @export
#'
#' @examples
#' set_canvas_domain("https://canvas.upenn.edu")
set_canvas_domain <- function(domain) {
  Sys.setenv(CANVAS_DOMAIN = domain)
}

check_token <- function() {
  token <- Sys.getenv("CANVAS_API_TOKEN")
  if (identical(token, "")) {
    stop("Please set env var CANVAS_API_TOKEN to your access token.",
         call. = FALSE)
  }
  token
}

canvas_url <- function() paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/")

canvas_query <- function(url, args, type = "GET") {
  fun <- getFromNamespace(type, "httr")
  args <- sc(args)
  resp <- fun(url,
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
<<<<<<< HEAD

get_pages <- function(x) {
  pages <- httr::headers(x)$link
  stopifnot(!is.null(pages))
  pages <- stringr::str_split(pages, ";")[[1]]
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  pages <- purrr::map_chr(pages, stringr::str_extract, url_pattern)
  pages <- unique(pages[!is.na(pages)])
  base_url <- pages[1]
  n_pages <- readr::parse_number(stringr::str_extract(pages[length(pages)], "page=[0-9]{1,}"))
  pages <- stringr::str_replace(base_url, "page=[0-9]{1,}", sprintf("page=%s", 1:n_pages))
  return(pages)
}
=======
>>>>>>> a8e03d04f6dd8ec2ac65791646a219af9100c1a3
