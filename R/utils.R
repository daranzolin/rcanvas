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
  args <- sc(args)
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
  stopifnot(!is.null(pages))
  pages <- stringr::str_split(pages, ";")[[1]]
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  pages <- purrr::map_chr(pages, stringr::str_extract, url_pattern)
  pages <- stringr::str_replace_all(pages, "[<>]", "")
  pages <- unique(pages[!is.na(pages)])
  base_url <- pages[1]
  n_pages <- 1
  should_continue = TRUE
  if(has_last(pages)) {
    n_pages <- readr::parse_number(stringr::str_extract(pages[length(pages)], "page=[0-9]{1,}"))
  } else {
    while(should_continue) {
      page_temp <- stringr::str_replace(base_url, "page=[0-9]{1,}", sprintf("page=%s", n_pages))
      links_temp <- httr::HEAD(page_temp, query = list(access_token = check_token()))
      if(has_next(httr::headers(links_temp)$link)) {
        n_pages = n_pages + 1
      } else {
        should_continue = FALSE
      }
    }
  }
  pages <- stringr::str_replace(base_url, "page=[0-9]{1,}", sprintf("page=%s", 1:n_pages))
  return(pages)
}

has_last <- function(x) {
  any(grepl("rel=\"last\"", x))
}

has_next <- function(x) {
  any(grepl("rel=\"next\"", x))
}

get_next_page <- function(resp) {
  pages <- httr::headers(resp)$link
  if (is.null(pages)) {
    return(FALSE)
  }
  next_page_exists <- stringr::str_detect(pages, "next")
  if (next_page_exists) {
    pages <- stringr::str_split(pages, ",")[[1]]
    url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))*"
    next_page <- stringr::str_subset(pages, "next")
    next_page <- stringr::str_extract(next_page, url_pattern)
    next_page <- stringr::str_sub(next_page, end = -3L)
  } else {
    return(FALSE)
  }
  next_page
}
