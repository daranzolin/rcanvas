process_response <- function(url, args) {
  resp <- canvas_query(url, args, "HEAD")
  get_pages(resp) %>%
    purrr::map(canvas_query, args) %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
    dplyr::bind_rows()
}

get_pages <- function(x) {
  pages <- httr::headers(x)$link
  stopifnot(!is.null(pages))
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  pages <- stringr::str_split(pages, ";")[[1]]
  pages <- pages %>%
    purrr::map_chr(stringr::str_extract, url_pattern) %>%
    stringr::str_replace_all("[<>]", "")

  pages <- unique(pages[!is.na(pages)])
  base_url <- pages[1]
  n_pages <- 1
  should_continue <- TRUE
  if (has_rel(httr::headers(x)$link, "last")) {
    n_pages <- readr::parse_number(stringr::str_extract(pages[length(pages)], "page=[0-9]{1,}"))
  } else {
    while (should_continue) {
      page_temp <- stringr::str_replace(base_url, "page=[0-9]{1,}", sprintf("page=%s", n_pages))
      links_temp <- canvas_query(page_temp,
                                 args = list(access_token = check_token()),
                                 type = "HEAD")
      if (has_rel(httr::headers(links_temp)$link, "next")) {
        n_pages <- n_pages + 1
      } else {
        should_continue <- FALSE
      }
    }
  }
  pages <- stringr::str_replace(base_url, "page=[0-9]{1,}", sprintf("page=%s", 1:n_pages))
  return(pages)
}

has_rel <- function(x, rel) {
  stopifnot(!is.null(rel))
  any(grepl(paste0("rel=\"", rel, "\""), x))
}
