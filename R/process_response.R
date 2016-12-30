process_response <- function(url, args) {
  resp <- canvas_query(url, args, "HEAD")
  dat_list <- get_pages(resp)
  dat_list %>%
    purrr::map(canvas_query, args) %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
    dplyr::bind_rows()
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
  should_continue <- TRUE
  if (has_last(httr::headers(x)$link)) {
    n_pages <- readr::parse_number(stringr::str_extract(pages[length(pages)], "page=[0-9]{1,}"))
  } else {
    while (should_continue) {
      page_temp <- stringr::str_replace(base_url, "page=[0-9]{1,}", sprintf("page=%s", n_pages))
      links_temp <- canvas_query(page_temp,
                                 args = list(access_token = check_token()),
                                 type = "HEAD")
      if (has_next(httr::headers(links_temp)$link)) {
        n_pages <- n_pages + 1
      } else {
        should_continue <- FALSE
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
