#' @title Process a Canvas API response
#'
#' Wrapper function for common tasks in going from Canvas url to dataframe. Most
#' of the heavy lifting is done in \code{get_pages}, which finds which pages to
#' download. This function adds necessary arguments to those pages (e.g. the
#' authentication token), downloads the content, converts from JSON into data
#' frame format, and if there are multiple pages/dataframes, converts it into
#' one final dataframe.
#'
#' @param url url to query
#' @param args query arguments to be passed to \code{httr}, e.g. auth token
#'
#' @return processed dataframe
#'
#' @examples
process_response <- function(url, args) {
  resp <- canvas_query(url, args, "HEAD")
  get_pages(resp) %>%
    purrr::map(canvas_query, args) %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
    dplyr::bind_rows()
}

#' @title Get pages from Canvas API response
#'
#' @description The Canvas headers include a link object (usually), in form:
#' \code{Link:
#' <https://canvas.url/api/v1/[...]?page=1&per_page=10>; rel="current",
#' <https://canvas.url/api/v1/[...]?page=2&per_page=10>; rel="next",
#' <https://canvas.url/api/v1/[...]?page=1&per_page=10>; rel="first",
#' <https://canvas.url/api/v1/[...]?page=15&per_page=10>; rel="last"}
#'
#' In this case, we need to download every page from 1 to 15 to capture all data.
#' This function parses the response object intelligently, using only HEAD
#' requests, to figure out these page requirements.
#'
#' @param x a httr response object
#'
#' @return pages to download
#'
#' @examples
#' \dontrun{resp <- canvas_query(url, args, "HEAD")
#' get_pages(resp)}
get_pages <- function(x) {
  resp_headers <- httr::headers(x)
  stopifnot(httr::status_code(x) == 200) # OK status
  pages <- resp_headers$link
  # edge case of only 1 page
  if(is.null(pages)) return(x$url)
  # parse url's from link header
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
    # edge case for if there is no 'last' header, see:
    # https://canvas.instructure.com/doc/api/file.pagination.html
    # https://github.com/daranzolin/rcanvas/issues/4
    while (should_continue) {
      page_temp <- increment_pages(base_url, n_pages)
      links_temp <- canvas_query(page_temp,
                                 args = list(access_token = check_token()),
                                 type = "HEAD")
      if (has_rel(httr::headers(links_temp)$link, "next")) {
        n_pages <- n_pages + 1
      } else {
        # we're done
        should_continue <- FALSE
      }
    }
  }
  pages <- increment_pages(base_url, 1:n_pages)
  return(pages)
}

increment_pages <- function(base_url, n_pages) {
  stringr::str_replace(base_url, "([\\?&])(page=[0-9]{1,})",
                       sprintf("\\1page=%s", n_pages))
}

has_rel <- function(x, rel) {
  stopifnot(!is.null(rel))
  any(grepl(paste0("rel=\"", rel, "\""), x))
}
