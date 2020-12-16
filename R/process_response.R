#' Process a Canvas API response
#'
#' Wrapper function for common tasks in going from Canvas URL to data.frame. Most
#' of the heavy lifting is done in \code{paginate}, which finds which pages to
#' download. This function adds necessary arguments to those pages (e.g. the
#' authentication token), downloads the content, converts from JSON into
#' data.frame format, and if there are multiple pages/data.frames, converts it
#' into one final data.frame if able.
#'
#' @param url url to query
#' @param args query arguments to be passed to \code{httr}, e.g. auth token
#'
#' @return processed dataframe or list if unable to simplify
#' @importFrom magrittr `%>%`
process_response <- function(url, args) {

  resp <- canvas_query(url, args, "GET")

  d <- paginate(resp) %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE)

  # flatten to data.frame if able, otherwise return as is
  #d <- tryCatch(purrr::map_df(d, purrr::flatten_df),
  #              error = function(e) d)
  dplyr::bind_rows(d)
}

#' Get responses from Canvas API pages
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
#' @param showProgress if TRUE (default), show a textual progress bar
#'
#' @return unparsed responses
#'
#' @examples
#' \dontrun{resp <- canvas_query(url, args, "HEAD")
#' get_pages(resp)}
paginate <- function(x, showProgress=T) {
  first_response <- list(x)
  stopifnot(httr::status_code(x) == 200) # OK status
  pages <- httr::headers(x)$link

  if (is.null(pages)) return(first_response)

  should_continue <- TRUE

  if (has_rel(pages, "last")) {
    last_page <- get_page(x, "last")
    n_pages <- readr::parse_number(stringr::str_extract(last_page, "page=[0-9]{1,}"))

    if (n_pages == 1){
      return(first_response)
    }

    pages <- increment_pages(last_page, 2:n_pages)
    if (showProgress){
      bar = txtProgressBar(max=n_pages, style = 3)
    }

    queryfunc = function(...) {if (showProgress) bar$up(bar$getVal()+1); canvas_query(...)}
    responses <- pages %>%
      purrr::map(queryfunc, args = list(access_token = check_token()))
    responses <- c(first_response, responses)

    return(responses)
  } else {
    if (has_rel(httr::headers(x)$link, "next")) {
      pages[[1]] <- get_page(x, "current")

      inc <- 2

      # edge case for if there is no 'last' header, see:
      # https://canvas.instructure.com/doc/api/file.pagination.html
      # https://github.com/daranzolin/rcanvas/issues/4
      while (should_continue) {
        page_temp <- get_page(x, "next")
        pages[[inc]] <- page_temp
        x <- canvas_query(page_temp,
                          args = list(access_token = check_token()),
                          type = "HEAD")
        if (!has_rel(httr::headers(x)$link, "next")) {
          should_continue <- FALSE
        } else {
          inc <- inc + 1
        }
      }


      responses <- pages %>%
        purrr::map(canvas_query, args = list(access_token = check_token()))
    }
  }
}

increment_pages <- function(base_url, n_pages) {
  # odd regex but necessary, see http://regexr.com/3evr4
  stringr::str_replace(base_url, "([\\?&])(page=[0-9a-zA-Z]{1,})",
                       sprintf("\\1page=%s", n_pages))
}

has_rel <- function(x, rel) {
  stopifnot(!is.null(rel))
  any(grepl(paste0("rel=\"", rel, "\""), x))
}

get_page <- function(resp, page) {
  pages <- resp$headers$link
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  pages <- stringr::str_split(pages, ",")[[1]]
  url <- stringr::str_subset(pages, page)
  url <- stringr::str_extract(url, url_pattern)
  url <- stringr::str_replace_all(url, "[<>;]", "")
  return(url)
}
