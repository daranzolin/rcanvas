#' Canvas API helpers
#'
#' These functinos set your Canvas API token, as well as the Canvas base URL.
#' These functions are necessary for `rcanvas` to run.
#'
#' @name apihelpers
#' @md

#' @param token your API token
#' @export
#' @rdname apihelpers
#' @examples
#' set_canvas_token("abc123")
set_canvas_token <- function(token) {
  Sys.setenv(CANVAS_API_TOKEN = token)
}

#' @param domain Canvas domain
#' @export
#' @rdname apihelpers
#' @examples
#' set_canvas_domain("https://canvas.upenn.edu")
set_canvas_domain <- function(domain) {
  Sys.setenv(CANVAS_DOMAIN = domain)
}

#' @rdname apihelpers
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

convert_dates <- function(base_date = Sys.Date(), days) {
  new_date <- base_date + lubridate::ddays(days)
  format(new_date, "%Y-%m-%d")
}
