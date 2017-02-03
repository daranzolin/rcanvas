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

# get_announcements("1350207", start_date = convert_dates(days = -320))
# get_announcements("1350207", start_date = "2017-02-01")

convert_dates <- function(base_date = Sys.Date(), days) {
  new_date <- base_date + lubridate::ddays(days)
  format(new_date, "%Y-%m-%d")
}
