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

canvas_query <- function(urlx, args = NULL, type = "GET") {
  fun <- getFromNamespace(type, "httr")
  args <- sc(args)
  if (type %in% c("POST", "PUT")) {
    resp <- fun(urlx,
                httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                httr::add_headers(Authorization = paste("Bearer", check_token())),
                body = args)
  } else {
    resp <- fun(urlx,
                httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                httr::add_headers(Authorization = paste("Bearer", check_token())),
                query = args)
  }
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

#' Exectute a query on the remove API
#'
#' This function allows you to call methods which are not specifically exposed by this API yet
#'
#' @param endpoint the API endpoint to call, with or without the canvas domain. You can give a vector of parts which will be joined with slashes.
#' @param args a list of arguments for the call
#' @param method GET or POST
#' @param process_response if TRUE (default for GET requests), paginate results and return a data frame
#' @return A data.frame if process_response is TRUE, otherwise an httr response
#'
#' @export
#' @examples
#' # A get request to the announcements endpoint (replicating get_announcements):
#' do_query("announcements", list(`context_codes[]`="course_1234"))
#'
#' # A post request to the group membership endpoint (replicating add_group_user):
#' do_query(c("groups", 123, "memberships"), list(user_id=1), method = "POST")
do_query <- function(endpoint, args=NULL, method="GET", process_response=(method == "GET")) {
  endpoint = paste(endpoint, collapse="/")
  if (!grepl("^https?://", endpoint)) endpoint = paste0(canvas_url(), endpoint)
  if (process_response) {
    if (method != "GET") stop("Process_response can only be used on GET requests")
    process_response(endpoint, args)
  } else {
    invisible(canvas_query(endpoint, args, method))
  }
}
