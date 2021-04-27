#' Canvas API helpers
#'
#' These functions set your Canvas API token, as well as the Canvas base URL.
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
  keyring::key_set_with_value("rcanvas_CANVAS_API_TOKEN", NULL, token)
}

# env for the Canvas domain
cdenv <- new.env()

#' @param domain Canvas domain
#' @export
#' @rdname apihelpers
#' @examples
#' set_canvas_domain("https://canvas.upenn.edu")
set_canvas_domain <- function(domain) {
  assign("rcanvas_CANVAS_DOMAIN", domain, envir = cdenv)
}

#' @rdname apihelpers
check_token <- function() {
  token <- keyring::key_get("rcanvas_CANVAS_API_TOKEN")
  if (identical(token, "")) {
    stop("Please set your Canvas API token with set_canvas_token.",
         call. = FALSE)
  }
  token
}

canvas_url <- function() paste0(get("rcanvas_CANVAS_DOMAIN", envir = cdenv), "/api/v1")

make_canvas_url <- function(...) {
  url = paste(canvas_url(), ..., sep = "/")
  if (getOption('.rcanvas.show.url', default = FALSE)) {
    message(url)
  }
  return(url)
}

#' @importFrom httr GET POST PUT HEAD
canvas_query <- function(urlx, args = NULL, type = "GET") {

  args <- sc(args)
  resp_fun_args <- list(url = urlx,
                        httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                        httr::add_headers(Authorization = paste("Bearer", check_token())))

  if (type %in% c("POST", "PUT"))
    resp_fun_args$body = args
  else
    resp_fun_args$query = args

  resp <- do.call(type, resp_fun_args)

  httr::stop_for_status(resp)
  resp

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
  purrr::discard(x, is.null)
}

convert_dates <- function(base_date = Sys.Date(), days) {
  new_date <- base_date + lubridate::ddays(days)
  format(new_date, "%Y-%m-%d")
}

#' Execute a query on the remove API
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
