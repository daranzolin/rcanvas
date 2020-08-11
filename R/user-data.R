#' Get various user items
#'
#' @param user_id A valid canvas user id
#' @param item One of "missing_submissions", "details", "profile", "page_views", "colors", or "avatars"
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' get_user_items(365, "details")
get_user_items <- function(user_id, item) {
  if (item == "page_views") warning("Not all page views will be returned.")

  if (item == "details") {
    url <- make_canvas_url("users", user_id)
  } else {
    url <- make_canvas_url("users", user_id, item)
  }

  args <- list(access_token = check_token(),
               per_page = 100)

  dat <- process_response(url, args)
  return(dat)
}
