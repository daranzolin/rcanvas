#' Get groups given context
#'
#' Returns the list of active groups in the given context that are visible to user.
#'
#' @param object_id id for given type
#' @param object_type course or account
#'
#' @return returns groups available for a given course or account
#' @export
#'
#' @examples
#' get_groups_context(27)
get_groups_context <- function(object_id, object_type = "courses") {
  stopifnot(object_type %in% c("courses", "accounts"))
  url <- paste0(canvas_url(), paste(object_type, object_id, "groups", sep = "/"))
  args <- list(access_token = check_token(),
               per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}

#' Get group users
#'
#' @param group_id which group
#'
#' @return users in a group
#' @export
#'
#' @examples
#' get_group_users(27314)
get_group_users <- function(group_id, group_name) {
  if(missing(group_name)) {
    group_name <- NA
  }
  url <- paste0(canvas_url(),
                paste("groups", group_id, "users", sep = "/"))
  args <- list(access_token = check_token(),
               per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat %>% dplyr::mutate(group_id = group_id,
                        group_name = group_name)
}

#' Get all users in a course and which group they are signed up for
#'
#' @importFrom magrittr %>%
#' @param course_id which course
#'
#' @return dataframe with user name, user id, and group id
#' @export
#'
#' @examples
get_course_user_groups <- function(course_id) {
  all_course_groups <- get_groups_context(course_id)
  grouped_users <- purrr::map2_df(all_course_groups$id, all_course_groups$name,
                                  get_group_users)
  all_users <- get_course_items(course_id, item = "students")
  grouped_users <- dplyr::select(grouped_users, id, group_id, group_name)
  all_users <- dplyr::select(all_users, id, sortable_name)
  all_users %>%
    dplyr::left_join(grouped_users, by = "id") %>%
    unique
}
