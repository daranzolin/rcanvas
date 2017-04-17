#' Groups
#'
#' Groups serve as the data for a few different ideas in Canvas. The first is that they can be a community in the canvas network. The second is that they can be organized by students in a course, for study or communication (but not grading). The third is that they can be organized by teachers or account administrators for the purpose of projects, assignments, and grading. This last kind of group is always part of a group category, which adds the restriction that a user may only be a member of one group per category.
#' All of these types of groups function similarly, and can be the parent context for many other types of functionality and interaction, such as collections, discussions, wikis, and shared files.
#' Group memberships are the objects that tie users and groups together.
#' @md
#' @name groups
NULL

#' `get_groups_self`: Returns groups which the current user (you) belongs to
#' @export
#' @md
#' @rdname groups
#' @examples
#' \dontrun{get_groups_self}
get_groups_self <- function() {
  url <- paste0(canvas_url(), "users/self/groups", sep = "/")
  args <- list(access_token = check_token(),
               per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}

#' `get_groups_context`: Returns the list of active groups in the given context that are visible to user.
#'
#' @md
#' @rdname groups
#' @param object_id id for given type
#' @param object_type course or account
#' @export
#' @examples
#' \dontrun{get_groups_context(27)}
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

#' `get_group_users`: Get users which belong to a group
#'
#' @param group_id which group
#' @param group_name Optional, a name for the group. To be used when you know
#' the name of the group to use. This is generally of form "Group 13", and is
#' what is exposed to you via the Canvas UI. This is noticeably more
#' user-friendly than the group ID number.
#'
#' @return users in a group
#' @export
#' @md
#' @rdname groups
#'
#' @examples
#' \dontrun{get_group_users(27314)}
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
#' \dontrun{get_course_user_groups(27)}
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
