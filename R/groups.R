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
  args <- list(per_page = 100)
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
  args <- list(per_page = 100)
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
  args <- list(per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat %>% dplyr::mutate(group_id = group_id,
                        group_name = group_name)
}

#' Add the user to the group
#'
#' @param group_id the group ID
#' @param user_id the user ID
#' @rdname groups
#' @examples
#' \dontrun{add_group_users(group_id=23, user_ids=327))}
add_group_user <- function(group_id, user_id) {
  url <- paste0(canvas_url(),
                paste("groups", group_id, "memberships", sep="/"))
  args <- list(user_id = user_id)

  invisible(canvas_query(url, args, "POST"))
}

#' Add user(s) to group(s)
#'
#' Add one or more users to a group (or multiple groups).
#' group_id can be a single group ID, in which case all users are added to
#' that group. It can also be a vector of group IDs of the same length as
#' user IDs, in which case each user will be added to the corresponding group
#'
#' @param group_id the group ID or IDs
#' @param user_ids the users IDS to add to the group
#' @export
#' @rdname groups
#' @examples
#' \dontrun{add_multiple_group_users(group_id=23, user_ids=c(327, 328))}
#' \dontrun{add_multiple_group_users(group_id=c(23, 24), user_ids=c(327, 328))}
add_group_users <- function(group_id, user_ids) {
  invisible(purrr::map2(group_id, user_ids, add_group_user))
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

#' Group categories
#'
#' @param context_id context id
#' @param context_type context type
#'
#' @return data frame
#' @export
#'
#' @examples
#' get_group_categories(1350207)
get_group_categories <- function(context_id, context_type = "courses") {
  stopifnot(context_type %in% c("courses", "accounts"))
  url <- paste0(canvas_url(), paste(context_type, context_id,
                                    "group_categories", sep = "/"))
  args <- list(per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}

#' Get a single group category
#'
#' @param group_category_id
#'
#' @return data frame
#' @export
#'
#' @examples
#' get_group_category(52872)
get_group_category <- function(group_category_id) {
  url <- paste0(canvas_url(), paste("group_categories", group_category_id,
                                    sep = "/"))
  args <- list(per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}

#' Create a group category
#'
#' Does not work yet. Returns 422. Unclear how to fix.
#'
#' @param context_id Context id
#' @param context_type Context type
#' @param cat_name Name of the group category. Required.
#' @param self_signup Allow students to sign up for a group themselves (Course Only). valid values are: “enabled”, allows students to self sign up for any group in course;  “restricted” allows students to self sign up only for groups in the same section null disallows self sign up
#' @param auto_leader Assigns group leaders automatically when generating and allocating students to groups. Valid values are: “first” the first student to be allocated to a group is the leader; “random” a random student from all members is chosen as the leader
#' @param group_limit Limit the maximum number of users in each group (Course Only). Requires self signup.
#' @param create_group_count Create this number of groups (Course Only).
#'
#' @return invisible
#'
#' @examples
#' create_group_category(1350207, "courses", "FinalProjectGroup",
#' "enabled", "first", 3, 48)
create_group_category <- function(context_id, context_type = "courses",
                                  cat_name, self_signup = NULL,
                                  auto_leader = NULL, group_limit = NULL,
                                  create_group_count= NULL) {
  stopifnot(context_type %in% c("courses", "accounts"))
  url <- paste0(canvas_url(), paste(context_type, context_id,
                                    "group_categories", sep = "/"))
  args <- list(name = cat_name,
               self_signup = self_signup,
               auto_leader = auto_leader,
               group_limit = group_limit,
               create_group_count = create_group_count)
  sc(args)
  canvas_query(url, args, "PUT")
}

#' Get the group categories (group sets) for the given course
#'
#' @param course_id the Course ID to get the group sets for
#' @return a tibble with one group set per row
#' @rdname groups
#' @export
#' @examples
#' \dontrun{get_group_categories(361)}
get_group_categories <- function(course_id) {
  url <- paste0(canvas_url(),
                paste("courses", course_id, "group_categories", sep = "/"))
  args <- list(per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  process_response(url, args)
}


#' Create a new group
#' @rdname groups
#' @param category the ID of the group category (group set)
#' @param name the name of the new group
#' @param description Description of the new group
#' @param join_level Join level of the new group (who can join the group)
#' @examples
#' \dontrun{add_group(category=128,name="group name", description="description", join_level="invitation_only")}
#'
add_group <- function(category, name, description, join_level) {
  url <- paste0(canvas_url(),
                paste("group_categories", category, "groups", sep="/"))
  args <- list(name=name, description=description, join_level=join_level)
  invisible(canvas_query(url, args, "POST"))
}

#' Create new group(s)
#'
#' Creates one or more new groups in an existing group set (category)
#'
#' @rdname groups
#' @param category the ID of the group category (group set)
#' @param name the name(s) of the new group
#' @param description Description(s) of the new group
#' @param join_level Join level of the new group (who can join the group)
#' @export
#' @examples
#' \dontrun{add_group(category=128,name=paste('group', 1:2), description="test groups", join_level="invitation_only")}
add_groups <- function(category, name, description, join_level=c("parent_context_auto_join", "parent_context_request", "invitation_only")) {
  join_level = match.arg(join_level)
  invisible(purrr::map2(category, name, add_group, description, join_level))
}
