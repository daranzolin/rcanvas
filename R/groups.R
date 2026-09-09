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
  url <- make_canvas_url("users", "self", "groups")
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
  url <- make_canvas_url(object_type, object_id, "groups")
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
  url <- make_canvas_url("groups", group_id, "users")
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
  url <- make_canvas_url("groups", group_id, "memberships")
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
  url <- make_canvas_url(context_type, context_id, "group_categories")
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
  url <- make_canvas_url("group_categories", group_category_id)
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
  url <- make_canvas_url(context_type, context_id, "group_categories")
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
  url <- make_canvas_url("courses", course_id, "group_categories")
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
  url <- make_canvas_url("group_categories", category, "groups")
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

#' Update an existing group
#'
#' Renames or otherwise edits a group. Only non-NULL fields are sent to Canvas,
#' so omitted settings remain unchanged.
#'
#' @param group_id A valid group id.
#' @param name New name of the group.
#' @param description New description of the group.
#' @param is_public Whether the group is public. Note that Canvas does not allow
#' setting this back to \code{FALSE} once a group is public.
#' @param join_level Who can join the group: \code{"parent_context_auto_join"},
#' \code{"parent_context_request"}, or \code{"invitation_only"}.
#' @param avatar_id The id of a previously uploaded attachment to use as the
#' group avatar.
#' @param storage_quota_mb Storage quota for the group, in megabytes.
#' @param members The user ids the group should contain. Any users currently in
#' the group who are not included will be removed.
#' @param sis_group_id The SIS id of the group.
#' @param override_sis_stickiness Whether to update SIS-managed fields.
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' update_group(23, name = "Project team 1")
#' update_group(23, description = "Final project", join_level = "invitation_only")
#' }
update_group <- function(group_id, name = NULL, description = NULL,
                         is_public = NULL, join_level = NULL, avatar_id = NULL,
                         storage_quota_mb = NULL, members = NULL,
                         sis_group_id = NULL,
                         override_sis_stickiness = NULL) {
  stopifnot(length(group_id) == 1)
  if (!is.null(join_level)) {
    join_level <- match.arg(join_level,
                            c("parent_context_auto_join",
                              "parent_context_request", "invitation_only"))
  }

  args <- list(
    name = name,
    description = description,
    is_public = is_public,
    join_level = join_level,
    avatar_id = avatar_id,
    storage_quota_mb = storage_quota_mb,
    sis_group_id = sis_group_id,
    override_sis_stickiness = override_sis_stickiness
  ) %>%
    purrr::discard(is.null)
  args <- c(args, iter_args_list(members, "members[]"))
  if (length(args) == 0) {
    stop("Provide at least one group field to update.", call. = FALSE)
  }

  url <- make_canvas_url("groups", group_id)
  resp <- canvas_query(url, args, "PUT")

  message(stringr::str_c("Updated group ", group_id))
  invisible(resp)
}

#' Update an existing group category (group set)
#'
#' Renames or otherwise edits a group category. Only non-NULL fields are sent to
#' Canvas, so omitted settings remain unchanged.
#'
#' @param group_category_id A valid group category id.
#' @param name New name of the group category.
#' @param self_signup Whether students may sign up for a group themselves
#' (course group categories only): \code{"enabled"} allows self sign-up for any
#' group in the course, \code{"restricted"} only for groups in the same section.
#' Pass an empty string to disallow self sign-up.
#' @param auto_leader How group leaders are assigned automatically:
#' \code{"first"} (the first student allocated to the group) or \code{"random"}.
#' @param group_limit Maximum number of users in each group. Requires self
#' sign-up.
#' @param sis_group_category_id The SIS id of the group category.
#' @param create_group_count Create this number of groups (course group
#' categories only).
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' update_group_category(52872, name = "Final project groups")
#' update_group_category(52872, self_signup = "enabled", group_limit = 4)
#' }
update_group_category <- function(group_category_id, name = NULL,
                                  self_signup = NULL, auto_leader = NULL,
                                  group_limit = NULL,
                                  sis_group_category_id = NULL,
                                  create_group_count = NULL) {
  stopifnot(length(group_category_id) == 1)
  if (!is.null(self_signup) && !identical(self_signup, "")) {
    self_signup <- match.arg(self_signup, c("enabled", "restricted"))
  }
  if (!is.null(auto_leader)) {
    auto_leader <- match.arg(auto_leader, c("first", "random"))
  }

  args <- list(
    name = name,
    self_signup = self_signup,
    auto_leader = auto_leader,
    group_limit = group_limit,
    sis_group_category_id = sis_group_category_id,
    create_group_count = create_group_count
  ) %>%
    purrr::discard(is.null)
  if (length(args) == 0) {
    stop("Provide at least one group category field to update.", call. = FALSE)
  }

  url <- make_canvas_url("group_categories", group_category_id)
  resp <- canvas_query(url, args, "PUT")

  message(stringr::str_c("Updated group category ", group_category_id))
  invisible(resp)
}
