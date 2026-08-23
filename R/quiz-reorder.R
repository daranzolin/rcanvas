#' Reorder questions and groups in a classic quiz
#'
#' Canvas maintains quiz display order through a separate reorder endpoint.
#' Supply item ids in their desired order and identify each as a question or
#' question group.
#'
#' @param course_id A valid course id.
#' @param quiz_id A valid classic quiz id.
#' @param item_ids Quiz question or question-group ids in the desired order.
#' @param item_types Either a single type recycled across all ids or one type
#' per id. Allowed values are \code{"question"} and \code{"group"}.
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' reorder_quiz_items(20, 123, c(456, 457, 458))
#' reorder_quiz_items(20, 123, c(456, 99), c("question", "group"))
#' }
reorder_quiz_items <- function(course_id, quiz_id, item_ids,
                               item_types = "question") {
  stopifnot(length(course_id) == 1, length(quiz_id) == 1,
            length(item_ids) > 0)

  if (length(item_types) == 1) {
    item_types <- rep(item_types, length(item_ids))
  }
  if (length(item_types) != length(item_ids) ||
      !all(item_types %in% c("question", "group"))) {
    stop("item_types must contain one valid type per item id.",
         call. = FALSE)
  }

  args <- purrr::map2(
    item_ids,
    item_types,
    ~ list(`order[][id]` = .x, `order[][type]` = .y)
  ) %>%
    purrr::flatten()
  url <- make_canvas_url("courses", course_id, "quizzes", quiz_id,
                         "reorder")
  resp <- canvas_query(url, args, "POST")

  message(stringr::str_c(length(item_ids), " items reordered in quiz ",
                         quiz_id, " in course ", course_id))
  invisible(resp)
}
