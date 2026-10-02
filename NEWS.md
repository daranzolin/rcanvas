# rcanvas 0.9.3

* Add `get_course_submissions()` with course-wide paging, assignment/student
  filters, and submitted/graded-since timestamps. `get_course_gradebook()` now
  supports incremental refresh from a saved snapshot, with separate submission
  and regrade queries, overlap, and cached enrollment metadata. Full imports
  use grouped course-wide requests. **Behavior change:** unpublished assignments
  are excluded by default; set `include_unpublished = TRUE` to include them.
  Assignment publishing changes are checked on every refresh. API errors now
  fail rather than silently producing a partial gradebook.

* Added helpers to list, create, update, delete, and reorder content in classic quizzes.
* Add helpers to list, read, create, reply to, and update Canvas Inbox
  conversations, and `add_conversation_recipients()` to add people to an
  existing thread. `create_conversation()` verifies that every recipient
  was included, since Canvas can return success while silently leaving one
  out; group conversations are repaired in place (closes #66).

* Added the following functions:
  * `create_canvas_module()`
  * `create_canvas_module_item()`
  * `get_module_list()`
* Fixed bug in `create_course_assignment()` and `create_course_folder()`

# rcanvas 0.9.2

* Added two function, `get_term_course_list()` and `get_account_course_list()`.
* `get_term_course_list()` gathers a list of all courses in a specific term.
* `get_account_course_list()` gathers a list of all courses for an account or sub-account

# rcanvas 0.9.1

* Added a `NEWS.md` file to track changes to the package.
* Uses [`pkgdown`](https://hadley.github.io/pkgdown/index.html) to generate documentation.
* Many changes to consolidate and update documentation.
* Changed "if" to "it" in the documentation of the 'add_enrollments' function

# rcanvas 0.9.0

* Initial release
