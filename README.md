
<!-- README.md is generated from README.Rmd. Please edit that file -->
![](https://avatars3.githubusercontent.com/u/515326?v=3&s=400)

Description
===========

`rcanvas` is a bouquet of functions to query your institution's instance of the [Canvas LMS.](https://www.canvaslms.com)

Installation
============

`rcanvas` is not on CRAN, but can be installed via:

``` r
devtools::install_github("daranzolin/rcanvas")
library(rcanvas)
```

Setup
=====

Some prep work is required before use. You must first stash two items in your `.Renviron` file: (1) your third-party access token, available through your Canvas LMS account settings; and (2) your institution's particular domain. (e.g. `"https://<your_domain>.instructure.com/"`) Name your token `CANVAS_API_TOKEN`, and your domain `CANVAS_DOMAIN`. For help on setting up your `.Renviron` (and all things R) see Jenny Bryan's [Stat545 page.](http://stat545.com/bit003_api-key-env-var.html)

The process for getting your Canvas API token:

`Canvas -> Account -> Settings -> Approved Integrations -> Add new token`

We provide two helper functions to make this process easier.

``` r
set_canvas_token("YOUR_TOKEN_HERE")
set_canvas_domain("https://canvas.yourdomain.edu")
```

Usage
=====

Each function returns a data frame with various information.

Course Data
-----------

``` r
###Get all courses:
get_course_list()

get_course_list(include = c("teachers", "total_students"))

###Get course analytics data:
get_course_analytics_data(course_id = 20, type = "activity")

###Get course items:
get_course_items(course_id = 20, item = "users", include = "email")
```

------------------------------------------------------------------------

User Data
---------

``` r
get_user_items(user_id = 365, item = "details")
get_user_items(365, "missing_submissions")
```

------------------------------------------------------------------------

Gradebooks
----------

This function is fickle, but special thanks to [Garth Egbert for the inspiration.](https://community.canvaslms.com/groups/canvas-developers/blog/2016/07/09/gradebook-analysis-error-checking-the-gradebook)

``` r
get_course_gradebook(course_id = 20)
```

------------------------------------------------------------------------

Submissions
-----------

Submissions are available for both quizzes and assignments, e.g.:

``` r
get_submissions(course_id = 27, type = "assignments", type_id = 2248)
get_submissions(27, "quizzes", 168)
```

You can also comment on assignments or assign them scores programatically.

``` r
comment_submission(course_id = 27, assignment_id = 2248, 
                   user_id = 4227, comm = "Test comment!")
grade_submission(course_id = 27, assignment_id = 2248, 
                   user_id = 4227, grade = 95)
```

Groups
------

Students can belong to groups, which belong to a particular course object. The functions in `groups.R` deal with listing the users and groups in each course.

``` r
get_course_user_groups(course_id = 27)
```

Announcements and Discussions
-----------------------------

Additionally, we have some functions that will collect the announcements or discussions from a particular course.

``` r
get_announcements(course_id = 27) 
get_announcements(course_id = 27, start_date = "2017-02-01") 
```

``` r
get_discussions_context(4371405, object_type = "courses")
```

Future Work
===========

-   Additional functions
-   More precise querying
-   More tests
-   Vignette
