
## Importing and exporting

Use only box::use for imports. Using library and :: is forbidden.

box::use statement (if needed) should be located at the top of the file.

There can be two box::use statements per file. First one should include only R packages, second should only import other scripts.

Imports in box::use should be sorted alphabetically.

Using [...] is forbidden.

All external functions in a script should be imported. This includes operators, like %>%.

A script should only import functions that it uses.
Ways of importing

There are two ways a package or a script can be imported:

    List imported functions - functions imported are listed in []

box::use(
  dplyr[filter],
)

filter(mtcars, cyl > 4)

Use it if there are no more than 8 functions imported from this package/script.

    Import package and access functions with $

box::use(
  dplyr,
)
dplyr$filter(mtcars, cyl > 4)

When moving function into a different script, remember to adjust imports in box::use:

    Add import for all required functions to the file where you moved the function.
    Make sure to follow the correct way of importing (direct or using $) in the new file. Modify it if needed.
    Remove redundant imports from the original file.
    Import the moved function in the original file.

Use it if there are more than 8 functions imported from this package/script.
## Exporting

If a function is used only inside a script, it should not be exported.

If a function is used by other scripts, it should be exported by adding #' @export before the function.
## Rhino modules

When creating a new module in app/view, use the template:

box::use(
  shiny[moduleServer, NS]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

## Unit tests

All R unit tests are located in tests/testthat.

There should be only one test file per script, named test-{script name}.R.

If testing private functions (ones that are not exported), use this pattern:

box::use(app/logic/mymod)

impl <- attr(mymod, "namespace")

test_that('{test description}', {
    expect_true(impl$this_works())
})

## Testing exported and non-exported functions

When testing a box module that contains both exported and non-exported functions:

    Import the entire module without specifying individual functions:

box::use(
  app/logic/mymodule,
)

    Access exported functions using the module name with $:

test_that("exported function works", {
  expect_equal(mymodule$exported_function(1), 2)
})

    For testing non-exported functions, get the module’s namespace at the start of the test file:

impl <- attr(mymodule, "namespace")

test_that("non-exported function works", {
  expect_equal(impl$internal_function(1), 2)
})

This pattern allows testing both public and private functions while maintaining proper encapsulation.

## Code style

The maximum line length is 180 characters.
