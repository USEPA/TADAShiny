test_that("app_server initializes reactive state and disables expected tabs", {
  srv <- app_server
  
  # Stub all modules that exist in this app
  fake_module <- function(id, ...) invisible(NULL)
  for (fn in c(
    "mod_filtering_server","mod_query_data_server","mod_data_flagging_server",
    "mod_summary_server","mod_overview_server","mod_censored_data_server",
    "mod_harmonize_np_server","mod_depth_server","mod_review_data_server",
    "mod_figures_server","mod_TADA_summary_server"
  )) mockery::stub(srv, fn, fake_module)
  
  # Capture shinyjs::disable calls
  disabled_selectors <- character(0)
  fake_disable <- function(selector, ...) {
    disabled_selectors <<- c(disabled_selectors, selector)
    invisible(NULL)
  }
  mockery::stub(srv, "shinyjs::disable", fake_disable)
  
  shiny::testServer(srv, {
    # ReactiveValues constructed
    expect_true(inherits(tadat, "reactivevalues"))
    
    # Defaults initialized
    expect_true(is.na(tadat$load_progress_file))
    expect_true(is.na(tadat$save_progress_file))
    expect_identical(tadat$flags_present, FALSE)
    
    # Job id + default outfile
    expect_true(is.character(tadat$job_id))
    expect_match(tadat$job_id, "^ts[0-9]{12}$")  # 12 digits: YYMMDDhhmmss
    expect_identical(tadat$default_outfile, paste0("tada_output_", tadat$job_id))
  })
  
  expect_setequal(
    disabled_selectors,
    c(
      '.nav li a[data-value="Overview"]',
      '.nav li a[data-value="Flag"]',
      '.nav li a[data-value="Filter"]',
      '.nav li a[data-value="Censored"]',
      '.nav li a[data-value="Harmonize"]',
      '.nav li a[data-value="Depth"]',
      '.nav li a[data-value="Figures"]',
      '.nav li a[data-value="Review"]'
    )
  )
})

test_that("app_server shows modal and switches to Overview on new data, and tracks tab", {
  srv <- app_server
  
  # Stub modules
  fake_module <- function(id, ...) invisible(NULL)
  for (fn in c(
    "mod_filtering_server","mod_query_data_server","mod_data_flagging_server",
    "mod_summary_server","mod_overview_server","mod_censored_data_server",
    "mod_harmonize_np_server","mod_depth_server","mod_review_data_server",
    "mod_figures_server","mod_TADA_summary_server"
  )) mockery::stub(srv, fn, fake_module)
  mockery::stub(srv, "shinyjs::disable", function(...) invisible(NULL))
  
  # Capture modal and tabset updates
  modal_shown <- FALSE
  selected_tab <- NULL
  mockery::stub(srv, "shiny::showModal", function(...) { modal_shown <<- TRUE; invisible(NULL) })
  mockery::stub(srv, "shiny::updateTabsetPanel", function(session, inputId, selected) {
    selected_tab <<- selected
    invisible(NULL)
  })
  
  shiny::testServer(srv, {
    # Provide minimal raw/new to trigger the observer
    tadat$raw <- data.frame(
      ResultIdentifier = 1:2,
      MonitoringLocationIdentifier = c("A","B"),
      stringsAsFactors = FALSE
    )
    tadat$new <- TRUE
    session$flushReact()
    
    expect_true(modal_shown)
    expect_identical(selected_tab, "Overview")
    
    # Track active tab
    session$setInputs(tabbar = "Review")
    session$flushReact()
    expect_identical(tadat$tab, "Review")
  })
})

