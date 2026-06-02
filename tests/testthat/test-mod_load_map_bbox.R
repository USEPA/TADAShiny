# Pin the original module function at file load time
mod_map_bboxServer_orig <- get("mod_map_bboxServer", asNamespace("TADAShiny"))

test_that("mod_map_bboxUI renders expected controls with 0.001 step and address search", {
  ui <- mod_map_bboxUI("bbox", step = 0.001)
  tq <- htmltools::tagQuery(ui)
  ns <- shiny::NS("bbox")

  # Leaflet output present (allow >= 1 due to wrapper duplication)
  expect_gte(length(tq$find(sprintf("#%s", ns("map_bbox")))$all()), 1L)

  # Address input and Find button present
  expect_gte(length(tq$find(sprintf("#%s", ns("addr")))$all()), 1L)
  expect_gte(length(tq$find(sprintf("#%s", ns("addr_find")))$all()), 1L)

  # Numeric inputs present; verify the <input type="number"> has step 0.001
  for (id in c("bb_N", "bb_S", "bb_W", "bb_E")) {
    node <- tq$find(sprintf("#%s", ns(id)))
    expect_gte(length(node$all()), 1L)

    # Find the actual input tag among selected tags
    tags <- node$selectedTags()
    input_tag <- NULL
    for (tg in tags) {
      if (identical(tg$name, "input")) {
        input_tag <- tg
        break
      }
    }
    expect_false(is.null(input_tag))
    expect_identical(input_tag$attribs$type, "number")
    expect_true(!is.null(input_tag$attribs$step))
    expect_equal(as.numeric(input_tag$attribs$step), 0.001, tolerance = 1e-12)
  }
})
