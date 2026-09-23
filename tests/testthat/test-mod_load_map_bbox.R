# test bbox function helpers

patch_leaflet_fun <- function(fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace("leaflet"))
  assignInNamespace(fn_name, replacement, ns = "leaflet")
  list(fn = fn_name, old = old)
}

restore_leaflet_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = "leaflet")
}

test_that("GetURL builds USGS WMS URL with default and custom host", {
  expect_identical(
    GetURL("USGSTopo"),
    "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WMSServer"
  )

  expect_identical(
    GetURL("USGSHydroCached", host = "example.org"),
    "https://example.org/arcgis/services/USGSHydroCached/MapServer/WMSServer"
  )
})

test_that("USGS group labels and attribution constants are stable", {
  expect_identical(
    grp,
    c(
      "USGS Topo",
      "USGS Imagery Only",
      "USGS Imagery Topo",
      "USGS Shaded Relief",
      "Hydrography"
    )
  )

  expect_match(att, "U\\.S\\. Geological Survey")
  expect_match(att, "Policies")
  expect_match(att, "https://www\\.usgs\\.gov/")
})

test_that("add_USGS_base wires expected WMS layers, hideGroup calls, and layer control", {
  calls <- new.env(parent = emptyenv())
  calls$wmstiles <- list()
  calls$hide <- character(0)
  calls$wmstile_opt <- list()
  calls$layers_opt <- list()
  calls$layers_control <- list()

  patches <- list(
    patch_leaflet_fun(
      "addWMSTiles",
      function(
        x,
        baseUrl,
        group,
        attribution = NULL,
        layers = NULL,
        options = NULL
      ) {
        calls$wmstiles[[length(calls$wmstiles) + 1L]] <<- list(
          url = baseUrl,
          group = group,
          attribution = attribution,
          layers = layers,
          options = options
        )
        x
      }
    ),
    patch_leaflet_fun("hideGroup", function(x, group) {
      calls$hide <<- c(calls$hide, group)
      x
    }),
    patch_leaflet_fun("WMSTileOptions", function(...) {
      calls$wmstile_opt <<- list(...)
      list(kind = "wmstile", ...)
    }),
    patch_leaflet_fun("layersControlOptions", function(...) {
      calls$layers_opt <<- list(...)
      list(kind = "layers_control", ...)
    }),
    patch_leaflet_fun(
      "addLayersControl",
      function(
        x,
        baseGroups,
        overlayGroups,
        options = NULL,
        position = "topright"
      ) {
        calls$layers_control <<- list(
          baseGroups = baseGroups,
          overlayGroups = overlayGroups,
          options = options,
          position = position
        )
        x
      }
    )
  )
  on.exit(lapply(rev(patches), restore_leaflet_fun), add = TRUE)

  sentinel_map <- structure(list(id = "map"), class = "leaflet")
  out <- add_USGS_base(sentinel_map)

  expect_identical(out, sentinel_map)

  expect_length(calls$wmstiles, 5L)
  expect_identical(
    vapply(calls$wmstiles[1:4], `[[`, character(1), "group"),
    grp[1:4]
  )
  expect_identical(calls$wmstiles[[5]]$group, grp[5])

  expect_identical(calls$wmstiles[[1]]$url, GetURL("USGSTopo"))
  expect_identical(calls$wmstiles[[2]]$url, GetURL("USGSImageryOnly"))
  expect_identical(calls$wmstiles[[3]]$url, GetURL("USGSImageryTopo"))
  expect_identical(calls$wmstiles[[4]]$url, GetURL("USGSShadedReliefOnly"))
  expect_identical(calls$wmstiles[[5]]$url, GetURL("USGSHydroCached"))

  expect_true(all(vapply(
    calls$wmstiles,
    function(z) identical(z$layers, "0"),
    logical(1)
  )))
  expect_identical(calls$wmstiles[[1]]$attribution, att)
  expect_identical(calls$wmstiles[[4]]$attribution, att)
  expect_null(calls$wmstiles[[5]]$attribution)

  expect_identical(calls$hide, c(grp[2], grp[3], grp[4], grp[5]))

  expect_identical(calls$wmstile_opt$format, "image/png")
  expect_identical(calls$wmstile_opt$transparent, TRUE)
  expect_identical(calls$layers_opt$collapsed, FALSE)

  expect_identical(calls$layers_control$baseGroups, grp[1:4])
  expect_identical(calls$layers_control$overlayGroups, grp[5])
  expect_identical(calls$layers_control$position, "topleft")
})

##############################################################################

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
