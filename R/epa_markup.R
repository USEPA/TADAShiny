# header_tags =
#   
  epa_header <-
  
  tags$head(
    HTML(
      "<!-- Google Tag Manager -->
		<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
		new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
		j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
		'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
		})(window,document,'script','dataLayer','GTM-L8ZB');</script>
		<!-- End Google Tag Manager -->
		"
    ),
    tags$meta(charset = "utf-8"),
    tags$meta(property = "og:site_name", content = "US EPA"),
    #tags$link(rel = "stylesheet", type = "text/css", href = "css/uswds.css"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/css/uswds.min.css",
      integrity = "sha512-ZKvR1/R8Sgyx96aq5htbFKX84hN+zNXN73sG1dEHQTASpNA8Pc53vTbPsEKTXTZn9J4G7R5Il012VNsDEReqCA==",
      crossorigin = "anonymous",
      referrerpolicy = "no-referrer"
    ),
		tags$meta(property = "og:url", content = "https://www.epa.gov/themes/epa_theme/pattern-lab/?p=pages-standalone-template"),
		tags$link(rel = "canonical", href = "https://www.epa.gov/themes/epa_theme/pattern-lab/?p=pages-standalone-template"),
		tags$link(rel = "shortlink", href = "https://www.epa.gov/themes/epa_theme/pattern-lab/?p=pages-standalone-template"),
		tags$meta(property = "og:url", content = "https://www.epa.gov/themes/epa_theme/pattern-lab/?p=pages-standalone-template"),
		tags$meta(property = "og:image", content = "https://www.epa.gov/sites/all/themes/epa/img/epa-standard-og.jpg"),
    tags$meta(property = "og:image:width", content = "1200"),
    tags$meta(property = "og:image:height", content = "630"),
    tags$meta(property = "og:image:alt", content = "U.S. Environmental Protection Agency"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:image:alt", content = "U.S. Environmental Protection Agency"),
    tags$meta(name = "twitter:image:height", content = "600"),
    tags$meta(name = "twitter:image:width", content = "1200"),
    tags$meta(name = "twitter:image", content = "https://www.epa.gov/sites/all/themes/epa/img/epa-standard-twitter.jpg"),
    tags$meta(name = "MobileOptimized", content = "width"),
    tags$meta(name = "HandheldFriendly", content = "true"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$meta(`http-equiv` = "x-ua-compatible", content = "ie=edge"),
    tags$script(src = "js/pattern-lab-head-script.js"),
    tags$title('TADAShiny | US EPA'),
    tags$link(rel = "icon", type = "image/x-icon", href = "https://www.epa.gov/themes/epa_theme/images/favicon.ico"),
    tags$meta(name = "msapplication-TileColor", content = "#FFFFFF"),
    tags$meta(name = "msapplication-TileImage", content = "https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
    tags$meta(name = "application-name", content = ""),
    tags$meta(name = "msapplication-config", content = "https://www.epa.gov/themes/epa_theme/images/ieconfig.xml"),
    tags$link(rel = "apple-touch-icon-precomposed", sizes = "196x196", href =
                "https://www.epa.gov/themes/epa_theme/images/favicon-196.png"),
    tags$link(rel = "apple-touch-icon-precomposed", sizes = "152x152", href =
                "https://www.epa.gov/themes/epa_theme/images/favicon-152.png"),
    tags$link(rel = "apple-touch-icon-precomposed", sizes = "144x144", href =
                "https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
    tags$link(rel = "apple-touch-icon-precomposed", sizes = "120x120", href =
                "https://www.epa.gov/themes/epa_theme/images/favicon-120.png"),
    tags$link(rel = "apple-touch-icon-precomposed", sizes = "114x114", href =
                "https://www.epa.gov/themes/epa_theme/images/favicon-114.png"),
    tags$link(rel = "apple-touch-icon-precomposed", sizes = "72x72", href =
                "https://www.epa.gov/themes/epa_theme/images/favicon-72.png"),
    tags$link(rel = "apple-touch-icon-precomposed", href = "https://www.epa.gov/themes/epa_theme/images/favicon-180.png"),
    tags$link(rel = "icon", href = "https://www.epa.gov/themes/epa_theme/images/favicon-32.png", sizes =
                "32x32"),
    tags$link(
      rel = "preload",
      href = "https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-regular-webfont.woff2",
      as = "font",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "preload",
      href = "https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-bold-webfont.woff2",
      as = "font",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "preload",
      href = "https://www.epa.gov/themes/epa_theme/fonts/merriweather/Latin-Merriweather-Bold.woff2",
      as = "font",
      crossorigin = "anonymous"
    ),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/ajax-progress.module.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/autocomplete-loading.module.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/js.module.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/sticky-header.module.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/system-status-counter.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-counters.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-general-info.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/tabledrag.module.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/tablesort.module.css?r6lsex"),
    tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/core/themes/stable/css/system/components/tree-child.module.css?r6lsex"),
		
		#################
		# BELOW (https://www.epa.gov/themes/epa_theme/css/styles.css?r6lsex) CAUSES ISSUES WITH PLOTLY AND LEAFLET
		# tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/themes/epa_theme/css/styles.css?r6lsex"),
		# reference css file locally instead (see styles.css in www folder)
		# doing this still causes the PLOTLY and LEAFLET issues
		tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
		#################
		
		tags$link(rel = "stylesheet", media = "all", href = "https://www.epa.gov/themes/epa_theme/css-lib/colorbox.min.css?r6lsex"),
		# this is a beta file, may have issues, js files tend to have issues
		tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds-init.min.js'),
    # fix container-fluid that boostrap RShiny uses
    tags$style(
      HTML(
        '.container-fluid {
            padding-right: 0;
            padding-left: 0;
            margin-right: 0;
            margin-left: 0;
        }
        .tab-content {
            margin-right: 30px;
            margin-left: 30px;
        }'
      )
    ),
    
    tags$body(
      class = "path-themes not-front has-wide-template",
      id = "top",
      tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds.min.js')
    )
  )
