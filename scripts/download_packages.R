library(stringr)

args <- commandArgs(trailingOnly = TRUE)

options(repos = "https://packagemanager.rstudio.com/cran/__linux__/jammy/latest")

# Test if a package destination directory was passed as an argument
if (length(args) == 0) {
  stop("Usage: R -f download_packages.R destination_directory package1,package2,package3", call. = FALSE)
}

# Create the directory
if (!dir.exists(args[1])) {
        message("Creating the directory")
        dir.create(args[1], recursive = TRUE)
}

# Split the packages into a character list
input_packs <- unlist(str_split(args[2], ","))

message(input_packs)

options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
        paste(getRversion(), R.version["platform"],
        R.version["arch"], R.version["os"])))

# Get packages function to get the packages and dependencies
get_packages <- function(packs) {
        message("Getting the package dependencies")
        packages <- unlist(
                tools::package_dependencies(packs, available.packages(),
                        which = c("Depends", "Imports"), recursive = TRUE
                )
        )
        packages <- union(packs, packages)
        packages
}

# Install any GitHub packages which are not available in Posit Public Package Manager but some of their dependencies may be
github_packages_known_list <- list(c("TADA", "USEPA/TADA"))
gh_pkg_names <- sapply(github_packages, "[[", 1)
github_packages_to_build <- list()
for (p in intersect(input_packs, gh_pkg_names)) {
        gh_pkg <- github_packages_known_list[gh_pkg_names == p]
        append(github_packages_to_build, gh_pkg)
        deps <- pak::pkg_deps(gh_pkg[2])
        in_cran <- deps["ref"][deps["type"] == "standard"]
        for (g in deps[deps["type"] == "github", c("package", "ref")]) {
                append(github_packages_to_build, array(unlist(g)))
        }
        input_packs <- intersect(input_packs, in_cran)
}

# Get the package dependencies from Posit Public Package Manager
packages <- get_packages(setdiff(input_packs), gh_pkg_names)

download_and_build_cran_package <- function(pack) {
        dir.create("junktemp")
        download.packages(c(pack), destdir = "junktemp")
        pkg <- list.files("junktemp")[1]
        devtools::build(pkg = paste("junktemp", pkg, sep = "/"),
                        path = args[1], binary = TRUE)
        unlink("junktemp", recursive = TRUE)
}

download_and_build_github_package <- function(pack) {
        orig_wd <- getwd()
        setwd(args[1])
        devtools::install_github("USEPA/TADA", dependencies = FALSE,
                build = TRUE, build_opts = c("--binary"))
        setwd(orig_wd)
}

packages_needing_to_be_built <- c("sf")

# Download the packages from the repository
message(paste("Downloading the packages and dependencies to",
                args[1], sep = " "))
download.packages(setdiff(packages, packages_needing_to_be_built),
                destdir = args[1])

for (p in intersect(packages, packages_needing_to_be_built)) {
        message(paste("Package", p, "requires special handling", sep = " "))
        download_and_build_cran_package(p)
        cat(p, file = "r-build-deps.txt", sep = "\n", append = TRUE)
}

for (p in github_packages_to_build) {
        message(paste("Package", p, "must be built from GitHub", sep = " "))
        download_and_build_github_package(p)
}

if ("rmarkdown" %in% packages) {
        cat("pandoc", file = "r-build-deps.txt", sep = "\n", append = TRUE)
}

message("Completed downloading packages")

tools::write_PACKAGES(dir = args[1], fields = NULL,
  type = c("source"),
  verbose = FALSE, unpacked = FALSE, subdirs = FALSE,
  latestOnly = TRUE, addFiles = FALSE, rds_compress = "xz",
  validate = FALSE)

message("Wrote package description files")
