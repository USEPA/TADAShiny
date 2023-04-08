library(stringr)
install.packages("rlist")
args <- commandArgs(trailingOnly = TRUE)

options(repos = 
        "https://packagemanager.rstudio.com/cran/__linux__/jammy/latest")

# Test if a package destination directory was passed as an argument
if (length(args) == 0) {
        stop("Usage: R -f download_packages.R destination_directory 
        package1,package2,package3", call. = FALSE)
}

# Create the directory
if (!dir.exists(args[1])) {
        message("Creating the directory")
        dir.create(args[1], recursive = TRUE)
}

# Split the packages into a character list
input_packs <- unlist(str_split(args[2], ","))

message(paste(input_packs), sep = " ")

plfrm <- paste(getRversion(), R.version["platform"],
        R.version["arch"], R.version["os"])

options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), plfrm))

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

# Install any GitHub packages which are not available in
# Posit Public Package Manager but some of their dependencies may be
github_packages_known_list <- list(c("TADA", "USEPA/TADA"))
gh_pkg_names <- sapply(github_packages_known_list, "[[", 1)
github_packages_to_build <- list()
for (p in intersect(input_packs, gh_pkg_names)) {
        gh_pkg <- github_packages_known_list[gh_pkg_names == p]
        message(paste("Github package", p, "found", sep = " "))
        deps <- pak::pkg_deps(gh_pkg[[1]][2])
        in_cran <- deps["ref"][deps["type"] == "standard"]
        gpk <- unlist(deps[deps["type"] == "github", c("package", "ref")])
        i <- 1
        while (i < length(gpk)) {
                github_packages_to_build <- rlist::list.append(
                        github_packages_to_build, c(gpk[i], gpk[i + 1]))
                i <- i + 2
        }
        input_packs <- union(input_packs, in_cran)
}

message(paste("Github packages needed are:",
        github_packages_to_build, sep = " "))

# Get the package dependencies from Posit Public Package Manager
packages <- get_packages(setdiff(input_packs, gh_pkg_names))

download_and_build_cran_package <- function(pack) {
        dir.create("junktemp")
        download.packages(c(pack), destdir = "junktemp")
        pkg <- list.files("junktemp")[1]
        devtools::build(pkg = paste("junktemp", pkg, sep = "/"),
                        path = args[1], binary = TRUE)
        unlink("junktemp", recursive = TRUE)
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

if ("rmarkdown" %in% packages) {
        cat("pandoc", file = "r-build-deps.txt", sep = "\n", append = TRUE)
}

message("Completed downloading packages")

download_github_package <- function(pack) {
        dir.create("junktemp")
        dl <- pak::pkg_download(pack[2], dest_dir = "junktemp",
                dependencies = FALSE,
                platforms = "source")
        if ("fulltarget_tree" %in% names(dl)) {
                print(dl$fulltarget_tree)
                dir.create("junktemp2")
                # fulltarget_tree has extension ".tar.gz-t"; switch to ".zip"
                no_exten <- str_sub(dl$fulltarget_tree, start = 1, end = -10)
                print(no_exten)
                as_zip <- paste(no_exten, ".zip", sep = "")
                print(as_zip)
                unzip(as_zip, list = TRUE)
                # untar(tarfile = no_dash_t, exdir = "junktemp2", list = TRUE)
                devtools::build(pkg = no_exten,
                        path = args[1], binary = TRUE)
                unlink("junktemp2", recursive = TRUE)
        } else {
                file.copy(dl$fulltarget, args[1])
        }
        unlink("junktemp", recursive = TRUE)
}

for (p in github_packages_to_build) {
        message(paste("Package", p[1], "must be built from GitHub", sep = " "))
        download_github_package(p)
}

tools::write_PACKAGES(dir = args[1], fields = NULL,
        type = c("source"),
        verbose = FALSE, unpacked = FALSE, subdirs = FALSE,
        latestOnly = TRUE, addFiles = FALSE, rds_compress = "xz",
        validate = FALSE)

message("Wrote package description files")