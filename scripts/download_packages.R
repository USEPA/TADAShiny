library(stringr)

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

# Packages that should be pulled from GitHub
github_packages_list <- list(c("TADA", "USEPA/TADA"))

# Packages not available as binary from Posit
packages_needing_to_be_built <- c("sf")

# Get packages deps function to get the packages and dependencies
get_package_deps <- function(packs, github, to_build_cran) {
    refs <- packs
    message("Replacing names for items that should be pulled from GitHub")
    for (p in github) {
        if (p[1] %in% refs){
            refs[match(p[1], refs)] <- p[2]
        }
    }
    message("Getting the package dependencies")
    deps <- pak::pkg_deps(refs)
    deps["cran_build"] <- c(FALSE, TRUE)[mapply(`%in%`, deps["package"],
        to_build_cran) + 1]
    deps["require_build"] <- deps["type"] == "github" |
        deps["cran_build"] == TRUE
    deps
}

# Get the package dependencies
packages <- get_package_deps(input_packs, github_packages_list,
    packages_needing_to_be_built)

# Download the packages from the Posit repository
message(paste("Downloading the packages and dependencies to",
    args[1], sep = " "))
download.packages(packages["ref"][packages["require_build"] == FALSE],
    destdir = args[1])

message(paste("Github packages needed are:",
    packages["ref"][packages["type"] == "github"], sep = " "))

build_cran_package <- function(pack) {
        dir.create("junktemp")
        download.packages(c(pack), destdir = "junktemp")
        pkg <- list.files("junktemp")[1]
        devtools::build(pkg = paste("junktemp", pkg, sep = "/"),
                        path = args[1], binary = TRUE)
        unlink("junktemp", recursive = TRUE)
}

build_github_package <- function(pack) {
    print(paste("Pack is", pack, sep = " "))
    pak::pkg_install(pack)
    dir.create("junktemp")
    dl <- pak::pkg_download(pack, dest_dir = "junktemp",
        dependencies = FALSE,
        platforms = "source")
    dl <- head(dl, 1)
    print(paste("Fulltarget is", dl$fulltarget, sep = " "))
    print(paste("Fulltarget_tree is", dl$fulltarget_tree, sep = " "))
    if (file.exists(dl$fulltarget)) {
        devtools::build(dl$fulltarget, path = args[1], binary = TRUE)
    } else {
        print(dl$fulltarget_tree)
        dir.create("junktemp2")
        try(untar(dl$fulltarget_tree, exdir = "junktemp2"))
        try(unzip(dl$fulltarget_tree, exdir = "junktemp2"))
        f <- list.files(path = "junktemp2")
        print(f)
        devtools::build(pkg = paste("junktemp2", f[1], sep = "/"),
            path = args[1], binary = TRUE)
        unlink("junktemp2", recursive = TRUE)
    }
    unlink("junktemp", recursive = TRUE)
}

for (p in packages["ref"][packages["cran_build"] == TRUE]) {
    message(paste("Package", p, "must be built from CRAN", sep = " "))
    build_cran_package(p)
}

for (p in packages["ref"][packages["type"] == "github"]) {
    message(paste("Package", p, "must be built from GitHub", sep = " "))
    build_github_package(p)
}

message("Completed downloading packages")

tools::write_PACKAGES(dir = args[1], fields = NULL,
    type = c("source"),
    verbose = FALSE, unpacked = FALSE, subdirs = FALSE,
    latestOnly = TRUE, addFiles = FALSE, rds_compress = "xz",
    validate = FALSE)

message("Wrote package description files")