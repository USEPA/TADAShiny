library(stringr)

args <- commandArgs(trailingOnly = TRUE)

# Test if a package destination directory was passed as an argument
if (length(args) == 0) {
        stop("Usage: R -f download_packages.R destination_directory 
        package1,package2,package3", call. = FALSE)
}

# Split the packages into a character list
input_packs <- unlist(str_split(args[2], ","))

message(paste(input_packs), sep = " ")

deps <- pak::pkg_deps(input_packs)

for (row in 1:nrow(deps)) {
        dl <- pak::pkg_download(deps[row, "ref"], dest_dir = args[1],
                dependencies = FALSE)
        if ("fulltarget_tree" %in% names(dl)) {
                # Indicates package needs to be built
                print(dl$fulltarget_tree)
                dir.create("junktemp")
                unzip(dl$fulltarget_tree, exdir = "junktemp")
                f <- list.files(path = "junktemp")
                print(f)
                devtools::build(pkg = paste("junktemp", f[1], sep = "/"),
                        path = args[1], binary = TRUE)
                unlink("junktemp", recursive = TRUE)
                file.remove(dl$fulltarget_tree)
        }
}