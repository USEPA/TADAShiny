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

pak::pkg_install(input_packs, lib = args[1])