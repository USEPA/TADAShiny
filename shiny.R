library_path <- paste("Library Path: ", Sys.getenv(c("LD_LIBRARY_PATH")))
print(paste("LD_LIBRARY_PATH: ", library_path))

lib_dir <- "/home/vcap/deps/0/r/lib"
local_lib_dir <- "r-lib"
local_bin_dir <- "r-bin"

if (dir.exists(lib_dir)) {
  if (dir.exists(local_lib_dir)) {
    # Get the list of libs
    lib_tars <- list.files(local_lib_dir)
    lib_tars <- paste(local_lib_dir, lib_tars, sep="/")

    print(paste("Local libs: ", lib_tars))
    print(paste("Working directory: ", list.files(getwd())))

    # Copy the files to the lib_dir
    for(i in 1:length(lib_tars)) {
      untar(lib_tars[i], exdir = lib_dir)
    }

    Sys.setenv(PROJ_LIB=lib_dir)

  }
  if (dir.exists(local_bin_dir)) {
    Sys.setenv(RMARKDOWN_PANDOC = local_bin_dir)
    Sys.setenv(PATH=paste("/home/vcap/app/", local_bin_dir, ":${PATH}", sep = ""))
  }
  print(list.files(lib_dir))
}

library(shiny)
runApp(appDir="shiny-app", host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))
