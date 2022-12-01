## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",  
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  no_dev <- function(path, package_name, ...){
#     fs::dir_delete("dev")
#  }
#  create_golem("ici", project_hook = no_dev)

## -----------------------------------------------------------------------------
#  new_css <- function(path, package_name, ...){
#  
#    css_path <- fs::path_abs("inst/app/www/custom.css")
#  
#    fs::file_create(css_path)
#  
#    write_there <- function(...){
#      write(..., file = css_path, append = TRUE)
#    }
#  
#    write_there("body {")
#    write_there("    background-color:red;")
#    write_there("}")
#  
#    cli::cat_bullet("CSS generated")
#  
#  }
#  
#  create_golem("ici", project_hook = new_css)

## -----------------------------------------------------------------------------
#  my_tmpl <- function(name, path, export, ...){
#     # Define a template that only write the name of the
#     # module in the file
#     write(name, path)
#  }
#  golem::add_module(name = "custom", module_template = my_tmpl)
#  
#  my_other_tmpl <- function(name, path, ...){
#     # Copy and paste a file from somewhere else
#     file.copy(..., path)
#  }
#  golem::add_module(name = "custom", module_template = my_other_tmpl)

## -----------------------------------------------------------------------------
#  my_tmpl <- function(path, ...){
#     # Define a template that only write the name of the
#     # module in the file
#    write_there <- function(...){
#      write(..., file = path, append = TRUE)
#    }
#  
#    write_there("body {")
#    write_there("    background-color:red;")
#    write_there("}")
#  }
#  golem::add_css_file(name = "custom", template = my_tmpl)

