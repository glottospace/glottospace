#' Local directory with glottospace package files
#'
#' Loads path to local directory where glottospace files are stored and creates directory in a platform independent way in case it doesn't exist.
#'
#' @noRd
glottofiles_cachedir <- function(){
  cachedir <- base::normalizePath(rappdirs::user_data_dir("glottospace"), winslash = "\\", mustWork = FALSE)
  if(!base::dir.exists(cachedir)){
    base::dir.create(cachedir, recursive = TRUE)
  }
  cachedir
}

#' Create local path for a file that is stored in glottospace cachedir
#'
#' @param filename filename
#' @noRd
#'
glottofiles_makepath <- function(filename){
  filedir <- glottofiles_cachedir()
  filepath <- base::file.path(filedir, filename)
  normalizePath(filepath, winslash = "\\", mustWork = FALSE)
  # pkgfilecache::are_files_available(pkgfilecache::get_pkg_info("glottospace"), "something.gpkg")
}

#' Create local directory for a dirname
#'
#' @param dirname dirname
#' @noRd
#'
glottofiles_makedir <- function(dirname){
  filedir <- glottofiles_cachedir()
  dirpath <- base::file.path(filedir, dirname)
  dirpath <- normalizePath(dirpath, winslash = "\\", mustWork = FALSE)
  if(!base::dir.exists(dirpath)){
    base::dir.create(dirpath, recursive = TRUE)
  }
  dirpath
}
