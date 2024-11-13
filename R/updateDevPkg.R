getVersionLocal = function(pkg, field, lib.loc=NULL){
  # get DESCRIPTION metadata field from local library
  stopifnot(is.character(pkg), is.character(field), length(pkg)==1L, length(field)==1L)
  dcf = system.file("DESCRIPTION", package=pkg, lib.loc=lib.loc, mustWork=TRUE)
  if (nzchar(dcf)) read.dcf(dcf, fields=field)[1L] else NA_character_
}
# 

getVersionGithub = function(repo, field) {
  # get DESCRIPTION metadata field from remote PACKAGES file
  stopifnot(is.character(field), length(field)==1L, is.character(repo), length(repo)==1L, field!="Package")
  idx = file("https://raw.githubusercontent.com/robustport/PCRA/refs/heads/main/DESCRIPTION")
  on.exit(close(idx))
  read.dcf(idx, fields=field)
}


#' Update to Developer version on Github that have access to additional functions and data
#'
#' @param pkg Default to "PCRA" package name. 
#' @param repo Default to "https://github.com/robustport/PCRA"
#' @param field Default to "Version" field under Package Description file.
#' @param lib library path where the package would be installed.
#'
#' @return NULL
#' @export
#'
update_dev_pkg = function(pkg="PCRA", repo="https://github.com/robustport/PCRA",field= "Version", lib=NULL) {
  # this works for any package, not just data.table
  # perform package upgrade when new Revision present
  stopifnot(is.character(pkg), length(pkg)==1L, !is.na(pkg),
            is.character(repo), length(repo)==1L, !is.na(repo),
            is.character(field), length(field)==1L, !is.na(field),
            is.null(lib) || (is.character(lib) && length(lib)==1L && !is.na(lib)))
  # get Revision field from remote repository PACKAGES file
  
  if(!is.null(lib) && !lib%in%.libPaths())
    .libPaths(lib)
  
  una = is.na(ups<-.getVersionGithub(repo, field))
  if (una)
    catf("No revision information found in DESCRIPTION file for %s package. Make sure that '%s' is correct field in PACKAGES file in your package repository. Otherwise package will be re-installed every time, proceeding to installation.\n",
         pkg, field)
  # see if Revision is different then currently installed Revision, note that installed package will have Revision info only when it was installed from remote devel repo
  upg = una || !identical(ups,.getVersionLocal(pkg, field))
  # update_dev_pkg fails on windows R 4.0.0, we have to unload package namespace before installing new version #4403
  on.exit({
    if (upg) {
      unloadNamespace(pkg) ## hopefully will release dll lock on Windows
      devtools::install_github(repo=repo)
      msg_fmt = gettext("R %s package has been updated to %s (from %s)\n")
    } else {
      msg_fmt = gettext("R %s package is up-to-date at %s (from %s)\n")
    }
    field_val = unname(read.dcf(system.file("DESCRIPTION", package=pkg, lib.loc=lib, mustWork=TRUE), fields=field)[, field])
    cat(sprintf(msg_fmt, pkg, field_val, utils::packageVersion(pkg, lib.loc=lib)))
  })
  invisible(upg)
}