load_packages <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0) {
    cat("Installing:", paste(to_install, collapse = ", "), "\n")
    install.packages(to_install,
                     repos = "https://packagemanager.posit.co/cran/latest",
                     quiet = TRUE)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
  cat("Packages loaded.\n\n")
}
