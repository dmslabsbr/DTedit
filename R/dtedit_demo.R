#' Run a shiny app showing how the DTedit function works.
#'
#' @export
dtedit_demo <- function() {
	dir <- paste0(find.package('DTedit'), '/shiny_demo/app.R')
	message(paste0("Running shiny app from ", dir))
	shiny::runApp(appDir = dir)
}


#' Run a shiny app showing how the DTedit function works.
#'
#' @export
dtedit_demo2 <- function() {
  dir <- paste0(find.package('DTedit'), '/shiny_demo/app-tst2.R')
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}