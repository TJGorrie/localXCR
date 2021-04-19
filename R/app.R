#' Launch local XCR instance
#'
#' @param host The IP address. default is '0.0.0.0'
#' @param port The port to host app from
#' @param launch.browser Logical, indicate whether or not the default browser
#'  should open
#' @importFrom shiny shinyApp runApp
#' @return Runs the XCR shiny applet.
#' @examples
#' \dontrun{
#' launch_XCR()
#' }
#' @export
launch_XCR <- function(host = '0.0.0.0', port = 8000, launch.browser=TRUE){
    app <- shiny::shinyApp(ui = ui(), server = server)
    shiny::runApp(
        app,
        host = host,
        port = as.numeric(port),
        launch.browser = launch.browser
    )
}