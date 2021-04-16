#' Create LaunchPad UI for XCR
#' 
#' @return UI Components for Launchpad Tab in XCR
#' @import shiny
launch_pad_ui <- function(){
    components <- fluidPage(
        #selectInput(
        #    'lp_selection', 
        #    'Select Target', 
        #    selected = '', 
        #    choices = c('Not yet implemented...')
        #),
        actionButton(
            'lp_launcher', 
            "Compile Files"
        ),
        downloadButton(
            "lp_download", 
            "Download Files as Zip!"
        )
    )
    return(components)
}        
