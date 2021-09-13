#' Create LaunchPad UI for XCR
#' 
#' @return UI Components for Launchpad Tab in XCR
#' @import shiny
launch_pad_ui <- function(){
    components <- fluidPage(
        checkboxInput('copymaps', 'Copy Map Files', value = FALSE),
        checkboxInput('usereviews', 'Only copied reviewed ligands', value = TRUE),
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
