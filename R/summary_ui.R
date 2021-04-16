#' UI generator for first page
#' 
#' Contain the data importer too
#' 
#' @return Corresponding UI elements for sidebar
#' @import shiny shinydashboard
#' @importFrom shinyFiles shinyDirButton
summary_ui <- function(){
    components <- fluidPage(
        shinyFiles::shinyDirButton(
            id = 'summary_import_dir',
            label = 'Select a folder',
            title = 'Please select an input folder'
        ),
        textOutput('summary_import_dir_filepath'),
        # InfoBoxes
        fluidRow(
            infoBoxOutput('num_ligs'),
            infoBoxOutput('num_reviewed'),
            infoBoxOutput('num_annotated')
        )
    )
    return(components)
}
