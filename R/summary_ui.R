summary_ui <- function(){
    components <- fluidPage(
        # Button to select a folder... Chosen folder should have a aligned and crystallographic folder.
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
