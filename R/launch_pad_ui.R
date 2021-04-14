launch_pad_ui <- function(){
    components <- fluidPage(
        selectInput(
            'lp_selection', 
            'Select Target', 
            selected = '', 
            choices = c('A', 'B', 'C') #fragfolders
        ),
        actionButton(
            'lp_launcher', 
            "Compile Files"
        ),
        downloadButton(
            "lp_download", 
            "Download File"
        )
    )
    return(components)
}        
