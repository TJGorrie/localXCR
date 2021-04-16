#' Flexible UI sidebar panel for XCR
#' 
#' @param which The name of the tab
#' @param session_data Some reactive values
#' 
#' @return Corresponding UI elements for sidebar
#' @import shiny
flex_ui <- function(which, session_data){
    return(switch(which,
        review = tagList(
            div(
                id = 'form',
                selectInput(
                    'ligand', 
                    'Ligand', 
                    selected = '', 
                    choices = session_data$ligand_choices,
                    multiple = FALSE),
                selectInput(
                    "decision", 
                    "Decision", 
                    choices =  c(
                        "", 
                        "Release", 
                        "More Refinement", 
                        "More Experiments", 
                        "Reject"
                    )
                ),
                selectizeInput(
                    "reason", 
                    "Reason(s)", 
                    list(), 
                    multiple = TRUE
                ),
                textInput(
                    'comments', 
                    'Additional  Comments', 
                    value = "", 
                    width = NULL, 
                    placeholder = NULL
                ),
                fluidRow(
                    column(
                        6, 
                        actionButton(
                            'submit', 
                            'Submit', 
                            class = 'btn-primary'
                        )
                    ),
                    column(
                        6,
                        actionButton(
                            'clear', 
                            'Clear', 
                            class = 'btn-primary'
                        )
                    )
                )
             )
        ),
        fragview = tagList(
            actionButton(
                'updateTable', 
                'Refresh Metadata Table'
            ),
            checkboxInput(
                'desync', 
                'Turn off automatic Updates', 
                value = FALSE
            ),
            textOutput('metastatus'),
            actionButton('write_fv', 'Write metadata to table'),
            fluidRow(
                column(4,actionButton('goback', 'Prev Ligand')),
                column(4,actionButton('gonext', 'Next Ligand'))
            ),
            selectInput(
                'goto', 
                'Go to Ligand', 
                selected = '', 
                choices = session_data$ligand_choices
            ),
            textInput('crysname', 'Ligand Name', '' ),
            textInput('smiles', 'Smiles String', ''),
            textInput('new_smiles', 'New Smiles String', ''),
            textInput('alternate_name', 'Alternate Fragment Name', ''),
            selectizeInput(
                'site_name', 
                'Site Label', 
                list(), 
                multiple = FALSE, 
                options = list(create = TRUE)
            ),
            textInput('pdb_entry', 'PDB Entry', '')
        ),
        help = tagList(
        ),
        launchpad = tagList(
        ),
        summary = tagList(
        )
    ))
}