flex_ui <- function(which, session_data){
    return(switch(which,
        review = tagList(
            div(
                id = 'form',
                # Ligand/Xtal Select????
                textInput('name', 'Name', ''),
                selectInput('ligand', 'Ligand', selected='', choices = session_data$ligand_choices, multiple=FALSE),
                selectInput("decision", "Decision", choices =  c("", "Release", "More Refinement", "More Experiments", "Reject")),
                selectizeInput("reason", "Reason(s)", list(), multiple=TRUE),
                textInput('comments', 'Additional  Comments', value = "", width = NULL, placeholder = NULL),
                fluidRow(
                    column(6, actionButton('submit', 'Submit', class = 'btn-primary')),
                    column(6, actionButton('clear', 'Clear', class = 'btn-primary'))
                )
             )
        ),
        fragview = tagList(
            checkboxInput('desync', 'Turn off automatic Updates', value = FALSE),
            actionButton('goback', 'Prev Ligand'),
            actionButton('gonext', 'Next Ligand'),
            selectInput('goto', 'Go to Ligand', selected = '', choices = session_data$ligand_choices),
            textInput('crysname', 'Ligand Name', '' ),
            textInput('smiles', 'Smiles String', ''),
            textInput('new_smiles', 'New Smiles String', ''),
            textInput('alternate_name', 'Alternate Fragment Name', ''),
            selectizeInput('site_name', 'Site Label', list(), multiple=FALSE, options=list(create=TRUE)),
            textInput('pdb_entry', 'PDB Entry', ''),
            textOutput('metastatus'),
            uiOutput('writeButton'),
            actionButton('updateTable', 'Refresh Metadata Table')
        ),
        help = tagList(
        ),
        launchpad = tagList(
        ),
        summary = tagList(
        )
    ))
}