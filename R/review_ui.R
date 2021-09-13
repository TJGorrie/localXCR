#' Generate Review UI components
#' 
#' @param session_data Some reactive values
#' 
#' @return Corresponding UI elements for Review
#' @import shiny nglShiny DT shinydashboard
#' @importFrom shinyWidgets chooseSliderSkin
review_ui <- function(session_data){
    components <- fluidPage(fluidRow(
        nglShiny::nglShinyOutput('nglShiny', height = '500px'),
        jqui_draggable(
            tabBox(
                tabPanel(
                    title = 'NGL Controls',
                    fluidRow(
                    column(6, actionButton(
                        "fitButton", 
                        "Center on Ligand"
                    )),
                    column(6,checkboxInput('autocenter', 'Automatically Center on load', value=TRUE))
                    ),
                    fluidRow(
                        shinyWidgets::chooseSliderSkin(
                            "Flat", 
                            color = '#112446'
                        ),
                        column(6,
                            fluidRow(
                                column(
                                    2, 
                                    checkboxInput(
                                        'eventMap', 
                                        'Show Event Map', 
                                        value = TRUE
                                    )
                                ),
                                column(10, 
                                    sliderInput(
                                        "isoEvent", 
                                        "", 
                                        min = 0, 
                                        max = 3, 
                                        value = 1, 
                                        step = 0.1
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    2, 
                                    checkboxInput(
                                        'twofofcMap', 
                                        'Show 2fofc Map', 
                                        value = TRUE
                                    )
                                ),
                                column(
                                    10, 
                                    sliderInput(
                                        "iso2fofc", 
                                        "", 
                                        min = 0, 
                                        max = 3, 
                                        value = 1.5, 
                                        step = 0.1
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    2, 
                                    checkboxInput(
                                        'fofcMap', 
                                        'Show fofc Map', 
                                        value = FALSE)
                                ),
                                column(
                                    10, 
                                    sliderInput(
                                        "isofofc", 
                                        "", 
                                        min = 0, 
                                        max = 3, 
                                        value = 3, 
                                        step = 0.1
                                    )
                                )
                            )
                        ),
                        column(
                            6,
                            radioButtons(
                                'views', 
                                'View Type', 
                                selected = 'aligned', 
                                inline = FALSE, 
                                width = NULL,
                                choiceNames = c(
                                    'Aligned (what will be in Fragalysis)', 
                                    'Raw Input Files (large map files)'
                                ),
                                choiceValues = c('aligned', 'crystallographic')
                            ),
                            selectInput(
                                'asuSwitch', 
                                'Assembly Type (Only in Raw)',
                                selected = 'AU', 
                                choices = c('AU', 'UNITCELL', 'SUPERCELL')
                            ),
                            selectInput(
                                'emap', 
                                'Select Eventmap', 
                                choices = '', 
                                multiple = FALSE
                            )
                        )
                    ),
                    fluidRow(
                        selectInput(
                            'gotores',
                            'Go to Residue:',
                            choices = '',
                            multiple = FALSE
                        ),
                        selectizeInput(
                            'highlight_res',
                            'Highlight Residues:',
                            choices = '',
                            multiple = TRUE
                        )
                    )
                ),
                tabPanel(
                    title = 'Atom Selection (Alt + Left Click)',
                    textOutput('as_message'),
                    fluidRow(
                        column(3, actionButton('as_clear', label = 'Clear Atoms')),
                        column(3, fluidRow(
                            actionButton('write_all', 'Write to All Atoms?', value=FALSE), 
                            actionButton('write_selected', label = 'Write to selected rows')
                            )
                        ),
                        column(3, selectizeInput('atom_text', 'Comment', choices=c('', 'Weak Density', 'No Density Evidence', 'Unexpected Atom', 'Multiple Conformations', options=list(create=TRUE))))
                    ),
                    DT::dataTableOutput('atoms')
                )
            ), options = list(delay = '1000', cancel = '.selectize-control')
        ),
        jqui_draggable(
            tabBox(
                tabPanel(
                    title = 'Review Table',
                    div(
                        style = 'overflow-y:scroll;height:600px;',
                        DT::DTOutput('reviewtable') # Great Name!
                    )
                )
            ), options = list(delay = '1000')
        )
    ))
    return(components)
}