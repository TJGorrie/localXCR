review_ui <- function(session_data){
    components <- fluidPage(fluidRow(
        nglShiny::nglShinyOutput('nglShiny', height = '500px'),
        jqui_draggable(
            tabBox(
                tabPanel(
                    title = 'NGL Controls',
                    actionButton(
                        "fitButton", 
                        "Center on Ligand"
                    ),
                    fluidRow(
                        chooseSliderSkin(
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
                    )
                ),
                #tabPanel(
                #    title = 'Ligand Information',
                #    div(
                #        style='overflow-y:scroll;height:600px;',
                #        fluidRow(
                #            column(
                #                8,
                #                column(
                #                    imageOutput('ligimage')
                #                    6, 
                #                ),
                #                column(
                #                    6,
                #                    imageOutput('spiderPlot')
                #                )
                #            ),
                #            column(
                #                4,
                #                div(
                #                    style = "margin-top:-1em", 
                #                        'renderMisc', 
                #                    checkboxInput(
                #                        'Render Image/Spider Plot', 
                #                        value = TRUE, 
                #                        width = NULL
                #                    )
                #                ),
                #                div(
                #                    style = "margin-top:-1em", 
                #                    selectInput(
                #                        'emap', 
                #                        'Select Eventmap', 
                #                        choices = '', 
                #                        multiple = FALSE
                #                    )
                #                ),
                #            )
                #        ),
                #        column(
                #            12,
                #            div(
                #                style = "margin-top:-15em",
                #                    fluidRow(
                #                        uiOutput('plotElement')
                #                    )
                #            )
                #        )
                #    ),
                #),
                tabPanel(
                    title = 'Atom Selection (Alt + Left Click)',
                    textOutput('as_message'),
                    actionButton('as_clear', label = 'Clear all selected atoms'),
                    DT::dataTableOutput('atoms')
                )
            ), options = list(delay = '1000')
        ),
        jqui_draggable(
            tabBox(
                tabPanel(
                    title = 'Review Table',
                    div(
                        style = 'overflow-y:scroll;height:600px;',
                        DT::DTOutput('reviewtable') # Great Name!
                    )
                )#,
                #tabPanel(
                #    title = 'Review Plots (Click points to load ligand)',
                #        fluidRow(
                #            column(
                #                4,
                #                selectInput(
                #                    'fpex', 
                #                    'x', 
                #                    selected = 'res', 
                #                    choices = c(
                #                       'res', 
                #                       'r_free', 
                #                       'rcryst', 
                #                       'ramachandran_outliers', 
                #                       'rmsd_angles', 
                #                       'rmsd_bonds'
                #                    )
                #                )
                #            ),
                #            column(
                #                4,
                #                selectInput(
                #                    'fpey', 
                #                    'y', 
                #                    selected = 'r_free', 
                #                    choices = c(
                #                        'res', 
                #                        'r_free', 
                #                        'rcryst', 
                #                        'ramachandran_outliers', 
                #                        'rmsd_angles', 
                #                        'rmsd_bonds'
                #                    )
                #                )
                #            ),
                #            column(
                #                4,
                #                selectInput(
                #                    'fpe_target', 
                #                    'target', 
                #                    selected = '', 
                #                    choices = c(
                #                        'A', 
                #                        'B', 
                #                        'C'
                #                    )
                #                )
                #            )
                #        ),
                #        verbatimTextOutput("info"),
                #        uiOutput('flexPlotElement')
                #)
            ), options = list(delay = '1000')
        )
    ))
    return(components)
}