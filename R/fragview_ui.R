fragview_ui <- function(){
    components <- fluidPage(    
        fluidRow(
            nglShinyOutput(
                'FragViewnglShiny', 
                height = '500px'
            ),
            jqui_draggable(
                tabBox(
                    div(
                        style = 'overflow-y:scroll;height:600px;',
                        DT::dataTableOutput('fragview')
                    )
                )
            )
        )
    )
    return(components)
}