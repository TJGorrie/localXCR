#' Create the UI components for FragView
#' 
#' @return UI Components for fragview
#' @import nglShiny DT shinydashboard
#' @importFrom shinyjqui jqui_draggable
fragview_ui <- function(){
    components <- fluidPage(    
        fluidRow(
            nglShinyOutput('FragViewnglShiny', height = '500px'),
            shinyjqui::jqui_draggable(tabBox(
                div(style='overflow-y:scroll', 
                DT::dataTableOutput('fragviewtable')), width=11))
        )
    )
    return(components)
}