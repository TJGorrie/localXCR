#' UI generator
#' 
#' @return UI components!
#' @import shinydashboard shiny
ui <- function(){
    header <- dashboardHeader(
        title = 'local XChemReview',
        tags$li(
            class = 'dropdown', 
            actionButton(
                'controls', 
                'Additional NGL Controls', 
                class = 'btn-primary', 
                icon = icon(
                    'cog', 
                    lib = 'glyphicon'
                )
            )
        )
    )

    sidebar <- dashboardSidebar(
        sidebarMenu(
            id = 'tab',
            menuItem(
                'Annotate', 
                tabName = 'fragview', 
                icon = icon('dashboard')
            ),
            #menuItem(
            #    'Summary', 
            #    tabName = 'summary', 
            #    icon = icon('th')
            #),
            menuItem(
                'Review', 
                tabName = 'review', 
                icon = icon('dashboard')
            ),
            menuItem(
                'Summary/Export', 
                tabName = 'launchpad', 
                icon = icon('th')
            ),
            menuItem(
                'Help', 
                tabName = 'help', 
                icon = icon('th')
            ),
            hr(),
            uiOutput('flex')
        )
    )

    body <- dashboardBody(
        tabItems(
            tabItem(
                tabName = 'review',
                uiOutput('review_ui')
            ),
            tabItem(
                tabName = 'help',
                includeMarkdown(system.file('extdata', 'help_pages.md', package = 'localXCR'))
            ),
            tabItem(
                tabName = 'summary',
                uiOutput('summary_ui')
            ),
            tabItem(
                tabName = 'fragview',
                uiOutput('fragview_ui')
            ),
            tabItem(
                tabName = 'launchpad',
                uiOutput('launch_pad_ui')
            )
        )
    )

    rendered_ui <- dashboardPage(header, sidebar, body)

    return(rendered_ui)
}