controlPanelModal <- function(values, title){
    # Function that opens up a modal dialog to contain accessory ngl controls that are not needed to be accessed immediately.
    draggableModalDialog(
        title=title,
        numericInput('boxsize', 'Box Size', value = values$boxsize, min = 0, max = 100, width = '100px'),
        numericInput('clipDist', 'Clipping Distance', value = values$clipDist, min = 0, max = 100, width = '100px'),
        sliderInput('fogging', 'Fogging:', min = 0, max = 100, value = values$fogging),
        sliderInput('clipping', 'Clipping:', min = 0, max = 100, value = values$clipping),
        selectInput('backgroundColor', 'Background Colour', selected = values$backgroundColor, choices = c('black', 'white')),
        selectInput('cameraType', 'Camera Type', selected = values$cameraType, choices = c('orthographic', 'perspective')),
        selectInput('mousePreset', 'Mouse Preset', selected = values$mousePreset, choices = c('default', 'coot', 'pymol')),
        easyClose = FALSE,
        footer = tagList(actionButton('updateParams', 'Update Controls'))
    )
}