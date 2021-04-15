#' Normalise shinyFiles path
#'
#' @import utils
#' @return Character Vector of normalised path
normalise_path <- function(
    x,
    root = getwd()
    ){
    folder_path <- unlist(x)
    trimmed_folder_path <- head(folder_path, length(folder_path) - 1)
    collapsed_folder_path <- paste(
        c(root, trimmed_folder_path),
        collapse = '/'
    )
    return(collapsed_folder_path)
}

rsplit <- function(string, split_by, n=1){
    spl <- strsplit(string, split_by)[[1]]
    result <- c(
        paste(
            unlist(head(spl, length(spl)-n)),
            collapse = split_by
        ), 
        unlist(tail(spl, n))
    )
    return(result)
}

updateMainTable <- function(x, pl=25, format=TRUE){
    if(format){
        DT::renderDataTable({
            DT::datatable(
                x(),
                selection = 'single',
                options = list(
                    pageLength = pl,
                    columnDefs = list(list(width='100px', targets=c(4)))
                ), rownames= FALSE
            ) %>% DT::formatStyle(
                'decision_str',
                target = 'row',
                backgroundColor = DT::styleEqual(
                    c('Release', 'More Refinement', 'More Experiments', 'Reject'),
                    c('#648FFF', '#FFB000',         '#FE6100',          '#DC267F')
                )
            ) %>% DT::formatStyle(columns = 1:ncol(x()),"white-space"="nowrap")
        })
    } else {
        DT::renderDataTable({
            DT::datatable(
                x(),
                selection = 'single',
                options = list(
                    pageLength = pl
                ), rownames= FALSE
            ) %>% DT::formatStyle(columns = 1:ncol(x()),"white-space"="nowrap")
        })
    }
}




removeSpaces <- function(x) return(gsub(' ', '\\ ', x, fixed=T))

tcl <- function(x) return(tolower(as.character(as.logical(x))))

getExt <- function(x) return(sapply(strsplit(x, '[.]'), tail, 1))

pathOrNA <- function(path){
    return(ifelse(is.na(path), NA, ifelse(file.exists(path), path, NA)))
}

# Default Values for Control panel trick
loadDefaultParams <- function(){
    defaults <- list(
        fogging = c(49, 63),
        clipping = c(49, 55),
        boxsize = 5,
        clipDist = 5,
        backgroundColor = 'black',
        cameraType = 'orthographic',
        mousePreset = 'default'
    )
    return(defaults)
}

# Reads whatever the current input values are...
getCurrentParams <- function(input){
    fields <- c('fogging', 'clipping', 'boxsize', 'clipDist', 
        'backgroundColor', 'cameraType', 'mousePreset')
    names(fields) <- fields
    return(lapply(reactiveValuesToList(input), '[[', fields))
}

updateParam <- function(session, which, what){
    session$sendCustomMessage('updateaparam', list(which, what)) # Does not return anything...
}


removeNamedComponent <- function(session, objectname) session$sendCustomMessage(type='removeNamedComponent', list(objectname))

uploadPDB <- function(session, filepath, input){
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'setPDB2', # See TJGorrie/NGLShiny for details on setPDB2
        message = list(
            choice,
            isolate(input$clipDist),
            isolate(input$clipping)[1],
            isolate(input$clipping)[2],
            isolate(input$fogging)[1],
            isolate(input$fogging)[2]
        )
    )
}

uploadApoPDB <- function(session, filepath, repr){
    filepath <- removeSpaces(filepath)
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'setapoPDB',
        message = list(
            choice,
            repr
        )
    )
}

uploadMolAndFocus <- function(session, filepath, ext){
    filepath <- removeSpaces(filepath)
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'addMolandfocus',
        list(choice,ext)
    )
}

uploadMF2 <- function(session, filepath){
        syscall <- sprintf('cat %s', filepath)
        pdbstrings <- system(syscall, intern = TRUE)
        choice <- paste0(pdbstrings, collapse = '\n')
        session$sendCustomMessage(
            type = 'fv_addMolandfocus',
            list(choice)
        )
    }

uploadUnfocussedMol <- function(session, filepath){
    filepath <- removeSpaces(filepath)
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type='addMol',
        list(choice)
    )
}

uploadVolumeDensity <- function(session, filepath, color, negateiso = FALSE, boxsize, isolevel, visable, windowname){
    volume_bin <- readBin(filepath, what='raw', file.info(filepath)$size)
    volume_b64 <- caTools::base64encode(volume_bin, size=NA, endian=.Platform$endian)
    session$sendCustomMessage(
        type = 'addVolumeDensity',
        message = list(
            as.character(volume_b64), #0
            as.character(isolevel), #1
            as.character(color), #2
            tcl(negateiso), #3
            as.character(getExt(filepath)), #4
            as.character(boxsize), #5
            tcl(visable), #6
            as.character(windowname) #7
        )
    )
}

# Control how volume densities get toggled.
updateVisability <- function(session, name, bool){
    session$sendCustomMessage(
        type = 'updateVolumeDensityVisability',
        list(
            as.character(name),
            tcl(bool)
        )
    )
}

updateDensityISO <- function(session, name, isolevel) session$sendCustomMessage('updateVolumeDensityISO', list(name, isolevel))
updateDensityBoxSize <- function(session, name, boxsize) session$sendCustomMessage('updateVolumeDensityBoxSize', list(name, boxsize))

remapData <- function(session_data){
    reactive({
        cbind('Ligand' = session_data$data$get_ligands, session_data$data$get_reviews)
    })
}

remapFragviewData <- function(session_data){
    reactive({
        dat <- session_data$data$get_metadata[,-1]
        rownames(dat) <- dat[,1]
        return(dat)
    })
}

resetForm <- function(session, session_data){
    updateSelectizeInput(session, "ligand", selected = '', choices = session_data$ligand_choices)
    updateSelectInput(session, 'decision', selected ='', choices = c("", "Release", "More Refinement", "More Experiments", "Reject"))
    updateSelectInput(session, 'reason', selected='', choices='')
    updateTextInput(session, 'comments', value = "")
}

strorNA <- function(x){
    ifelse(x == '', NA, x)
}

saveReview <- function(x,z, ligand){
    badatomstr <- strorNA(paste(z[,'index'], collapse=';'))
    badcommentstr <- strorNA(paste(z[,'comment'], collapse=';'))
    ligand$review <- c(x[[1]], x[[2]], x[[3]], badatomstr, badcommentstr)
    # Edit the mol files...
    if(!is.na(badatomstr)){
        lines <- readLines(ligand$mol_file)
        if(any(lines == '> <BADATOMS>')){
            badid_line <- which(lines == '> < BADATOMS>') + 1
            badcomment_line <- which(lines == '> <BADCOMMENTS>') + 1
            lines[badid_line] <- badatomstr
            lines[badcomment_line] <- badcommentstr
        } else {
            lines <- c(lines, '> <BADATOMS>', badatomstr, '> <BADCOMMENTS>', badcommentstr)
        }
        cat(paste(lines, collapse='\n'), file=ligand$mol_file)
    }
}

uploadMolAndFocus2 <- function(filepath){
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'fv_addMolandfocus',
        list(choice)
    )
}