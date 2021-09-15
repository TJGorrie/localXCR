#' Normalise shinyFiles path
#'
#' Converts a shinyFiles path to an absolute path which should be valid given
#' a particular root
#' 
#' @param x The shinyFiles output
#' @param root The start of the file path
#' 
#' @import utils
#' @return Character String of the normalised path
normalise_path <- function(x, root){
    folder_path <- unlist(x)
    print(folder_path)
    trimmed_folder_path <- head(folder_path, length(folder_path) - 1)
    collapsed_folder_path <- paste(
        c(root[tail(folder_path, 1)], trimmed_folder_path),
        collapse = '/'
    )
    return(collapsed_folder_path)
}

#' Split a starting from the right
#' 
#' This is an R equivalent to the python str.rsplit method which will return a
#' vector of length n+1 of a string split by a given character starting from 
#' the right hand side of the string
#' 
#' @param string The string to split
#' @param split_by The character to split string by
#' @param n The number of split you want to perform
#' 
#' @return The string split from the right
#' 
#' @import utils
#' @export 
#' @examples 
#' rsplit(string = 'my_string_abc', split_by = '_', n = 1)
#' rsplit(string = 'my_string_abc', split_by = '_', n = 2)
rsplit <- function(string, split_by, n = 1){
    spl <- strsplit(string, split_by)[[1]]
    result <- c(
        paste(
            unlist(head(spl, length(spl) - n)),
            collapse = split_by
        ), 
        unlist(tail(spl, n))
    )
    return(result)
}

#' Create a DT Render from a reactive data object
#' 
#' Internal function which will update a given table using some optional
#' formatting - to be suppled to a output$table 
#' 
#' @param x data.frame inside a reactive environment
#' @param pl default row-numbers/pagelength of table
#' @param format Boolean, indicate whether special formatting rules should be applied
#' 
#' @return An render instance to be supplied to shiny output
#' 
#' @import DT
updateMainTable <- function(x, pl = 25, format = TRUE){
    if(format){
        DT::renderDataTable({
            DT::datatable(
                x(),
                selection = 'single',
                options = list(
                    pageLength = pl,
                    columnDefs = list(list(width = '100px', targets = c(4)))
                ), rownames= FALSE
            ) %>% DT::formatStyle(
                'decision_str',
                target = 'row',
                backgroundColor = DT::styleEqual(
                    c('Release', 'More Refinement', 
                    'More Experiments', 'Reject'),
                    c('#648FFF', '#FFB000',         
                    '#FE6100', '#DC267F')
                )
            ) %>% DT::formatStyle(
                columns = 1:ncol(x()),
                "white-space" = "nowrap"
            )
        })
    } else {
        DT::renderDataTable({
            DT::datatable(
                x(),
                selection = 'single',
                options = list(
                    pageLength = pl
                ), rownames= FALSE
            ) %>% DT::formatStyle(
                columns = 1:ncol(x()),
                "white-space" = "nowrap"
            )
        })
    }
}

#' Escape spaces in a string 
#' 
#' This function replaces all single white space characters with escaped 
#' versions (e.g. `\\ `)
#' 
#' @param x The string to remove spaces from
#' 
#' @return x but with `\\ ` instead of ` `
#' @export 
#' @examples 
#' removeSpaces('Remove Space')
removeSpaces <- function(x) return( gsub(' ', '\\ ', x, fixed = T) )

#' Convert a logical value into lower case characters for java-script
#' 
#' Converts a TRUE/FALSE value into 'true' or 'false', for some reason I need
#' this and others might too.
#' 
#' @param x a bool value
#' 
#' @return Either 'true' or 'false'
#' @export 
#' @examples 
#' tcl(TRUE)
#' tcl(FALSE)
tcl <- function(x) return( tolower(as.character(as.logical(x))) )

#' Get the file extension of a string (if any).
#' 
#' @param x A string
#' 
#' @return A string corresponding the file extension
#' @export 
#' @examples 
#' getExt('afile.txt')
#' @import utils
getExt <- function(x) return( sapply(strsplit(x, '[.]'), tail, 1) )

#' Return a valid filepath or a NA Value
#' 
#' @param path A string specifying a filepath to test
#' 
#' @return a string or NA value
#' @export 
#' @examples 
#' pathOrNA('NotAPath/NotAValidPath')
#' pathOrNA('./') # Valid
pathOrNA <- function(path){
    return( ifelse(is.na(path), NA, ifelse(file.exists(path), path, NA)) )
}

#' Get XCR specific defaults
#' 
#' @return A list of default values for fogging, clipping, boxsize, 
#'  clipDist, backgroundColor, cameraType and mousePreset
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

#' Get current values from shiny input object
#' 
#' @param input a Shiny input object usually within server
#' 
#' @return a List containing values from specified fields
getCurrentParams <- function(input){
    fields <- c('fogging', 'clipping', 'boxsize', 'clipDist', 
        'backgroundColor', 'cameraType', 'mousePreset')
    names(fields) <- fields
    return(lapply(reactiveValuesToList(input), '[[', fields))
}

#' Update NGL stage using updateaparam
#' 
#' @param session A shiny server session object
#' @param which which parameter to change
#' @param what what value to change to
#' 
#' @return Returns Nothing
updateParam <- function(session, which, what){
    session$sendCustomMessage(type = 'updateaparam', list(which, what))
}

#' Remove component from NGL stage
#' 
#' @param session A shiny server session object
#' @param objectname window.object to get rid of
#' 
#' @return Returns Nothing
removeNamedComponent <- function(session, objectname) {
    session$sendCustomMessage(type = 'removeNamedComponent', list(objectname))
}


#' Upload a PDB to NGL Stage
#' 
#' @param session A shiny server session object
#' @param filepath The file to upload
#' @param input A shiny server input object
#' 
#' @return Returns Nothing
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

#' Upload an apo PDB to NGL Stage
#' 
#' @param session A shiny server session object
#' @param filepath The file to upload
#' @param repr Representation for th structure (e.g. line or cartoon)
#' 
#' @return Returns Nothing
uploadApoPDB <- function(session, filepath, repr, focus){
    filepath <- removeSpaces(filepath)
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'setapoPDB',
        message = list(
            choice, #0
            repr, #1
            tcl(focus) #2
        )
    )
}

#' Upload a mol file to NGL Stage (and focus!)
#' 
#' @param session A shiny server session object
#' @param filepath The file to upload
#' @param ext The file extension
#' 
#' @return Returns Nothing
uploadMolAndFocus <- function(session, filepath, ext, focus){
    filepath <- removeSpaces(filepath)
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'addMolandfocus',
        list(choice,ext, tcl(focus))
    )
}

#' Upload a mol file and do something different to NGL Stage
#' 
#' @param session A shiny server session object
#' @param filepath The file to upload
#' 
#' @return Returns Nothing
uploadMF2 <- function(session, filepath){
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'fv_addMolandfocus',
        list(choice)
    )
}

#' Upload a mol file and not focus to NGL Stage
#' 
#' @param session A shiny server session object
#' @param filepath The file to upload
#' 
#' @return Returns Nothing
uploadUnfocussedMol <- function(session, filepath){
    filepath <- removeSpaces(filepath)
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'addMol',
        list(choice)
    )
}

#' Upload a mol file and do something different to NGL Stage
#' 
#' @param session A shiny server session object
#' @param filepath The file to upload
#' @param color string, colour of the map
#' @param negateiso bool, indicate whether or not iso should be negated
#' for fofc map files particularly
#' @param boxsize numeric, how large the map should render - 0 for max size
#' @param isolevel numeric, sigma contour level
#' @param visable bool, whether it should render or not (really?)
#' @param windowname string, name of the window object
#' 
#' @return Returns Nothing
#' @importFrom caTools base64encode
uploadVolumeDensity <- function(
    session, 
    filepath, 
    color, 
    negateiso = FALSE, 
    boxsize, 
    isolevel, 
    visable, 
    windowname,
    isotype = 'value'
    ){
    print(filepath)
    volume_bin <- readBin(filepath, what='raw', file.info(filepath)$size)
    volume_b64 <- caTools::base64encode(
        volume_bin, 
        size=NA,
        endian=.Platform$endian
    )
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
            as.character(windowname), #7
            as.character(isotype),#8
        )
    )
}

#' Toggle the visability of a map
#' 
#' @param session A shiny server session object
#' @param name name of the window to update
#' @param bool bool, Which state the update to
#' 
#' @return Returns Nothing
updateVisability <- function(session, name, bool){
    session$sendCustomMessage(
        type = 'updateVolumeDensityVisability',
        list(
            as.character(name),
            tcl(bool)
        )
    )
}

#' Update the ISO level of a map
#' 
#' @param session A shiny server session object
#' @param name name of the window to update
#' @param isolevel numeric, value to update iso level to
#' 
#' @return Returns Nothing
updateDensityISO <- function(session, name, isolevel){
    session$sendCustomMessage(
        type = 'updateVolumeDensityISO', 
        list(name, isolevel)
    )
}

#' Update the boxsize level of a map
#' 
#' @param session A shiny server session object
#' @param name name of the window to update
#' @param boxsize numeric, value to update boxsize to
#' 
#' @return Returns Nothing
updateDensityBoxSize <- function(session, name, boxsize){
    session$sendCustomMessage(
        type = 'updateVolumeDensityBoxSize', 
        list(name, boxsize)
    )
}

#' Create reactive environment - from reactive values
#' 
#' @param session_data Some reactive values
#' 
#' @return Return review table in reactive object
#' @import shiny
remapData <- function(session_data){
    reactive({
        cbind(
            'Ligand' = session_data$data$get_ligands, 
            session_data$data$get_reviews
        )
    })
}

#' Create another reactive environment - from reactive values
#' 
#' @param session_data Some reactive values
#' 
#' @return Return fragview table in reactive object
#' @import shiny
remapFragviewData <- function(session_data){
    reactive({
        dat <- session_data$data$get_metadata[,-1]
        rownames(dat) <- dat[,1]
        return(dat)
    })
}

#' Reset the review form
#' 
#' @param session Shiny Session object
#' @param session_data Some reactive values
#' 
#' @return Returns nothing
#' @import shiny
resetForm <- function(session, session_data){
    updateSelectizeInput(session, "ligand", selected = '', 
        choices = session_data$ligand_choices
    )
    updateSelectInput(session, 'decision', selected = '', 
        choices = c("", "Release", "More Refinement", 
            "More Experiments", "Reject")
    )
    updateSelectInput(session, 'reason', selected = '', choices = '')
    updateTextInput(session, 'comments', value = '')
}

#' Return a str or NA
#' 
#' Converts empty strings to NA otherwise return string as is
#' 
#' @param x The string to check
#' 
#' @return Returns x or NA
#' @export 
#' @examples 
#' strorNA('')
#' strorNA('lorem')
strorNA <- function(x){
    ifelse(x == '', NA, x)
}

#' Save review of ligand to filesystem
#' 
#' @param x Vector of review info...
#' @param z Atom crique
#' @param ligand ligand class object
#' 
#' @return Returns nothing but will edit a couple of files
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
            lines <- c(
                lines, 
                '> <BADATOMS>', 
                badatomstr, 
                '> <BADCOMMENTS>', 
                badcommentstr
            )
        }
        cat(paste(lines, collapse='\n'), file=ligand$mol_file)
    }
}


#' Upload a mol file and do something different to NGL Stage
#' 
#' @param session Shiny Session object
#' @param filepath The file to upload
#' 
#' @return Returns Nothing
uploadMolAndFocus2 <- function(session, filepath){
    syscall <- sprintf('cat %s', filepath)
    pdbstrings <- system(syscall, intern = TRUE)
    choice <- paste0(pdbstrings, collapse = '\n')
    session$sendCustomMessage(
        type = 'fv_addMolandfocus',
        list(choice)
    )
}

#' Create the folder one would need to upload to fragalysis
#' 
#' @param data an experiment object
#' @param copymaps bool, if you want to copy the maps
#' 
#' @return Creates some folders on the file system and return the filepath of the zipped file!
createFragUploadFolder <- function(data, copymaps=FALSE, usereviews=TRUE){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating Folder", value = 0)

    meta <- data$get_metadata
    colnames(meta) <- c('','crystal_name', 'RealCrystalName', 'smiles', 'new_smiles', 'alternate_name', 'site_name', 'pdb_entry')

    prot = data$target
    protsuffix <- paste(prot, format(Sys.time(), "%Y%m%d_%H%M"), sep='_')

    base_root <- data$filepath
    rootf <- file.path(dirname(base_root), protsuffix)
    basef <- file.path(rootf, prot)
    align_dir <- file.path(basef, 'aligned')
    crys_dir <- file.path(basef, 'crystallographic')

    system2('mkdir', rootf)
    system2('mkdir', basef)
    system2('mkdir', align_dir)
    system2('mkdir', crys_dir)


    # Filter out IGNORES and non Releases?
    keep_one <- !meta$site_name == 'IGNORE'
    keep_one[is.na(keep_one)] <- FALSE
    if(usereviews){
        keep_two <- data$get_status == 'Release'
    } else {
        keep_two <- rep(T, length(data$get_status))
    }
    meta <- meta[keep_one & keep_two, ]
    # aligned data copy
    progress$set(message = "Copying aligned Files", value = 0)
    increment = (1/nrow(meta))/1.1
    for(frag in meta$crystal_name){
        # For each ligand object...
        obj <- data$ligands[[frag]]
        progress$inc(increment, detail=frag)
        cf <- obj$aligned_loc
        nf <- paste(align_dir, frag, sep='/')
        files <- list.files(cf)
        if(!copymaps) files <- files[!grepl("(.map$|.ccp4$)", files)]
        system(sprintf('mkdir %s', nf))
        file.copy(file.path(cf,files), file.path(nf, files))
        # Then do the crystallographic?
        cf <- obj$crys_loc
        nf <- crys_dir
        files <- list.files(cf, pattern = rsplit(frag, '_', 1)[1])
        if(!copymaps) files <- files[!grepl("(.map$|.ccp4$)", files)]
        file.copy(file.path(cf,files), file.path(nf, files))
    }

    write.csv(meta, sprintf("%s/metadata.csv", basef), quote = FALSE)
    write.csv(meta, sprintf("%s/metadata.csv", align_dir), quote = FALSE)

    progress$set(message = "Zipping File!", value = .9)

    # Zip File
    zipf <- sprintf('%s.zip', prot)
    zipcommand <- sprintf("(cd %s && zip -r %s .)", rootf, prot)
    system(zipcommand)

    full_path_zipf <- sprintf('%s/%s', rootf, zipf)

    return(full_path_zipf)
}

#' Get residue list from pdb_file
#' 
#' @param pdb_file PDB file
#' 
#' @return Returns list of resiudes
#' @importFrom bio3d read.pdb
get_residues <- function(pdb_file){
    struc <- bio3d::read.pdb(pdb_file)
    c('', unique(paste(struc$atom$resid, struc$atom$resno, sep='_')))
}