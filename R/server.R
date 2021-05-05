#' XCR Server
#' 
#' @param input nothing really
#' @param output nothing really
#' @param session nothing really
#' 
#' @return it's the thing that makes this go
#' @import shiny nglShiny
server <- function(input, output, session){
    # Cheeky Defaults that I need to encapsulate...
    possDec <- c("", "Release", "More Refinement", "More Experiments", "Reject")
    possAns <- possAns2 <- c('Select Decision')
    possRes <- list()
    possRes[['Release']] <- c('High Confidence', 'Clear Density, Unexpected Ligand', 'Correct Ligand, Weak Density', 'Low Confidence', 'No Ligand Present')
    possRes[["More Refinement"]] <- c('Check ligand conformation',
        'Check sidechain rotamers',
        'Check Rfactors',
        'Check that refinement converged',
        'Improve water model',
        'Build alternate conformations',
        'Fix geometry',
        'Trim down ligand',
        'Density did not load',
        'Other (specify in comments)')
    possRes[["More Experiments"]] <- c('Get better density',
        'Get better resolution',
        'Confirm ligand identity',
        'Check if ligand changed',
        'Other (specify in comments)')
    possRes[["Reject"]] <- c('Density not convincing',
        'Too few interactions',
        'Binding site too noisy',
        'Not the ligand',
        'Other (specify in comments)')
    possDec_int <- 1:4
    names(possDec_int) <- c("Release", "More Refinement", "More Experiments", "Reject")

    # On session init, set control panel values to defaults.
    ngl_control_values <- reactiveValues()
    ngl_control_values$defaults <- loadDefaultParams()

    observeEvent(input$updateParams, {
        removeModal()
        for(i in names(ngl_control_values$defaults)){
            ngl_control_values$defaults[[i]] <- input[[i]]
        }
    })

    observeEvent(input$backgroundColor, { updateParam(session=session, 'backgroundColor', as.character(input$backgroundColor)) })
    observeEvent(input$cameraType, { updateParam(session=session, 'cameraType', as.character(input$cameraType)) })
    observeEvent(input$mousePreset, { updateParam(session=session, 'mousePreset', as.character(input$mousePreset)) })
    observeEvent(input$clipDist, { updateParam(session=session, 'clipDist', as.character(input$clipDist)) })
    observeEvent(input$fogging, {
        updateParam(session=session, 'fogNear', as.character(input$fogging[1]) )
        updateParam(session=session,'fogFar' , as.character(input$fogging[2]) )
    })
    observeEvent(input$clipping, {
        updateParam(session=session,'clipNear', as.character(input$clipping[1]) )
        updateParam(session=session,'clipFar' , as.character(input$clipping[2]) )
    })


    session_data <- reactiveValues()
    session_data$selected <- ''
    session_data$ligand_choices <- ''

    n_ligands <- reactive({
        n_ligs(session_data$data)
    })
    n_reviews <- reactive({
        n_revi(session_data$data)
    })
    n_annotated <- reactive({
        n_anno(session_data$data)
    })
    # UI BITS
    output$summary_ui <- renderUI({summary_ui()})
    output$review_ui <- renderUI({review_ui()})
    output$fragview_ui <- renderUI({fragview_ui()})
    output$launch_pad_ui <- renderUI({launch_pad_ui()})
    output$flex <- renderUI({flex_ui(which = input$tab, session_data=session_data)})
    
    # NGL STAGE
    output$nglShiny <- nglShiny::renderNglShiny(
        nglShiny::nglShiny(name = 'nglShiny', list(), width = NULL, height = NULL)
    )
    output$FragViewnglShiny <- nglShiny::renderNglShiny(
        nglShiny::nglShiny(name = 'nglShiny', list(), width=NULL, height=100)
    )

    # Map Listeners
    observeEvent(input$eventMap,   { 
        updateVisability(session = session, name = 'eventmap', bool = input$eventMap  ) 
    })
    observeEvent(input$twofofcMap, { 
        updateVisability(session = session, name = 'twofofc' , bool = input$twofofcMap) 
    })
    observeEvent(input$fofcMap,    {
        updateVisability(session = session, name = 'fofcpos', bool = input$fofcMap)
        updateVisability(session = session, name = 'fofcneg', bool = input$fofcMap)
    })

    observeEvent(input$isoEvent, {
        updateDensityISO(session = session, name = 'eventmap', isolevel = input$isoEvent)
    })
    observeEvent(input$iso2fofc, {
        updateDensityISO(session = session, name = 'twofofc', isolevel = input$iso2fofc)
    })
    observeEvent(input$isofofc , {
        updateDensityISO(session = session, name = 'fofcpos', isolevel = input$isofofc)
        updateDensityISO(session = session, name = 'fofcneg', isolevel = input$isofofc)
    })

    observeEvent(input$boxsize , {
        for(windowname in c('eventmap', 'twofofc', 'fofcpos', 'fofcneg')){
            updateDensityBoxSize(session = session, name = windowname, boxsize = input$boxsize)
        } 
    })


    observeEvent(input$asuSwitch, {
        try(session$sendCustomMessage('updateAssembly', list(isolate(input$asuSwitch))))
    })

    output$isoEventSlider <- renderUI({
            sliderInput("isoEvent", "",
                    min = 0, max = 3,
                    value = 1, step = 0.1)
    })

    output$iso2fofcSlider <- renderUI({
            sliderInput("iso2fofc", "",
                    min = 0, max = 3,
                    value = 1.5, step = 0.1)
    })

    output$isofofcSlider <- renderUI({
            sliderInput("isofofc", "",
                min = 0, max = 3,
                value = 3, step = 0.1)
    })

    # Shiny Files cludge for windows?? This could be improved in general I think.
    roots = c(root = '/')
    osSystem <- Sys.info()['sysname']
    windows = FALSE
    if(osSystem == 'Windows') {
        windows = TRUE
        wmic <- paste0(Sys.getenv('SystemRoot'), '\\System32\\Wbem\\WMIC.exe')
        if(!file.exists(wmic)){
            roots = c(root = '.')
        } else {
            volumes <- system(sprintf("%s logicaldisk get Caption", wmic), intern = TRUE)
            volumes <- sub(" *\\r$", "", volumes)
            keep <- !tolower(volumes) %in% c('caption', '')
            volumes <- volumes[keep]
            volNames <- system(sprintf("%s logicaldisk get VolumeName", wmic), intern = TRUE)
            volNames <- sub(" *\\r$", "", volNames)
            volNames <- volNames[keep]
            volNames <- paste0(volNames, ifelse(volNames == "", "", " "))
            volNames <- sprintf("%s(%s)", volNames, volumes)
            names(volumes) <- volNames
            roots <- gsub(":$", ":/", volumes)
        }
    }

    shinyFiles::shinyDirChoose(
        input, 
        'summary_import_dir', 
        roots = roots,
        filetypes = c('', '.pdb', '.mol', '.ccp4', '.map', '.txt')
    )

    observeEvent(input$summary_import_dir, {
        print(isolate(input$summary_import_dir))
        path <- normalise_path(x = isolate(input$summary_import_dir), root = roots)
        output$summary_import_dir_filepath <- renderText({
            path
        })
        if(all(c('aligned', 'crystallographic') %in% dir(path))){
            session_data$data <- createExperiment(path)
            session_data$ligand_choices <- c('', sort(names(session_data$data$ligands)))
            output$num_ligs <- renderInfoBox({
                infoBox('Ligands', n_ligands(), icon = icon('thumbs-up', lib = 'glyphicon'), color = 'red')
            })
            output$num_reviewed <- renderInfoBox({
                infoBox('Reviewed', n_reviews(), icon = icon('thumbs-up', lib = 'glyphicon'), color = 'red')
            })
            output$num_annotated <- renderInfoBox({
                infoBox('Annotated', n_annotated(), icon = icon('thumbs-up', lib = 'glyphicon'), color = 'red')
            })
        }
        f1 <- remapFragviewData(session_data = session_data)
        output$fragviewtable <- updateMainTable(x=f1, pl=100, format=FALSE)
    })

    r1 <- remapData(session_data = session_data)
    f1 <- remapFragviewData(session_data = session_data)

    output$reviewtable <- updateMainTable(x=r1)
    output$fragviewtable <- updateMainTable(x=f1, pl=100, format=FALSE)

    atomstoquery <- reactiveValues()
    atomstoquery$data <- data.frame(name=character(),
                 index=character(),
                 comment=character(),
                 stringsAsFactors=FALSE)

    output$atoms <- DT::renderDataTable({
        DT::datatable(atomstoquery$data, options = list(autoWidth = TRUE, columnDefs = list(list(width='50px', targets=c(1,2)))))
        }
    )
    observeEvent(input$clickedAtoms, {
        newdat <- isolate(atomstoquery$data)
        # Check for 'new' rows:
        new <- which(!as.character(input$clickNames) %in% as.character(newdat$name))
        for(i in new){
            newdat <- rbind(newdat, data.frame(name = input$clickNames[i], index = input$clickedAtoms[i], comment = '', stringsAsFactors=FALSE))
        }
        tokeep <- as.character(newdat$name) %in% as.character(input$clickNames)
        newdat <- newdat[tokeep,]
        atomstoquery$data <- newdat
        output$atoms <- DT::renderDataTable({DT::datatable(atomstoquery$data, editable = list(target = 'cell', disable = list(columns = c(1,2))), options = list(autoWidth = TRUE, columnDefs = list(list(width='50px', targets=c(1,2)))))})
    })

    observeEvent(input$atoms_cell_edit, {
        info = input$atoms_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        update <- isolate(atomstoquery$data)
        update[i, j] <- as.character(v)
        atomstoquery$data <- update
        output$atoms <- DT::renderDataTable({DT::datatable(atomstoquery$data, editable = list(target = 'cell', disable = list(columns = c(1,2))), options = list(autoWidth = TRUE, columnDefs = list(list(width='50px', targets=c(1,2)))))})
    })

    observeEvent(input$write_selected, {
        idx <- isolate(input$atoms_rows_selected)
        update <- isolate(atomstoquery$data)
        if(input$write_all) idx <- 1:nrow(update)
        update[idx, 3] <- as.character(input$atom_text)
        atomstoquery$data <- update
        output$atoms <- DT::renderDataTable({DT::datatable(atomstoquery$data, editable = list(target = 'cell', disable = list(columns = c(1,2))), options = list(autoWidth = TRUE, columnDefs = list(list(width='50px', targets=c(1,2)))))})
    })


    observeEvent(input$reviewtable_rows_selected, {
        id <- input$reviewtable_rows_selected
        lignam <- r1()[id,1]
        updateSelectInput(session, 'ligand', selected = lignam)
    })

    observeEvent(input$ligand, ignoreNULL=TRUE, {
        if(!input$ligand == '') session_data$selected <- session_data$data$ligands[[input$ligand]]
        atomstoquery$data <- data.frame(name=character(),
                 index=character(),
                 comment=character(),
                 stringsAsFactors=FALSE
        )
        output$atoms <- DT::renderDataTable({DT::datatable(atomstoquery$data)})

        previous = isolate(input$views)
        if(previous == 'aligned'){
            session$sendCustomMessage(type = 'setup', message = list())
            the_pdb_file <- the_mol_file <- the_emaps <- the_2fofc_map <- the_fofc_map <- ''
            updateParam(session=session,'mousePreset', as.character(input$mousePreset))
            selected_ligand <- isolate(session_data$selected)
            possible <- try(selected_ligand$apo_pdb_file, silent = TRUE)
            if(!inherits(possible, 'try-error')){
                the_pdb_file <- selected_ligand$apo_pdb_file
                the_mol_file <- selected_ligand$mol_file
                the_emaps <- selected_ligand$event_map_paths
                the_2fofc_map <- selected_ligand$twofofc_file
                the_fofc_map <- selected_ligand$fofc_file
            }

            if(!the_pdb_file == ''){
                withProgress(message = sprintf('Loading %s Ligand', input$views), value = 0,{
                    incProgress(.2, detail = 'Uploading Crystal + Ligand')
                    try(uploadApoPDB(session = session, filepath = the_pdb_file, repr = 'line'), silent = TRUE)
                    try(uploadMolAndFocus(session = session, filepath = the_mol_file, ext = 'mol'), silent = TRUE)
                    names(the_emaps) <- basename(the_emaps)
                    session_data$current_emaps <- the_emaps
                    incProgress(.2, detail = 'Uploading Event map')
                    updateSelectInput(session, 'emap', choices = names(isolate(session_data$current_emaps)), selected = names(isolate(session_data$current_emaps))[1])
                    # Move this to a different part?
                    incProgress(.2, detail = 'Uploading 2fofc map')
                    try(uploadVolumeDensity(session=session, filepath=the_2fofc_map,
                        color = 'blue', negateiso = FALSE, boxsize = input$boxsize, isolevel = input$iso2fofc, visable=input$twofofcMap, windowname='twofofc'), silent=F)
                    incProgress(.1, detail = 'Uploading fofc map')
                    try(uploadVolumeDensity(session=session, filepath=the_fofc_map,
                        color = 'lightgreen', negateiso = FALSE, boxsize = input$boxsize, isolevel = input$isofofc, visable=input$fofcMap, windowname='fofcpos'), silent=F)
                    incProgress(.1, detail = 'Uploading fofc map')
                    try(uploadVolumeDensity(session=session, filepath=the_fofc_map,
                        color = 'tomato', negateiso = TRUE, boxsize = input$boxsize, isolevel = input$isofofc, visable=input$fofcMap, windowname='fofcneg'), silent=F)
                    setProgress(1)
                })
            }
        } else {
            # There is a problem with observeEvents not rendering stale references therefore we have to manually the loading if the event state does not change.
            updateRadioButtons(session, 'views', selected = 'aligned')
        }

    })

    observeEvent(input$emap, ignoreNULL = TRUE, {
        sel <- isolate(session_data$current_emaps)[input$emap]
        try(uploadVolumeDensity(session=session, filepath=sel,
            color = 'orange', negateiso = FALSE, boxsize = input$boxsize, isolevel = input$isoEvent, visable=input$eventMap, windowname='eventmap'), silent=F)
    })

    observeEvent(input$views, {
        selected_ligand <- isolate(session_data$selected)
        if(is.null(input$views)) updateRadioButtons(session, 'views', selected = 'aligned')
        session$sendCustomMessage(type = 'setup', message = list())
        updateParam(session=session,'mousePreset', as.character(input$mousePreset))
        updateParam(session=session,'clipDist', as.character(input$clipDist))
        updateSelectInput(session, 'emap', choices = c('NotAMap.ccp4'), selected = c('NotAMap.ccp4'))
        updateSelectInput(session, 'asuSwitch', selected='AU', choices=c('AU', 'UNITCELL', 'SUPERCELL'))
        
        possible <- try(selected_ligand$apo_pdb_file, silent = TRUE)
        if(!inherits(possible, 'try-error')){
            the_pdb_file <- selected_ligand$apo_pdb_file
            the_mol_file <- selected_ligand$mol_file
            the_emaps <- selected_ligand$event_map_paths
            the_2fofc_map <- selected_ligand$twofofc_file
            the_fofc_map <- selected_ligand$fofc_file
        } else {
            the_pdb_file <- ''
        }

        withProgress(message = sprintf('Loading %s Ligand', input$views), value = 0, {
            if(!the_pdb_file == ''){
                incProgress(.2, detail = 'Uploading Crystal + Ligand')
                switch(isolate(input$views),
                    ' ' = {
                        the_pdb_file <- ''
                        the_mol_file <- ''
                        the_emaps <- ''
                        the_2fofc_map <- ''
                        the_fofc_map <- ''
                    },
                    'aligned' = {
                        # Default Behaviour do not change anything!
                        try(uploadApoPDB(session = session, filepath = the_pdb_file, repr = 'line'), silent = TRUE)
                        try(uploadMolAndFocus(session = session, filepath = the_mol_file, ext = 'mol'), silent = TRUE)
                    },
                    'crystallographic' = {
                        the_pdb_file <- selected_ligand$crys_pdb_file
                        try(uploadPDB(session = session, filepath = the_pdb_file, input = input), silent=T)
                        the_2fofc_map <- selected_ligand$crys_2fofc_map
                        the_fofc_map <- selected_ligand$crys_fofc_map
                        the_emaps <- selected_ligand$crys_event_maps
                    }
                )
                if(length(the_emaps) > 0) {
                    names(the_emaps) <- basename(the_emaps)
                    session_data$current_emaps <- the_emaps
                    updateSelectInput(session, 'emap', choices = names(isolate(session_data$current_emaps)), selected = names(isolate(session_data$current_emaps))[1])
                }
                incProgress(.2, detail = 'Uploading Event map')
                # Move this to a different part?
                incProgress(.2, detail = 'Uploading 2fofc map')
                try(uploadVolumeDensity(session=session, filepath=the_2fofc_map,
                    color = 'blue', negateiso = FALSE, boxsize = input$boxsize, isolevel = input$iso2fofc, visable=input$twofofcMap, windowname='twofofc'), silent=T)
                incProgress(.1, detail = 'Uploading fofc map')
                try(uploadVolumeDensity(session=session, filepath=the_fofc_map,
                    color = 'lightgreen', negateiso = FALSE, boxsize = input$boxsize, isolevel = input$isofofc, visable=input$fofcMap, windowname='fofcpos'), silent=T)
                incProgress(.1, detail = 'Uploading fofc map')
                try(uploadVolumeDensity(session=session, filepath=the_fofc_map,
                    color = 'tomato', negateiso = TRUE, boxsize = input$boxsize, isolevel = input$isofofc, visable=input$fofcMap, windowname='fofcneg'), silent=T)
            }
            setProgress(1)
        })
    })

    # Control Panel Listeners
    observeEvent(input$controls, ignoreNULL = TRUE, {
        showModal(
            controlPanelModal(
                values = isolate(ngl_control_values$defaults),
                title = 'NGL Viewer Controls'
            )
        )
    })
    session_data$ngl_not_opened <- TRUE

    session_data$render_fragview <- TRUE
    observeEvent(input$tab, ignoreNULL = TRUE, {
        if(input$tab == 'review' & session_data$ngl_not_opened){
            session_data$ngl_not_opened <- FALSE
            showModal(
                controlPanelModal(
                    values = isolate(ngl_control_values$defaults),
                    title = 'As part of setup please confirm NGL Viewer Controls'
                )
            )
        }
        if(input$tab == 'fragview'){
            updateActionButton(session, 'updateTable')
            ligands <- isolate(session_data$data$get_ligands)
            apo_files <- isolate(session_data$data$get_apo_files)
            mol_files <- isolate(session_data$data$get_mol_files)
            updateSelectInput(session, 'goto', choices = ligands)
            tryAddPDB <- try(uploadApoPDB(filepath=apo_files[1], repr='cartoon'), silent=T)
            molout <- try(sapply(mol_files, uploadUnfocussedMol), silent=T)
        }
    })

    observeEvent(input$as_clear, {
        session$sendCustomMessage(type = 'as_resetclicked', list())
        atomstoquery$data <- data.frame(name = character(),
            index = character(),
            comment = character(),
            stringsAsFactors = FALSE)

    })

    # Needs some custom JS: 
    # 1) loadFile and auto-view (do not re-render thing)
    # 2) Save the coordinates of default view and call to them directly :D?
    # 3) Cry
    observeEvent(input$fitButton, {
        try(uploadMolAndFocus(session = session, filepath = isolate(session_data$selected)$mol_file, ext = 'mol'), silent = TRUE)
    })

    observeEvent(input$decision, {
        possAns <- possRes[[input$decision]]
        updateSelectizeInput(session, 'reason', choices = possAns)
    })

    formData <- reactive({
        data_to_retrieve <- sapply(
            c('decision', 'reason', 'comments'),
            function(x) paste0(input[[x]], collapse = ';')
        )
        list(decision_str = data_to_retrieve[1],
             reason = data_to_retrieve[2],
             comment = data_to_retrieve[3]
            )
    })

    observeEvent(input$submit, ignoreNULL=TRUE, {
        fData <- formData()
        if(any(fData[1:2] %in% c('', ' '))){
            showModal(modalDialog(title = 'Please fill all fields in the form',
            'One or more fields have been left blank - please provide a decision and reason(s) before clicking submit', 
            easyClose = TRUE, footer = tagList(modalButton("Cancel"))
            ))
        } else {
            if(any(as.character(atomstoquery$data$comment) %in% c('', ' '))){
                showModal(modalDialog(title = 'You have flagged some atoms',
                    'Please annotate the selected atoms in the Atom Selection tab by double clicking on the comment cells. If you accidentally flagged an atom, try reloading the structure and resubmitting your review!',
                    easyClose=TRUE
                ))
            } else {
                saveReview(x = fData, z = atomstoquery$data, ligand = isolate(session_data$selected))
                resetForm(session = session, session_data = session_data)
                r1 <- remapData(session_data = session_data)
                output$reviewtable <- updateMainTable(x=r1)
            }
        }
    })

    observeEvent(input$updateTable, ignoreNULL=FALSE, {
        f1 <- remapFragviewData(session_data = session_data)
        output$fragviewtable <- updateMainTable(x=f1, pl=100, format=FALSE)
        apo_files <- isolate(session_data$data$get_apo_files)
        mol_files <- isolate(session_data$data$get_mol_files)
        try(uploadApoPDB(session=session, filepath=apo_files[1], repr='cartoon'), silent=F)
        try(sapply(mol_files, uploadUnfocussedMol, session=session), silent=F)
    })

    observeEvent(input$write_fv, {
        ligand <- session_data$data$ligands[[input$goto]]
        newmeta <- c('',ligand$name, ligand$crystal, ligand$smiles_string, input$new_smiles, 
        input$alternate_name, input$site_name, input$pdb_entry)
        newmeta[is.na(newmeta)] <- ''
        ligand$metadata <- newmeta
        output$metastatus <- renderText({'STATUS: Written!'})
        if(!input$desync){
            f1 <- remapFragviewData(session_data = session_data)
            output$fragviewtable <- updateMainTable(x=f1, pl=100, format=FALSE)
        }
    })


    observeEvent(input$gonext, {
        ligands <- isolate(session_data$data$get_ligands)
        apo_files <- isolate(session_data$data$get_apo_files)
        mol_files <- isolate(session_data$data$get_mol_files)
        nmol <- length(mol_files)
        id <- which(ligands == input$goto)
        next_id <- id + 1
        if(next_id > nmol) next_id <- 1 # Overflow back to start of list
        # Cycle along to next ligand in molfil
        updateSelectInput(session, 'goto', selected = ligands[next_id], choices=ligands)
    })

    observeEvent(input$goback, {
        ligands <- isolate(session_data$data$get_ligands)
        apo_files <- isolate(session_data$data$get_apo_files)
        mol_files <- isolate(session_data$data$get_mol_files)
        nmol <- length(mol_files)
        id <- which(ligands == input$goto)
        next_id <- id - 1
        if(next_id < nmol) next_id <- 1 # Overflow back to start of list
        # Cycle along to next ligand in molfil
        updateSelectInput(session, 'goto', selected = ligands[next_id], choices=ligands)
    })

    observeEvent(input$goto, {
        output$metastatus <- renderText({'STATUS: Pending...'})
        ligand <- session_data$data$ligands[[input$goto]]
        mol_file <- ligand$mol_file
        smiles <- ligand$get_smiles
        choices <- unique(c('', as.character(remapFragviewData(session_data = session_data)()[, 6])))
        meta <- as.character(ligand$metadata)
        meta[is.na(meta)] <- ''
        # Fill Form as seen
        updateTextInput(session, 'crysname', value = input$goto)
        updateTextInput(session, 'smiles', value = meta[4])
        updateTextInput(session, 'new_smiles', value = meta[5])
        updateTextInput(session, 'alternate_name', value = meta[6])
        updateSelectizeInput(session, 'site_name', selected = meta[7], choices=choices)
        updateTextInput(session, 'pdb_entry', value = meta[8])
        # Go to specific ligand do not edit go next loop
        gogogo <- try(uploadMF2(session=session, filepath=mol_file), silent=F)
    })

        # On Table Rowclick # Potentially slow? Unneeded? # Go back to
    observeEvent(input$fragviewtable_rows_selected, {
        ligands <- isolate(session_data$data$get_ligands)
        choice = isolate(rownames(f1())[input$fragviewtable_rows_selected])
        updateSelectizeInput(session, 'goto', selected = choice, choices=ligands)
    })

    observeEvent(input$lp_launcher, {
        session_data$fullpath_frag <- createFragUploadFolder(
            data = session_data$data,
            copymaps = input$copymaps
        )
    })

    output$lp_download <- downloadHandler(
        filename = function(){
            return(basename(session_data$fullpath_frag))
        },
        content = function(file) {
            file.copy(session_data$fullpath_frag, file)
        }
    )
}
