#' Ligand Class
#' 
#' Describes a single Ligand from fragalysis-api
#' 
#' @importFrom R6 R6Class
#' @exportClass Ligand
#' @examples 
#' \dontrun{
#' l <- Ligand$new(name = 'Prot-x1234', base = './')
#' }
Ligand <- R6::R6Class(
    "Ligand",
    public = list(
        #' @field name Name of the ligand
        name = NA,
        #' @field crystal Name of the crystal the ligand belongs to
        crystal = NA,
        #' @field aligned_loc path of the aligned folder
        aligned_loc = NA,
        #' @field metadata_filepath path to the metadata file
        metadata_filepath = NA,
        #' @field crys_loc path of the crystallographic folder
        crys_loc = NA,

        #' @description
        #' Create a new ligand
        #' @param name Name of Ligand
        #' @param base Directory path containing ligandy goodness
        #' @return ligand class!
        initialize = function(
            name,
            base
        ) {
            self$name <- name
            self$crystal <- rsplit(name, '_', 1)[1]
            self$aligned_loc <- file.path(base, 'aligned', name)
            self$crys_loc <- file.path(base, 'crystallographic')
            self$metadata_filepath <- file.path(
                base, 'aligned', name, sprintf('%s_meta.csv', name)
            )
        }
    ),
    active = list(
        #' @field apo_pdb_file file path where apo.pdb should exist
        apo_pdb_file    = function(value){ 
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_apo.pdb', self$name)
            )
        },
        #' @field mol_file file path where .mol should exist
        mol_file        = function(value){
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s.mol', self$name)
            )
        },
        #' @field twofofc_file file path where _2fofc.map should exist
        twofofc_file    = function(value){ 
            if(missing(value)) {
                files = dir(self$aligned_loc, full=T)
                files[grep('_2fofc', files)][1]
            }
        },
        #' @field fofc_file file path where _fofc.map should exist
        fofc_file       = function(value){ 
            if(missing(value)) {
                files = dir(self$aligned_loc, full=T)
                files[grep('_fofc', files)][1]
            }
        },
        #' @field smiles_file file path where _smiles.txt should exist
        smiles_file     = function(value){ 
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_smiles.txt', self$name)
            )
        },
        #' @field smiles_string The contents of _smiles.txt
        smiles_string   = function(value){ 
            if(missing(value)) suppressWarnings(readLines(self$smiles_file))
        },
        #' @field event_map_paths file paths where event maps should reside
        event_map_paths = function(value){ 
            if(missing(value)) {
                files = dir(self$aligned_loc, full=T)
                files[grep('_event', files)]
            }
        },
        #' @field first_event_map returns the first eventmap
        first_event_map = function(value){ 
            if(missing(value)) self$event_map_paths[1]
        }, # Just take the first event map 
        #' @field metadata the contents of the metadata
        metadata        = function(value){
            if(missing(value)){
                suppressWarnings(read.csv(
                    self$metadata_filepath, 
                    header = FALSE, 
                    stringsAsFactors = FALSE))
            } else {
                # Error if fundamental changes are afoot.
                stopifnot(length(value) == 8)
                stopifnot(value[3] == self$crystal)
                stopifnot(value[2] == self$name)
                cat(paste(value, collapse=','), file = self$metadata_filepath)
            }
        },
        #' @field review_file file path where _review.txt should exist
        review_file     = function(value){
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_review.txt', self$name)
            )
        },
        #' @field review Contents of the review file
        review          = function(value){
            if(missing(value)){
                out <- try(
                    strsplit(
                        suppressWarnings(
                            readLines(self$review_file)
                        ),
                    '\t')[[1]],
                    silent = TRUE
                )
                if(inherits(out, 'try-error')) out <- c('','','','','')
                return(out)
            } else {
                stopifnot(length(value) == 5)
                cat(paste(value, collapse='\t'), file = self$review_file)
            }
        },
        #' @field status What is the review status
        status = function(value){
            if(missing(value)) self$review[1]
        },
        #' @field crys_pdb_file file path where crystallographic pdb should exist
        crys_pdb_file = function(value){
            if(missing(value)){
                file.path(self$crys_loc, paste0(self$crystal,'.pdb'))
            }
        },
        #' @field crys_event_maps file path where crystallographic event exist
        crys_event_maps = function(value){
            if(missing(value)){
                files = dir(self$crys_loc, pattern=self$crystal, full=T)
                files[grep('_event', files)]
            }
        },
        #' @field crys_first_event_map file path of 1st event map
        crys_first_event_map = function(value){
            if(missing(value)) self$crys_event_maps[1]
        },
        #' @field crys_2fofc_map filepath of 2fofc map
        crys_2fofc_map = function(value){
            if(missing(value)){
                files = dir(self$crys_loc, pattern=self$crystal, full=T)
                files[grep('_2fofc', files)]
            }
        },
        #' @field crys_fofc_map filepath of fofc map
        crys_fofc_map = function(value){
            if(missing(value)){
                files = dir(self$crys_loc, pattern=self$crystal, full=T)
                files[grep('_fofc', files)]
            }
        }
    )
)

#' Get a specific property from a list of ligands
#' 
#' @param value here for decoration, not needed
#' @param ligands the list of ligands
#' @param what what property to get
#' 
#' @return gets some stuff from ligands i guess
#' @export
#' @examples 
#' \dontrun{
#' ligands <- list(
#'  Ligand$new(name = 'Prot-x1234', base = './'),
#'  Ligand$new(name = 'Prot-x1233', base = './')
#' )
#' getLigandProp(ligands=ligands, what = 'name')
#' }
getLigandProp <- function(value, ligands, what){
    if (missing(value)){
        sapply(ligands, '[[', what)
    } else {
        stop(sprintf("`$%s` is read only.", what), call. = FALSE)
    }
}
#' Experiment Class
#' 
#' Describes a collection of ligands from fragalysis-api output.
#' 
#' @importFrom R6 R6Class
#' @exportClass Experiment
#' @examples 
#' \dontrun{
#' e <- Experiment$new(folder_path = './')
#' }
Experiment <- R6::R6Class(
    "Experiment",
    public = list(
        #' @field target Name of protein system
        target = NA,
        #' @field ligands List of Ligands
        ligands = NA,
        #' @field filepath input directory
        filepath = NA,
        #' @description
        #' Create a new Experiment object
        #' @param folder_path Directory path
        #' @return experiment class!
        initialize = function(folder_path){
            self$filepath <- folder_path
            self$target <- basename(folder_path)
            ligands <- dirname(dir(
                file.path(folder_path, 'aligned'), 
                pattern = '_apo.pdb$', recursive = TRUE
            ))
            self$ligands <- lapply(ligands, function(x){
                Ligand$new(x, folder_path)
            })
            names(self$ligands) <- ligands
        }
    ),
    active = list(
        #' @field get_ligands Vector of ligand names
        get_ligands     = function(value){
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'name'
            )
        },
        #' @field get_apo_files Vector of apo file names
        get_apo_files   = function(value){
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'apo_pdb_file'
            )
        },
        #' @field get_mol_files Vector of .mol files
        get_mol_files   = function(value){
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'mol_file'
            )
        },
        #' @field get_2fofc_maps Vector of 2fofc maps
        get_2fofc_maps  = function(value){
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'two_fofc_file'
            )
        },
        #' @field get_fofc_maps Vector of fofc maps
        get_fofc_maps   = function(value){
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'fofc_file'
            )
        },
        #' @field get_event_maps Vector of event maps
        get_event_maps  = function(value){
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'first_event_map'
            )
        },
        #' @field get_smiles Vector of SMILES strings
        get_smiles      = function(value){
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'smiles_string'
            )
        },
        #' @field get_reviews Table of Reviews
        get_reviews     = function(value) {
            res <- t(getLigandProp(value = value, 
                ligands = self$ligands, what = 'review'))
            colnames(res) <- c('decision_str', 'reason', 
                'comments', 'bad_ids', 'bad_comments')
            return(res)
        }, # Return a Table?
        #' @field get_metadata Table of meta data
        get_metadata    = function(value) {
            data.frame(
                apply(
                    t(getLigandProp(
                        value = value, 
                        ligands = self$ligands, 
                        what = 'metadata')),
                    2, 
                    unlist
                ),
                stringsAsFactors = FALSE
            )
        },
        #' @field get_status Get the review status of ligands
        get_status = function(value) {
            getLigandProp(
                value = value, 
                ligands = self$ligands, 
                what = 'status'
            )
        }
    )
)

#' Create experiment class object
#' 
#' Wrapper for R6 method
#' 
#' @param folder_path File input
#' @return Experiment Class
#' @export 
#' @examples 
#' \dontrun{
#' e <- createExperiment(folder_path = './')
#' }
createExperiment <- function(folder_path) Experiment$new(folder_path)

#' Number of ligands
#' 
#' The number of ligands in an experiment
#' @param exp Experiment Class
#' @return Integer, corresponding to number of ligands
#' @export
#' @examples 
#' \dontrun{
#' e <- createExperiment(folder_path = './')
#' n_ligs(e)
#' }
n_ligs <- function(exp) return(length(exp$get_ligands))

#' Number of ligands annotated
#' 
#' The number of annotated ligands in an experiment
#' @param exp Experiment Class
#' @return Integer, corresponding to number of ligands annotated
#' @export
#' @examples 
#' \dontrun{
#' e <- createExperiment(folder_path = './')
#' n_anno(e)
#' }
n_anno <- function(exp) return(sum(!is.na(exp$get_metadata[ ,7])))

#' Number of ligands reviewed
#' 
#' The number of reviewed ligands in an experiment
#' @param exp Experiment Class
#' @return Integer, corresponding to number of ligands that are reviewed
#' @export
#' @examples 
#' \dontrun{
#' e <- createExperiment(folder_path = './')
#' n_revi(e)
#' }
n_revi <- function(exp) return(sum(!exp$get_reviews[ ,'decision_str'] == ''))
