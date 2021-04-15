Ligand <- R6::R6Class(
    "Ligand",
    public = list(
        name = NA,
        crystal = NA,
        aligned_loc = NA,
        metadata_filepath = NA,
        crys_loc = NA,
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
        apo_pdb_file    = function(value){ 
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_apo.pdb', self$name)
            )
        },
        mol_file        = function(value){
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s.mol', self$name)
            )
        },
        twofofc_file    = function(value){ 
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_2fofc.map', self$name)
            )
        },
        fofc_file       = function(value){ 
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_fofc.map', self$name)
            )
        },
        smiles_file     = function(value){ 
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_smiles.txt', self$name)
            )
        },
        smiles_string   = function(value){ 
            if(missing(value)) suppressWarnings(readLines(self$smiles_file))
        },
        event_map_paths = function(value){ 
            if(missing(value)) file.path(
                self$aligned_loc, 
                dir(self$aligned_loc, pattern = 'event_')
            )
        },
        first_event_map = function(value){ 
            if(missing(value)) self$event_maps[1]
        }, # Just take the first event map 
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
        review_file     = function(value){
            if(missing(value)) file.path(
                self$aligned_loc, 
                sprintf('%s_review.txt', self$name)
            )
        },
        review          = function(value){
            if(missing(value)){
                out <- try(strsplit(suppressWarnings(readLines(self$review_file)), '\t')[[1]], silent=TRUE)
                if(inherits(out, 'try-error')) out <- c('','','','','')
                return(out)
            } else {
                stopifnot(length(value) == 5)
                cat(paste(value, collapse='\t'), file = self$review_file)
            }
        },
        crys_pdb_file = function(value){
            if(missing(value)){
                file.path(self$crys_loc, paste0(self$crystal,'.pdb'))
            }
        },
        crys_event_maps = function(value){
            if(missing(value)){
                files = dir(self$crys_loc, pattern=self$crystal, full=T)
                files[grep('_event', files)]
            }
        },
        crys_first_event_map = function(value){
            if(missing(value)) self$crys_event_maps[1]
        },
        crys_2fofc_map = function(value){
            if(missing(value)){
                file.path(self$crys_loc, paste0(self$crystal,'_2fofc.map'))
            }
        },
        crys_fofc_map = function(value){
            if(missing(value)){
                file.path(self$crys_loc, paste0(self$crystal,'_fofc.map'))
            }
        }
    )
)

getLigandProp <- function(value, ligands, what){
    if (missing(value)){
        sapply(ligands, '[[', what)
    } else {
        stop(sprintf("`$%s` is read only.", what), call. = FALSE)
    }
}

Experiment <- R6::R6Class(
    "Experiment",
    public = list(
        target = NA,
        ligands = NA,
        filepath = NA,
        initialize = function(folder_path){
            self$filepath <- folder_path
            self$target <- basename(folder_path)
            ligands <- dirname(dir(
                file.path(folder_path, 'aligned'), 
                pattern = '_apo.pdb$', recursive = TRUE
            ))
            self$ligands <- lapply(ligands, function(x) Ligand$new(x, folder_path))
            names(self$ligands) <- ligands
        }
    ),
    active = list(
        get_ligands     = function(value) getLigandProp(value = value, ligands = self$ligands, what = 'name'),
        get_apo_files   = function(value) getLigandProp(value = value, ligands = self$ligands, what = 'apo_pdb_file'),
        get_mol_files   = function(value) getLigandProp(value = value, ligands = self$ligands, what = 'mol_file'),
        get_2fofc_maps  = function(value) getLigandProp(value = value, ligands = self$ligands, what = 'two_fofc_file'),
        get_fofc_maps   = function(value) getLigandProp(value = value, ligands = self$ligands, what = 'fofc_file'),
        get_event_maps  = function(value) getLigandProp(value = value, ligands = self$ligands, what = 'first_event_map'),
        get_smiles      = function(value) getLigandProp(value = value, ligands = self$ligands, what = 'smiles_string'),
        get_reviews     = function(value) {
            res <- t(getLigandProp(value = value, ligands = self$ligands, what = 'review'))
            colnames(res) <- c('decision_str', 'reason', 'comments', 'bad_ids', 'bad_comments')
            return(res)
        }, # Return a Table?
        get_metadata    = function(value) data.frame(apply(t(getLigandProp(value = value, ligands = self$ligands, what = 'metadata')),2,unlist), stringsAsFactors = FALSE) # This returns a table
    )
)

createExperiment <- function(folder_path) Experiment$new(folder_path)

n_ligs <- function(exp) return(length(exp$get_ligands))
n_anno <- function(exp) return(sum(!is.na(exp$get_metadata[ ,6])))
n_revi <- function(exp) return(sum(!exp$get_reviews[ ,'decision_str'] == ''))
