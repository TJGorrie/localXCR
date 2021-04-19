# localXCR

<a name='installation'></a>

## Installation

To use localXCR you need to have an R installation - the installer can be downloaded [here](https://cloud.r-project.org).

Once R is installed you can install the local XCR R package by using the following commands in the R console (indicated with a `>`)

```R
install.packages('devtools')
devtools::install_github('tjgorrie/nglShiny')
devtools::install_github('tjgorrie/localXCR')
```

## Usage

Once localXCR is installed you can run the program by running the following in the R terminal

```R
library(localXCR)
launch_XCR()
```

0. [Installation and Quickstart](#installation)
1. [Import](#importing)
2. [Review](#reviewing)
3. [Annotate](#annotating)
4. [Export](#exporting)
5. [Tips & Tricks](#tips)
6. [Fragalysis API](#fragalysis-api)
7. [Contact](#contact)

<a name='importing'></a>

## Import

When you open XCR you will see a fairly blank page with a single button in the main body - clicking it will prompt you to load in directory which should contain the data you are expecting to load. This can be generated from the [fragalysis-api](https://github.com/xchem/fragalysis-api/) - which requires a bit of setup but is quite straightforward to use (I hope!). For more XCR related information on XCR [click here!](#fragalysis-api).

The directory structure of the folder you ultimately select should have the following structure:

```
crystal
|--- aligned
|--- |--- crystal-x0001
|--- |--- |--- crystal-x0001_apo.pdb
|--- |--- |--- crystal-x0001_smiles.txt
|--- |--- |--- crystal-x0001_2fofc.map
|--- |--- |--- crystal-x0001_event_0.ccp4
|--- |--- |--- ...
|--- |--- crystal-x0002
|--- |--- |--- ...
|--- |--- |--- ...
|--- |--- ...
|--- crystallographic
|--- |--- crystal-x0001.pdb
|--- |--- crystal-x0001_smiles.txt
|--- |--- crystal-x0001_2fofc.map
|--- |--- crystal-x0001_event_0.cpp4
|--- |--- ...
|--- |--- crystal-x0002.pdb
|--- |--- crystal-x0002_smiles.txt
|--- |--- ... (and so on)
```

From the above directory structure - for a given directory (e.g. crystal), localXCR expects that there is a **crystal/aligned** and **crystal/crystallographic** folder. The aligned folder contains the outputs of the fragalysis-api, these will contain representations of each ligand within a model (.pdb file) and may be split according to chain. Additionally the data will be aligned to the best resolution model in the dataset which means that slight deviations may be introduced when comparing the aligned ligand to the crystallographic model. We are always working on improving the precision of this!

Once a directory has been selected three info boxes will appear that describe how much reviewing and annotation need to be done. But you can now move onto the Reviewing tab.

<a name='reviewing'></a>

## Review

When you first open the reviewing tab it will prompt you to configure rendering options (such as the mouse behaviour) - in general you can just click the button at the bottom of the dialogue - if you need to re-open these options you can click on the cog in the top-right corner.

The reviewing of a ligand is quite straightforward - for each ligand described in the table displayed towards the bottom right provide a review given the evidence i.e. the structure and the electron density maps (if any). To select a ligand you can use the drop down menu on the left hand side or simply click the corresponding row in the table. This wi.. render the ligand in the ngl window at the top of the window.

When reviewing a structure consider how well the ligand fits within the protein structure and how well the electron density fits. If you think some technical artifacts have been introduced by the fragalysis api you can opt to render the original file by selecting the Raw input radio button.

To make the maps appear you may need to move the protein around the stage a bit (right-click drag) - but other than that it should all work fine.

### Atom Selection

A new feature in XCR is the ability to select individual atoms as part of the review. When reviewing if there are parts of the ligand that are not explained by the electron density you can highlight them (using ALT + Left Click) - you can then go to the Atom Selection Tab and provide an explanation regarding why you have selected the atom (by double clicking on the table cell). This will then be attached to the ligand and communicated in fragalysis.

### Making a Review

When you want to make a review - please fill all boxes on the left hand side and then press submit - this will write to a file within the aligned directory that will be used later on when exporting the data.

<a name='annotating'></a>

## Annotate

### n.b. There is currently an unresolvable bug which means data does not initially render when switching to the annotate tab - please click on refresh metadata to begin the process!

Once the table has rendered (using the above fix) - you can select ligands (much like in the review process) and begin assigning contextual information to them. In addition, you can use this step to correct some errors that may affect the how the ligand is displayed in Fragalysis.

The most important field is the Site Name - try to use an informative label so users of fragalysis can understand your experiment better.

If you want to ignore a particular ligand you can simply assign the ligand a site name of `IGNORE` (in all caps) or set the review to something that is not release.

<a name='exporting'></a>

## Export

After data has been reviewed and annotated - you can double check the numbers in this tab - you will need to press the compile data button to collect all of the Reviewed and Annotated ligands.
The compile button will do two things: Firstly it will generate a metadata.csv file for all Release structures and place it in the root directory that was selected. Secondly it will make a snapshot of the imported directory with the date and time it was made - this file will then be the one that should be uploaded to Fragalysis.

This will create a new folder in

<a name='tips'></a>

## Tips & Tricks

- To speed up map render time - it might be useful to cut the maps around the ligand e.g. using mapmask. Simply use the crystal-x####.pdb file within each aligned subdirectory as the center point and cut to a desired area (12 works nicely).

<a name='fragalysis-api'></a>

## Fragalysis API

The data that localXCR expects is from the output of the fragalysis API - it is a bit tricky to set up but it is required to place data on Fragalysis. The [readme](https://github.com/xchem/fragalysis-api#readme) will have a more indepth discussion about how to use the fragalysis API but the gist is:

Use this to align together a full set of pdbs together:

```python
python fragalysis-api/fragalysis_api/xcimporter/xcimporter.py  -i [input directory] -o [output directory] -t [target name] -m
```

If you are performing an iterative analysis and would like to add more structures to your fragalysis deposition you can use:

```python
python fragalysis-api/fragalysis_api/xcimporter/single_import.py --in_file=[pdbtobealigned.pdb] --out_dir=[output directory] --target [targetname] -m
```

For more information of the commandline options of fragalysis api please read [section 3 of the fragalysis API readme](https://github.com/xchem/fragalysis-api#3how-to-submit-pdb-files-for-conversion-to-a-fragalysis-friendly-format-fff)
<a name='contact'></a>

## Contact

For all bug-reports please make them [here](https://github.com/TJGorrie/localXCR/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5BBUG%5D)

For all suggestions or feature requests please make them [here](https://github.com/TJGorrie/localXCR/issues/new?assignees=&labels=enhancement&template=feature_request.md&title=%5BFEATURE+REQUEST%5D)

If you have any additional problems that are not solved please do not hesistate to contact Tyler (tyler.gorrie-stone@diamond.ac.uk) - he wrote this diabolical software.

## Why is this written in R instead of python?

Python and R each have their own limitations - although R is perhaps out-of-field for structural biology, python's main limitation is that I do not know it that well thus I have used R instead!
