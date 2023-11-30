# TMCF_Microclimate
Microclimate data from the TMCF epi-feedbacks project

Folders in Github:
Microclimate_data_supporting: Metadata files used in the various processing steps
Microclimate_data_Raw: Raw data on a per-datalogger basis, with variables identified by datalogger:port rather than by tree:station
Microclimate_data_L2: Data organized by tree:station, with a file for each tree. Starts in June 2023 because of a change in import processes
Microclimate_data_L3: Full datasets going back to project start, with a few cleaning steps applied

Folders (stored elsewhere; ignored by github):
Scripts_extra: Extra scripts I didn't want to delete 

Scripts:
Functions_microclimate.R: Functions used in the Import script
Import_and_process_MC_data.R: Processing script to import data from Zentracloud and then move it to L3.
Shiny_MC.R: Script to create the Shiny app
QC_MC.R: Quality control on MC data. This is a work in progress

Units:
Many... Will fill this out later
