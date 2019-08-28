# Title: Thesis 
# Author: Eva Linehan el1718@ic.ac.uk
# Date: July 2019
# Course : MSc Computational Methods in Ecology and Evolution



## Folder contents
This folder contains coding scripts relating to the execution of my thesis. All files arranged in the '**Code**' and '**Code**'. Please contact my email should you wish to ask any questions: eva.linehan18@imperial.ac.uk.

## Executing Project Analysis
The main scripts that encompass project analysis are as follows;
* Simulations.ipynb
* Revised_Simulation_Analysis.ipynb
* Revised_RealData_Analysis.ipynb

All scripts require the same path directory in this repository.

### '**Code**' contains the following; 

1. 'BIOT_Explore.R'
* R script used to investigate BIOT data, mainly misaligned coordinates, timestamps and information extraction from flight tlogs.

2. 'BIOT_Figures.R'
* Initial R script used to render report figures in ggplot2.

3. 'Data_Wrangling.R'
* Initial R script used to wrangle data.

3. 'Extract_image_info.py'
* Python script used to extract timestamp and GPS coordinates from raw images.

4. 'getHPCsims.sh'
* Shell script to retrieve HPC output from Imperial College London High Performance Computer.

5. 'GPS_from_images.sh'
* Shell script to move images into a single folder for analysis using 'Extract_image_info.py'.

6. 'GPS_from_TLOGS.sh'
* Shell script using mavlogdump.py to retrieve coordinates from tlogs.

7. 'gREM_Density.R'
* R script to run the gREM model and run other descriptive statistics.

8. 'Initial_gREM_script.R'
* Initial script with old code when analyzing dataset.

9. 'Preliminary_Analysis.R'
* Initial script with old code when analyzing dataset.

10. 'Simulations.ipynb'
* Jupyter Notebook to test and execute simulation code.

11. 'Simulations.py'
* Python script to run UAV simulations locally.

12. 'RealData_Analysis.ipynb'
* Initial Jupyter Notebook to analyze UAV data.

12. 'Revised_RealData_Analysis.ipynb'
* Actual Jupyter Notebook used to analyze UAV data.

13. 'Simulation_Analysis.ipynb'
* Jupyter Notebook to analyze simulation data.

14. 'Revised_Simulation_Analysis.ipynb'
* Actual Jupyter Notebook to analyze simulation data.

15. 'single_sim.py'
* Python script to run simulation on HPC.

16. 'run_sims.py'
* Python script to run simulation output on HPC.

17. 'run_sims.sh'
* Shell script to run simulations as jobs in parallel on HPC.
