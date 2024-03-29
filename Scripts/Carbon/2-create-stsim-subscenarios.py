## a269
## Katie Birchard (ApexRMS)
## April 2023
##
## This script creates ST-Sim datasheets for the CBM-CFS3 Spinup 
## and saves them as .csvs
## Option to save datasheets directly to library
## Assumes that 1-load-definitions.py script is run first (to generate project 
## datasheets)

## Workspace ----
# Set up environment
import os
import sys

# Load paths to retrieve helper functions and constants
cwd = os.getcwd()
root_dir = cwd.split(r"nestweb")[0] + "nestweb"

# Set working directory
os.chdir(os.path.join(root_dir, "Scripts/Carbon"))

# Add Scripts directory to path
sys.path.append(os.path.join(root_dir, "Scripts"))

# Import helper functions
from helper_functions import *

# Import constants/global variables 
from constants import *

# import modules
import pandas as pd
import pysyncrosim as ps

# Create directories for initial conditions and transition targets
create_subscenario_dir("stsim_RunControl", dir_name=CUSTOM_CARBON_SUB_CBM_SPINUP_DIR)


# Set preferences for saving and exporting definitions
saveDatasheets = False # Set to True to save datasheet back to library
exportDatasheets = True # Set to True to export datasheet as csv

if saveDatasheets:
    # Set up SyncroSim session if saveDatasheets = True
    mySession = ps.Session()
    mySession.add_packages("stsim")
    mySession.add_packages("stsimsf")
    mySession.add_packages("stsimcbmcfs3")
    
    # Uses the default SyncroSim session
    myLibrary = ps.library(name = os.path.join(LIBRARY_DIR, LIBRARY_FILE_NAME_BARE_LAND_SPINUP),
      session = mySession, package = "stsim", addons = "stsimsf")
    myLibrary.enable_addons("stsimcbmcfs3")
      
    # Assumes there is only one default project per library
    myProject = myLibrary.projects(name = "Definitions")

# Set myScenario as None - reset if saveDatasheets is True
myScenario = None

# Run Control ---------------------------------------------------------------
# Run Control: Non-spatial 300 years, 1 MC
scenarioName = "Run Control: Non-spatial, 300 yr, 1 MC"
if saveDatasheets:
    myScenario = myProject.scenarios(name = scenarioName)

datasheetName = "stsim_RunControl"
myDatasheet = pd.DataFrame({"MaximumIteration": [1], 
                            "MinimumTimestep": [0], 
                            "MaximumTimestep": [300]})

finalize_datasheets(saveDatasheets,
                    exportDatasheets,
                    myScenario,
                    datasheetName,
                    myDatasheet, 
                    os.path.join(CUSTOM_CARBON_SUB_CBM_SPINUP_DIR,
                                 datasheetName))