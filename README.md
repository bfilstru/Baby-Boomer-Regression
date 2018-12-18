# Baby-Boomer-Regression
Baby Boomer regression data analysis done in R.

## DATA ORIGINS

All data comes from IPUMS (USA). My data pulls from
the 2009-2011 ACS and the 2015-2017 ACS.

[IPUMS]
(https://usa.ipums.org/usa/index.shtml)

## INCLUDED VARIABLES

Below are the raw variables taken from the 
above data pull (taken straight from the 
IPUMS provided codebook) with the person/household 
type appended to each respective variable.

  Variable               
  YEAR               H   
  DATANUM            H   
  SERIAL             H   
  CBSERIAL           H  
  HHWT               H  
  GQ                 H  
  PERNUM             P  
  PERWT              P  
  YNGCH              P  
  SEX                P  
  AGE                P  
  MARST              P  
  RACE               P  
  RACED              P  
  HISPAN             P  
  HISPAND            P  
  EDUC               P  
  EDUCD              P  
  LABFORCE           P  
  INCTOT             P  
  MIGRATE1           P  
  MIGRATE1D          P  
  DIFFREM            P  
  DIFFPHYS           P  
  DIFFMOB            P  
  DIFFCARE           P  
  DIFFSENS           P  
  DIFFEYE            P  
  DIFFHEAR           P  
  VETSTAT            P  
  VETSTATD           P  
  
  ## INSTRUCTIONS FOR RUNNING
  
  - Change line 17 to your personal working directory.
  - Unzip the downloaded data CSVs into your working directory.
  - Beyond the working directory and data location the rest 
  of the code should work without any special alterations. 
  The rest of the instructions should only be read in the 
  event that the code does not work/encounters issues.
  - Make sure both data sets are appropriately titled as:
    - "usa_00003.csv"
    - "usa_00004.csv"
  - All necessary libraries are provided. Upon the first run
  a few errors will appear when going through the library
  packages. This is expected and will not impact the program.
  - Highlight the entire program (can simply click 4 times im 
  the same spot) and select "Run" in the top right of the 
  workspace (alternatively, you can press CTRL+ENTER).
  - Reading in the data and running the final regression will
  be slow due to the size of the data. This is to be expected.
  As long as the code has been running through the code (shown 
  in blue in the console output below) then everything is fine.
  The program will be done running when the regression output is
  shown in the console
  - The summary statistics will be saved in your working directory
  as:
    - "Summary_Statistics_MEGA_2009_2011.txt"
    - "Summary_Statistics_MEGA_2015_2017.txt"
  - Additionally, the regression output will be saved to your working
  directory as:
    - "Regression_Results_MEGA.txt"
  - IMPORTANT: If you receive a message like this: "Error: cannot 
  allocate vector of size 20.4 Mb", it means your computer is not
  currently capable of running the program. Though I've rarely 
  encountered this issue, this is a potential solution:
    1. Close other unnecessary programs hogging processing power.
    2. Try running the program again.
  If these steos fail, the other option is to delete half of the
  regression and only run it only for 2009-2011 ACS data (reg1-reg4)
  or for only 2015-2017 ACS data (reg5-reg8). If it does come to his,
  the individual will need to remove the corresponding regression
  mentions from line 160 and line 161. To run the other entry, simply
  CTRL+Z until the code returns to its original state and delete the
  opposite regressions.
  
    
