# Thesis analysis

##1 "Download bracelets from web".
This is an automatic download of multiple zip files wich contains E4 data (EDA, HR, IBI, TEMP etc.), Using Selenium python library.

Open the ipynb file.
In block 3 - change to the desired diractory.
In block 5 - add your mail and password.
In block 6 and so on - change to the desired dates.

##2 "Unleash the zip".
extracting files from zips and renameing them accordingly.
follow the comments in the code.
Using Zip and os libraries.

##3 "Adding subject number".
The porpuse of this code is to find for each documented subject (322 observations) its suitable E4-files, through the notes.xlsx file (my documentation for the experiment).
The output will be the bracelet files with the subjects number (indicates Primery Key). Using Pandas and os libraries. Also new notes file has created: "new_notes.xlsx", with the exact experiment time.

##4 "EDA_Duration" - Adding the Duration time, real Ending and Beginning time of each experiment to the main table, looking only at the EDA files.
By the Unix time of each EDA file (which was extracted in step 2), decided when the real measurment has begun, ended and the duration of the experiment. This EDA data was also inserted to the main notes table, now called:"new_notes_1.xlsx". Using Pandas, os, datetime libraries.

##5 "Dowloading noise files" from the site: https://eda-explorer.media.mit.edu/ 
This site helps to clean EDA artifacts.
There are two ways to clean: multiclass and binary. We decided to try them both. All the results were dowloaded to the folder "noise_files_Binary_Multi".
The noise files are aggregated to 5 seconds, and if there is a noise the five seconds are marked as "-1", else 1. in multiclass "0" is not for sure an artifact.
**citation: Taylor, S., Jaques, N., Chen, W., Fedor, S., Sano, A., and Picard, R. "Automatic Identification of Artifacts in Electrodermal Activity Data" In EMBC, August 2015.

##6 "Cleaning the EDA files" - and also making statistics of them. 
Block 4, and 5 checks how much data (in percentage) of the 322 files are coverd with artifacts. For that I made a table "try_stat.csv" that calculates that in the binary and multiclasse modes. By that (https://onlinelibrary.wiley.com/doi/10.1111/psyp.13420) article we decided to ignore files with more then 25% artifacts, and look at the both modes. The rest of the blocks make clean EDA files - binary and in multiclass.

##7 'Preparing for Matlab' - 
To prepare the Binary and Multyclass files for Matlab process from EDA to SCL using Ledalab, We need to convert the CSV files 
to TXT files with only one column -- The EDA data column. 

##8 Ledalab 
URL - http://www.ledalab.de/download/Analysis%20of%20EDA%20data%20using%20Ledalab.pdf
Citation - Benedek, M. & Kaernbach, C. (2010). A continuous measure of phasic electrodermal activity. Journal of Neuroscience Methods, 190, 80-91.

With this tool we converted EDA to SCL. 

        #8.a download the Ledalab software and Matlab.
        
        #8.b in Ledalab->main->import->gettext2data file, change to sr = 4 (for default 4 Hz EDA rate)
  
        #8.c in Matlab command window run: Ledalab('txt file path\', 'open', 'text2', 'analyze','CDA', 'optimize',1) which makes mat files for all the txt samples
  
        #8.d run the file for_files.m which saves xls files of SCL

##9 'SCL_Average_Norm' - making statistical calculations for SCL binary and milticlass files.
The calculations are: Kurtosis, Skewnwss, Mean, Median, Standart-Deviation. 
Now there is new_notes_2 updated with all the data. 

##10 'Kestrel Measures' - to each subject, calculate the averages of the meteorological conditions by the right time and date.
The measures are: Wind speed (m/s), Real humidity (%), Air Temprature (celsius), Globe Temprature (celsius).
Now there is new_notes_4 (new_notes_3 was cleaned manually) updated with all the data.

##11 'Tmrt' - calculate the mean radiant temprature using this site: https://comfort.cbe.berkeley.edu/

##12 Returning back to SCL - "Recalculating the Noisy EDA files (with more then 25% noise artifacts)". We thought it will be intresting to see how the "bad" samples are influencing the research so I went back to their noise files, found the bad files, cleaned the EDA, turned it into SCL and calculated statistics.

##13 'Merging the questionnaires' - merging to each subject number its demographical data like: Age, Height..

##14 'PET' - calculating the PET of each subject using pyautogui module and Rayman software. https://www.urbanclimate.net/rayman/rayman.htm (the cloths score was calculated manually)

##15 Manual step - For calculating the % of vegetation view, we first tried to use CNN (U-NET) to detect vegetated pixels via semantic segmentation method. I couldn't find labled fish eyed photo sets to train them on the model, so it didn't worked well with the "normal" data sets I downloaded from the internet. So we did a Semi-Automated process in the ArcGis Pro software. There the process was:

        #A. For every site - train a non vegetated and vagetated pixels using "classification tools".
        
        #B. Train Support Vector Machine tool (using the lables from step A) to get the .ecd file.
        
        #C. Use "raster classifier" tool to get the 2 classes raster. 
        
        #D. Use "clip raster" tool to get only the circled fish-eye picture.
        
        #E. With "pixel editor" edit the mis-matched pixels, save the new rater and calculate the amount of green pixels out of the total pixels (which are 1424767).
        
        #F. Add a new column of "other picture number" (the picture numbers of the backview of the subjects) manualy because we start to write this data only at the last subjects (the rest of them was recaptured later or acomplished from the pictures we already had).
        
        #G. Add a new column of "vegetation_percent_180" that we filled manualy with the data that came out from the ArcGis Pro process. We added a VLOOKUP sheet that contains all the pictures numbers (front and back view) with their frontal view percentage. After few itterations we decided to add also the actual number of pixels for both of the images (Vegetation_Pixels, Other_Vegetation_Pixels) to be more flexible with the "green view" calculations. Then we added the 360 degress view column called "Vegetation_Percent_360" and the calculation is based on equation 1 in the article: Li, X., Zhang, C., Li, W., Ricard, R., Meng, Q., & Zhang, W. (2015). Assessing street-level urban greenery using Google Street View and a modified green view index. Urban Forestry & Urban Greening, 14(3), 675-685.
        
        #H. We added "Duration_Before_Ex" that came manualy from "Notes" excell, indicating the pre-experiment duration time that the subject stayed in the place. 
        
        #I. For the statistical part we aggregated the Thermal answers from 5\7 bins into 3 bins (using excell if statements). That is in Comfort: 4,3 = 3 | 2 = 2 | 1,0 = 1. In Sensation: 0,-1,-2 = 0 | 1 = 1 | 3,2 =2. In Preference: -2,-3 = -2 | -1 = -1 | 0,1,2,3 = 0. It is crusial to note that we devided the groups to those bins base on the frequancy of each thermal score.
        
        #J. For future "outlier" testing we calculated by the Z-score method us suggested: Rincón-Martínez, J. C. (2023). Basic methods used for data analysis in adaptive thermal comfort studies. Ingeniería, investigación y tecnología, 24(1). We calculated them to the: SCL scores\PET\thermal assasements.

        #K. We then cleaned the un-named columns and opened a new sheet "cleaner".

##16 'General data and Experiments' - in this notebook you will find general calculations for descriptive statistics, algorithm for finding vegetation groups devidings (implemented in R in the next step - 17) and experiments on the data to see general correlations.

##17 'Ordinal_Regression' - R studio file. 
        
        ‏









