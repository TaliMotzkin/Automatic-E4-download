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

##13 'Merging the questionnaires' - merging to each subject number its demographical data like: Age, Hight..










