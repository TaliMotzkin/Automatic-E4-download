# Thesis analysis

##First step - Download bracelets from web
This is an automatic download of multiple zip files wich contains E4 data, Using Selenium python library.

Open the ipynb file using jupyter notebook.
In block 3 - change to the desired diractory.
In block 5 - add your mail and password.
In block 6 and so on - change to the desired dates.

##2 - Unleash the zip
extracting files from zips and renameing them accordingly.
follow the comments in the code.
Using Zip and os libraries.

##3- Adding subject number
The porpuse of this code is to find for each documented subject (322 observations) its suitible files, through the notes.xlsx file (my documentation for the experiment).
The output will be the bracelet files with the subjects number (indicates Primery Key). Using Pandas and os libraries. Also new notes file has created: "new_notes.xlsx".

##4- Adding the Duration time, real Ending and Beginning time of each experiment to the main table
By the Unix time of each EDA file (which I extracted), decided when the real measurment has begun, ended and the duration of the experiment. This EDA data was also inserted to the main notes table, now called:"new_notes_1.xlsx". Using Pandas, os, datetime libraries.





