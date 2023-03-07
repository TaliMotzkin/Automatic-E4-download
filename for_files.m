myDir = uigetdir; %gets directory
myFiles = dir(fullfile(myDir, "*.mat")); %gets all mat files in struct


for i = 1:length(myFiles)
    mat = load(myFiles(i).name, 'analysis');
    [ filepath , name , ext ] = fileparts(myFiles(i).name)
    newStr = strcat(strrep(name,"EDA","SCL"), "_bin") %or mul
    xlswrite(newStr,mat.analysis.tonicData')
end
