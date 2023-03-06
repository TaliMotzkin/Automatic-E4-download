myDir = uigetdir; %gets directory
myFiles = dir(fullfile(myDir, "*.mat")); %gets all mat files in struct


for i = 1:length(myFiles)
    mat = load(myFiles(i).name, 'analysis');
    [ filepath , name , ext ] = fileparts(myFiles(i).name)
    newStr = strcat(strrep(name,"EDA","SCL"), "_bin")
    xlswrite(newStr,mat.analysis.tonicData')
end

%Ledalab('C:\Users\Tali\Documents\study\work\seminar\sumer 2022\Braclets raw\bracelet raw\mat_binary_July\', 'open', 'text2', 'analyze','CDA', 'optimize',1)



