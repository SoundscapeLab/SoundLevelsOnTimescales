# SoundLevelsOnTimescales
Code used in the paper "Examining sound levels across different timescales measured from body worn dosimeters" by Erik Jorgensen, Jennifer B. Tufts, &amp; Erika Skoe

Data is here: https://osf.io/tfvjc/

This repository has 3 scripts. The MATLAB file is used to calculate sound level entropy across different timescales (e.g., seasons, weeks, day). See the file for calculation details. The code assumes the data has many individual observations across the timescale, that the dependent variable is a dB measurement, and that the dataset contains a variable for the timescale (e.g., a column called Week with values A, B, and C). The R files are used to conduct all analyses of the data for each timescale in the paper (seasons, weeks, days, and across the day) for group and individuals. The code generates plots of group data, conducts mixed-effects regressions, and creates tables of the model outputs. The code also calculates odds and ends like compliance. 

Code is provided "as-is" without any warranties or guarantees of any kind. The user assumes full responsibility for using this code and any outcomes 
resulting from its use. The code is provided without any form of support maintenance, or updates. Use at your own risk.
