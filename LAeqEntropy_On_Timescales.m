%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This is the basic code to calculate LAeq entropy from dosimeter readings
% within different subdivisions of time. LAeq entropy is treated as a
% measure of LAeq diversity. Entropy is calculated from the probability
% density estimates of LAeq for each participant. 
%
% In this code, the entropy is computed for timeScale A, timeScale B, and
% timescale C. However, this can be modified for any timescale (e.g., Fall,
% Winter, Spring, or Monday, Tuesday, etc.
%
% A couple of things:
% 
% If you want to use other timescales like seasons, etc., you need define
% these in your dataset first. There is an accompanying R code on our
% Github with some code to this for specific seasons and for days of the
% week. If you want to use different seasons from different years etc., you
% can modify that code, add the timescales to the data, and then use this
% code to calculate the entropy. 
%
% The discrete entropy is calculated. To do this, the bin size used is 3
% dB. This can be changed it desired.
%
% The ignoreZero bit is because dosimeters typically have a threshold, such
% that values below some level (e.g., instantaneous levels below 75 dB or LAeq
% values below 40 dB) are recorded as zero. This means the distribution
% takes on a binomial character if you include them. This results in
% entropy estimates that could be hard to interpret. However, you can
% include them if you want. Just specifiy ignoreZero = 0 if you want that.
% However, you also need to change these values to something that is
% non-zero, like 1, because log(0) is undefined. 
%
% Code is provided "as-is" without any warranties or guarantees of any kind. 
% The user assumes full responsibility for using this code and any outcomes 
% resulting from its use. The code is provided without any form of support 
% maintenance, or updates. Use at your own risk.
%
% Erik Jorgensen, AuD, PhD
% UW-Madison, 2024
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load data

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if you want to only include non-zero LAeq values, specify ignoreZero as 1
% if you want to include both, specify ignoreZero as 0
ignoreZero = 1;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%subset data if ignoreZero == 1

if ignoreZero == 0

    data = allcombinedNotF;

elseif ignoreZero == 1

    nonZeroRows = allcombinedNotF.LAeq ~= 0;
    data = allcombinedNotF(nonZeroRows, :);
end

numSub = max(data.ID); %get number of subjects

minBin = round(min(data.LAeq)); %get low bound
maxBin = round(max(data.LAeq)); %get high bound
binEdges = minBin:3:maxBin; %create vector of bin edges size 3 dB

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%timeScale A%%%%%%%%%%%%%%%%%%%%%%%

indexA = data.timeScale == 'A';
dataA = data(indexA, :); % Extract rows using the logical index

%pre-allocate
densityxA = zeros(numSub, 100);
densityyA = zeros(numSub, 100);
entropyA = zeros(numSub, 1);
laeqAmeans = zeros(numSub, 1);

%loop through subjects and plot densities

for ii = 1:numSub

subject = dataA.ID==ii; %index subject

    if isempty(dataA(subject,:)) %check for data
        disp(['No data for index ', num2str(ii)]);
        continue;  %skip this iteration
    end

subject_data = dataA(subject,:); %get data for ii subject

subject_laeq = subject_data(:,2); %just get LAeq
subject_laeq = table2array(subject_laeq); %make array

laeqAmeans(ii) = mean(subject_laeq); %get laeq mean for each subject

%Here we encounter a problem because log(0) is undefined and negative SPLs for KDEs aren't interpretable. 
%To include 0 LAeq values, replace 0 with a different value. 

if ignoreZero == 0

    replaceZero = 35; %this is arbitrary - must be less than 40
    subject_laeq(subject_laeq(:, 1) == 0, 1) = replaceZero;
else
end

[f, xi] = ksdensity(subject_laeq); %estimate the density at 100 points

densityxA(ii,:) = xi; %save the density estimates for all subjects
densityyA(ii,:) = f; %ditto

%%%%%%%%%%%%%%%%%%
%calculate entropy

[counts, binIndices] = histcounts(subject_laeq, binEdges); 
sub_len = length(subject_laeq); %get length
probabilityDensity = counts./sub_len;

check = sum(probabilityDensity); %check, should = 1
disp(['Check ', num2str(check)]);

logplaeqProps = probabilityDensity .* log2(probabilityDensity);
logplaeqProps = logplaeqProps(~isnan(logplaeqProps)); %remove NaN
p_sum = sum(logplaeqProps);
entropyA(ii,:) = -(p_sum);


end

%figure(1);
subplot(2,2,1)
for ii = 1:numSub

    if densityxA(ii,1) == 0
        disp(['No data for index ', num2str(ii)]);
        continue;  %skip this iteration
    end

    plot(densityxA(ii,:),densityyA(ii,:)) %plot the density
    hold on;
    ylim([0 .12]);
    xlim([0 150]);
    xlabel("LAeq (dB)");
    ylabel('KDE');
    title('LAeq KDE: timeScale A')
end


%timeScale B%%%%%%%%%%%%%%%%%%%%%%%

indexB = data.timeScale == 'B';
dataB = data(indexB, :); % Extract rows using the logical index

%pre-allocate
densityxB = zeros(numSub, 100);
densityyB = zeros(numSub, 100);
entropyB = zeros(numSub, 1);
laeqBmeans = zeros(numSub, 1);

%loop through subjects and plot densities

for ii = 1:numSub

subject = dataB.ID==ii; %index subject

    if isempty(dataB(subject,:)) %check for data
        disp(['No data for index ', num2str(ii)]);
        continue;  %skip this iteration
    end

subject_data = dataB(subject,:); %get data for ii subject

subject_laeq = subject_data(:,2); %just get LAeq
subject_laeq = table2array(subject_laeq); %make array

laeqBmeans(ii) = mean(subject_laeq); %get laeq mean for each subject

%Here we encounter a problem because log(0) is undefined and negative SPLs for KDEs aren't interpretable. 
%To include 0 LAeq values, replace 0 with a different value. 

if ignoreZero == 0

    replaceZero = 35;
    subject_laeq(subject_laeq(:, 1) == 0, 1) = replaceZero;
else
end

[f, xi] = ksdensity(subject_laeq); %estimate the density at 100 points

densityxB(ii,:) = xi; %save the density estimates for all subjects
densityyB(ii,:) = f; %ditto

%%%%%%%%%%%%%%%%%%
%calculate entropy

[counts, binIndices] = histcounts(subject_laeq, binEdges); 
sub_len = length(subject_laeq); %get length
probabilityDensity = counts./sub_len;

check = sum(probabilityDensity); %check, should = 1
disp(['Check ', num2str(check)]);

logplaeqProps = probabilityDensity .* log2(probabilityDensity);
logplaeqProps = logplaeqProps(~isnan(logplaeqProps)); %remove NaN
p_sum = sum(logplaeqProps);
entropyB(ii,:) = -(p_sum);


end

%figure(2);
subplot(2,2,2)
for ii = 1:numSub

    if densityxB(ii,1) == 0
        disp(['No data for index ', num2str(ii)]);
        continue;  %skip this iteration
    end

    plot(densityxB(ii,:),densityyB(ii,:)) %plot the density
    hold on;
    ylim([0 .12]);
    xlim([0 150]);
    xlabel("LAeq (dB)");
    ylabel('KDE');
    title('LAeq KDE: timeScale B')
end

%timeScale C%%%%%%%%%%%%%%%%%%%%%%%

indexC = data.timeScale == 'C';
dataC = data(indexC, :); % Extract rows using the logical index

%pre-allocate
densityxC = zeros(numSub, 100);
densityyC = zeros(numSub, 100);
entropyC = zeros(numSub, 1);
laeqCmeans = zeros(numSub, 1);

%loop through subjects and plot densities

for ii = 1:numSub

subject = dataC.ID==ii; %index subject

    if isempty(dataC(subject,:)) %check for data
        disp(['No data for index ', num2str(ii)]);
        continue;  %skip this iteration
    end

subject_data = dataC(subject,:); %get data for ii subject

subject_laeq = subject_data(:,2); %just get LAeq
subject_laeq = table2array(subject_laeq); %make array

laeqCmeans(ii) = mean(subject_laeq); %get laeq mean for each subject

%Here we encounter a problem because log(0) is undefined and negative SPLs for KDEs aren't interpretable. 
%To include 0 LAeq values, replace 0 with a different value. 

if ignoreZero == 0

    replaceZero = 35;
    subject_laeq(subject_laeq(:, 1) == 0, 1) = replaceZero;
else
end

[f, xi] = ksdensity(subject_laeq); %estimate the density at 100 points

densityxC(ii,:) = xi; %save the density estimates for all subjects
densityyC(ii,:) = f; %ditto

%%%%%%%%%%%%%%%%%%
%calculate entropy

[counts, binIndices] = histcounts(subject_laeq, binEdges); 
sub_len = length(subject_laeq); %get length
probabilityDensity = counts./sub_len;

check = sum(probabilityDensity); %check, should = 1
disp(['Check ', num2str(check)]);

logplaeqProps = probabilityDensity .* log2(probabilityDensity);
logplaeqProps = logplaeqProps(~isnan(logplaeqProps)); %remove NaN
p_sum = sum(logplaeqProps);
entropyC(ii,:) = -(p_sum);



end

%figure(3);
subplot(2,2,3)
for ii = 1:numSub

    if densityxC(ii,1) == 0
        disp(['No data for index ', num2str(ii)]);
        continue;  %skip this iteration
    end

    plot(densityxC(ii,:),densityyC(ii,:)) %plot the density
    hold on;
    ylim([0 .12]);
    xlim([0 150]);
    xlabel("LAeq (dB)");
    ylabel('KDE');
    title('LAeq KDE: timeScale C')
end

%%%%%%%%%%%%%%%
%plot some other things

%figure(4); %scatter plot entropy for each timeScale for each subject
subplot(2,2,4)
x = 1:numSub;
scatter(x,entropyA,'filled', 'DisplayName', 'A') %plot entropy values
hold on;
scatter(x,entropyB, 'filled','DisplayName', 'B') %plot entropy value
hold on;
scatter(x,entropyC, 'filled', 'DisplayName', 'C') %plot entropy value
xlabel("Subject");
ylabel('Entropy');
%ylim([0.0001 0.045]);
title('LAeq Entropy')
legend('Location', 'best')
box on;

laeqAmeans = array2table(laeqAmeans);
laeqBmeans = array2table(laeqBmeans);
laeqCmeans = array2table(laeqCmeans);

columnNames = {'laeqMean', 'timeScale'};

timeScaleLab = repmat("A", height(laeqAmeans), 1); 
laeqAmeans.timeScale = timeScaleLab;
laeqAmeans.Properties.VariableNames = columnNames;
timeScaleLab = repmat("B", height(entropyB), 1); 
laeqBmeans.timeScale = timeScaleLab;
laeqBmeans.Properties.VariableNames = columnNames;
timeScaleLab = repmat("C", height(entropyC), 1); 
laeqCmeans.timeScale = timeScaleLab;
laeqCmeans.Properties.VariableNames = columnNames;
laeqMeanscombined = vertcat(laeqAmeans, laeqBmeans, laeqCmeans);

laeqMeanscombined = laeqMeanscombined(laeqMeanscombined.laeqMean ~= 0, :);

figure(2)
subplot(1,2,1)
boxplot(laeqMeanscombined.laeqMean, laeqMeanscombined.timeScale) %boxplot LAeq for each timeScale
hold on;
% Customize the plot
xlabel("timeScale");
ylabel("LAeq (dB)");
title("LAeq by timeScale");

entropyA = array2table(entropyA);
entropyB = array2table(entropyB);
entropyC = array2table(entropyC);

columnNames = {'entropy', 'timeScale'};

timeScaleLab = repmat("A", height(entropyA), 1); 
entropyA.timeScale = timeScaleLab;
entropyA.Properties.VariableNames = columnNames;
timeScaleLab = repmat("B", height(entropyB), 1); 
entropyB.timeScale = timeScaleLab;
entropyB.Properties.VariableNames = columnNames;
timeScaleLab = repmat("C", height(entropyC), 1); 
entropyC.timeScale = timeScaleLab;
entropyC.Properties.VariableNames = columnNames;
entropyCombined = vertcat(entropyA, entropyB, entropyC);

entropyCombined = entropyCombined(entropyCombined.entropy ~= 0, :);

subplot(1,2,2)
boxplot(entropyCombined.entropy, entropyCombined.timeScale)
xlabel("timeScale");
ylabel("Entropy (H of LAeq)");
title("LAeq Entropy by timeScale");


