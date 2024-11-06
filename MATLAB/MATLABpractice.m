% Script Name : MATLABPractice.m
% Created on : 04/04/2023
% Author : Mary Dickinson
% Purpose : Variety of different skills learnt in MATLAB, including...
% Linear equations and derivatives
% Drug decay, loops and if statements 
% Learning reward tasks
% Signal processing and feature extraction (e.g. mismatch negativity in EEG
% data)
% Simulated EEG signal processing and time-series analysis 

clear all
close all
clc

%% 

clear all 
close all 

% Plotting f(x) = 0.2x^5 − 3x^2 + 6x + 12...

% Defining parameters...
x = -1:0.001:1;                      % Creating a vector x ranging from -1 to 1 at with intervals of 0.001
y = 0.2.*x.^5 - 3.*x.^2 + 6.*x + 12; % Calculating f(x) for the values in vector x

% Plot for f(x)...
figure
subplot(2,1,1)                                                 % Creating subplot to plot functions next to eachother
plot(x, y, "r", "LineWidth", 2)                                % Plotting red line for f(x)
xlabel("x")                                                    % Labelling x axis
ylabel("f(x)")                                                 % Labelling y axis
title("f(x)")                                                  % Adding title of function
text(-0.4, 14, "f(x) = 0.2x^5 − 3x^2 + 6x + 12", "color", "r") % Adding tet to label function
ylim([0 20])                                                   % Setting y axis limits
grid on                                                        % Adding grid 

% Plotting the derivative of f(x)...

% Defining parameters for derivative of f(x) (f'(x)), where f'(x) = x^4 - 6x + 6...
dy = gradient(y, 0.001); % Calculating f'(x) for values in vector x using gradient function at same intervals as x vector

% Plot for f'(x)...
subplot(2,1,2)                                      % Calling subplot
plot(x, dy, "b", "LineWidth", 2)                    % Plotting blue line for f'(x) 
xlabel("x")                                         % Labelling x axis
ylabel("f'(x)")                                     % Labelling y axis
title("Derivative of f(x)")                         % Adding title of derivative function
text(0.25, 6, "f'(x) = x^4 - 6x + 6", "color", "b") % Adding text to label derivative function
ylim([0 20])                                        % Setting y axis limits
grid on                                             % Adding grid 

%% 
% Calculating time it takes for 50mg of drug D to reach 10mg with decay rate of 0.3...

clear all 
close all

% Defining parameters and using formula...
t = 0:10;           % Defining time vector (t) in hours across 10 days 
C_0 = 50;           % Initial dose (C_0) of 50 mg
k = 0.3;            % Decay rate (k) of 0.3
C = C_0*exp(-k.*t); % Formula for drug depletion during metabolism in body 

% Locating time when drug D reaches 10mg...
days = find(C < 10);                          % 10mg is reached by day 7
fprintf("Days to reach 10mg: %d\n", days(1)); % Displaying day where 10mg is reached in command window

% Plotting decay rate across 10 particpants with random variation between 0.25 and 0.35...

% Generating 10 random numbers in the interval (0.25 , 0.35) with the formula r = a + (b-a).*rand(N,1)...
a = 0.25;                        % Start of random integer range
b = 0.35;                        % End of random integer range 
N = 10;                          % Number of participants 
Krand = a + (b - a).*rand(N, 1); % Using formula for random number generation 

% Using formula and parameters from earlier with new decay rate Krand...
Cx = zeros(11, N);       % Creating empty vector for Cx with 10 rows for particpants and 11 columns for time
Cx = C_0*exp(-Krand.*t); % Formula for drug depletion during metabolism in body 

% Plotting decay rates for the 10 participants in subplot...
figure 
sgtitle("Decay of Drug D Across the 10 Participants", "fontweight", "bold") % Adding title for whole figure of subplots 

for i = 1:N               % Using loop to plot subplots of participant data from the Cx matrix
    subplot(N/2,2,i)      % Calling the subplot number for each participant
    plot(t, Cx(i,:))
    xlabel("Time (days)") % Labelling x axis
    ylabel("Decay (mg)")  % Labelling y axis
    title(i)              % Adding title
    ylim([0 55])          % Setting y axis limits
    grid on               % Adding grid
end

% Plotting decay rates for 10 participants on one graph to find time to reach 10mg...
figure
plot(t, Cx)                                               % Plotting decay rates for each participants against time
xlabel("Time (days)")                                     % Labelling x axis
ylabel("Decay (mg)")                                      % Labelling y axis
title("Decay of Drug D across 10 participants")           % Adding title 
ylim([0 55])                                              % Setting y axis limits
hold on 
yline(10, "r")                                            % Line at 10mg 
grid on                                                   % Adding grid
legend({"Participant 1","Participant 2", ...
    "Participant 3","Participant 4","Participant 5", ...
    "Participant 6", "Participant 7","Participant 8", ...
    "Participant 9","Participant 10", "y = 10mg"}, ...
    "Location","northeast","Orientation","vertical")      % Adding legend to label each line 

% For minimum amount of days to reach 10mg...
for i = 1:length(Cx)
    if sum(Cx(:,i) < 10) % Going through columns to see if element is less than 10
        mint = i;        % Finding the minimum amount of days it takes to reach 10mg
        break            % Breaking loop after first element is below or equal to 10mg
    end
end
      
[minamount, minparticipant] = min(Cx(:,mint));  % The amount of drug left (minamount) and the participant number (minparticipant) at the minimum amount of days to reach 10

fprintf("Shortest whole days to reach 10mg: %d\n", mint);                                              % Printing the shortest amount of days in command window
fprintf("Amount of drug D remaining at the shortest whole days to reach 10mg: %d\n",  minamount);      % Printing the amount of drug remianing after the minimum amount of days to reach 10mg
fprintf("Participant number which took the shortest whole days to reach 10mg: %d\n",  minparticipant); % Printing participant number which took the shortest amount of time to reach 10mg 

% For maximum amount of days to reach 10mg...
for i = 1:length(Cx)
    if sum(Cx(:,length(Cx) - i) > 10) % Going through columns backwards from 11 to 1 to see if element is more than 10
        maxt = (length(Cx) - i) + 1;  % Finding the first amount of days from the end where it is more than 10mg, therefore the longest amount of days (+ 1 to find the day where 10mg is reached)
        break                         % Breaking loop after first element is above 10mg
    end
end
      
[maxamount, maxparticipant] = max(Cx(:,maxt));  % The amount of drug left (maxamount) and the participant number (maxparticipant) at the maximum amount of days to reach 10mg

fprintf("Longest whole days to reach 10mg: %d\n", maxt);                                               % Printing the longest amount of days in command window
fprintf("Amount of drug D remaining at the longest whole days to reach 10mg: %d\n",  maxamount);      % Printing the amount of drug remianing after the maximum amount of days to reach 10mg
fprintf("Participant number which took the longest whole days to reach 10mg: %d\n",  maxparticipant); % Printing participant number which took the longest amount of time to reach 10mg 

%% 

clear all
close all

% Define parameters for learning reward task...
ntrials = 100; % 100 trials with possible reward

trialreward = zeros(ntrials, 1); % Creating empty vector to store rewards in
x = randperm(100);               % Returns numbers up to 100 in random order 
y = x(1:25);                     % 25 trials rewarded £10
trialreward(y) = 10;             
z = x(26:50);                    % 25 trials rewarded £15
trialreward(z) = 15;
% 50 remaining random trials left with no reward

trialreward(101:200) = 0; % Adding 100 trails with no reward CS following the initial 100 with reward

% Phenotype 1: Depression learning rates...
Dep_alphapos = 0.2; % Low learning rate for better expected outcomes 
Dep_alphaneg = 0.6; % High learning rate for worse expected outcomes
% These values were chosen due to patterns of pessimistic behaviour providing higher punishment learning rates
% (Huang, Thompson & Paulus, 2017).

% Phenotype 2: Anxiety learning rates...
Anx_alphapos = 0.3; % Low learning rate for better expected outcomes
Anx_alphaneg = 0.8; % High learning rate for worse expected outcomes
% These values were chosen  due to patterns of pessimistic behaviour providing higher punishment learning rates, 
% and higher learning rates overall for those with anxiety as they percieve the world as highly
% changeable (Huang, Thompson & Paulus, 2017; Aylward et al., 2019). 

% Experiment for Phenotype 1: Depression...
Dep_v = zeros(1, length(trialreward)); % Setting the initial expected reward values to zero across all trials

for i = 2:length(trialreward)                       % Starting at the second trial

    delta = trialreward(i) - Dep_v(i-1);            % RPE = trial reward - expected reward

    if delta > 0
        Dep_v(i) = Dep_v(i-1) + Dep_alphapos*delta; % Update expected value for better expected outcomes
    else
        Dep_v(i) = Dep_v(i-1) + Dep_alphaneg*delta; % Update expected value for worse expected outcomes
    end

end

% Experiment for Phenotype 2: Anxiety...
Anx_v = zeros(1, length(trialreward)); % Setting the initial expected reward values to zero across all trials

for i = 2:length(trialreward)                       % Starting at the second trial

    delta = trialreward(i) - Anx_v(i-1);            % RPE = trial reward - expected reward

    if delta > 0
        Anx_v(i) = Anx_v(i-1) + Anx_alphapos*delta; % Update expected value for better expected outcomes
    else
        Anx_v(i) = Anx_v(i-1) + Anx_alphaneg*delta; % Update expected value for worse expected outcomes
    end

end

% Plotting expected value for both Phenotype 1: Depression and Phenotype 2: Anxiety...
figure
subplot(3, 1, 1)
plot(Dep_v, "r")                   % Plotting depression experiment in red
xlabel("Trials")                   % Labelling x axis for trial number 
ylabel("Expected Reward (£)")      % Labelling y axis for expected reward
title("Expected Value in Depression") % Adding title 
ylim([-0.2 15])                    % Setting y axis limits
grid on                            % Adding grid

subplot(3, 1, 2)
plot(Anx_v, "b")                   % Plotting anxiety experiment in blue
xlabel("Trials")                   % Labelling x axis for trial number 
ylabel("Expected Reward (£)")      % Labelling y axis for expected reward
title("Expected Value in Anxiety") % Adding title 
ylim([-0.2 15])                    % Setting y axis limits
grid on                            % Adding grid

subplot(3, 1, 3)
plot(Dep_v, "r")                                                 % Plotting depression experiment
hold on 
plot(Anx_v, "b")                                                 % And plotting anxiety experiment on same graph for comparison of phenotypes
xlabel("Trials")                                                 % Labelling x axis for trial number 
ylabel("Expected Reward (£)")                                    % Labelling y axis for expected reward
title("Comparing Expected Value between Depression and Anxiety") % Adding title 
ylim([-0.2 15])                                                  % Setting y axis limits
grid on                                                          % Adding grid
legend({"Depression Phenotype", "Anxiety Phenotype"})

%%  

clear all
close all

% Loading data for mismatch negativity paradigm...
load("Fz_data.mat");
amp = Fz_data(:, 1);  % First column is Fz channel EEG recording amplitude (uV)
time = Fz_data(:, 2); % Second column is time of sample (msec)
tone = Fz_data(:, 3); % Third column is onset of tone (standard tone = 1, deviant tone = 2)

% Extracting standard onset times...
onsets = find(Fz_data(:, 3) ~= 0); % Finding the positions of the tones 

% Separating the data into epochs...
for i = 1: length(onsets) % Creating onsets for epochs
    window(i,:) = onsets(i) - 50: onsets(i) + 280; % Creating each row as a time window of onsets
end

% Using onsets to epoch the amplitudes...
[nrows, ncolumns] = size(window); % Creating variables for size of windows

for i = 1:nrows % Epoching the amplitudes using a nested loop going through each rows and each column
    for ii = 1:ncolumns
        n = window(i, ii);
        epoched(i, ii) = Fz_data(n, 1); 
    end
end

% Baseline corrections for epoched data...
for i = 1:length(epoched)
    mnamp = mean(epoched(i, 1:50));        % Creating variable for mean to use in corrections
    epochdata(i,:) = epoched(i,:) - mnamp; % Creating epoched data 
end

% Assigning the epoched data to standard tone and deviant tone matrix...
p = 1; % Initialising indicies p and q for loop
q = 1;

for i = 1:nrows 
    x = onsets(i);
    if Fz_data(x,3) == 1                  % Data for standard tones added to new matrix standard
        standard(p, :) = epochdata(i, :);
        p = p + 1;    
    elseif Fz_data(x, 3) == 2             % Data for deviant tones added to new matrix deviant 
        deviant(q, :) = epochdata(i, :);
        q = q + 1; 
    end
end

% a) Plotting first 10 standard tones...
latency = ([-50:280]); % Setting latency between -50 and 280 ms

figure 
for i = 1:10                         % Looping through and plotting each tone
    plot(latency, standard(i,:))     % Plotting time against amplitude of waves
    xlim([-50 280])                  % Setting x axis limits
    title("First 10 Standard Tones") % Adding title
    xlabel("Latency (ms)")           % Labelling x axis 
    ylabel("Amplitude (uv)")         % Labelling y axis
    grid on                          % Adding grid
    hold on 
end

% Plotting the average amplitude for the first 10 standard tones...
first10 = standard(1:10,:); % Separating the first 10 standard tones (rows) into a new matrix 
avgfirst10 = mean(first10); % Finding the average amplitude of the first 10 standard tones

figure
plot(latency, avgfirst10, "r")              % Plot with red line
xlabel("Latency (ms)")                      % Labelling x axis for latency in ms 
ylabel("Amplitude (uV)")                    % Labelling y axis for amplitude in uV
title("Average of First 10 Standard Tones") % Adding title
ylim([-20 20])                              % Setting y axis limits
xlim([-50 280])                             % Setting x axis limits
grid on                                     % Adding grid

% b) Plotting final 10 standard tones...
figure
for i = length(standard)-9:length(standard) % Looping through and plotting each tone
    plot(latency, standard(i,:))            % Plotting time against amplitude of waves
    xlim([-50 280])                         % Setting x axis limits
    title("Final 10 Standard Tones")        % Adding title
    xlabel("Latency (ms)")                  % Labelling x axis 
    ylabel("Amplitude (uv)")                % Labelling y axis
    grid on                                 % Adding grid
    hold on
end 

% Plotting the average amplitude for the final 10 standard tones...
final10 = standard(length(standard)-9:length(standard),:); % Separating the first 10 standard tones (rows) into a new matrix 
avgfinal10 = mean(final10);                                % Finding the average amplitude of the first 10 standard tones

figure
plot(latency, avgfinal10, "b")              % Plot with blue line
xlabel("Latency (ms)")                      % Labelling x axis for latency in ms 
ylabel("Amplitude (uV)")                    % Labelling y axis for amplitude in uV
title("Average of Final 10 Standard Tones") % Adding title
ylim([-20 20])                              % Setting y axis limits
xlim([-50 280])                             % Setting x axis limits
grid on                                     % Adding grid

% c) Calculating the MMN...
avgstandard = mean(standard); % Calculating average for standard tones
avgdeviant = mean(deviant);   % Calculating average for deviant tones

MMN = avgstandard - avgdeviant; % Calculating MMN as differences between average of standard and deviant tones

% Plotting the MMN...
figure
plot(latency, MMN, "g")  % Plot with green line
xlabel("Latency (ms)")   % Labelling x axis for latency in ms 
ylabel("Amplitude (uV)") % Labelling y axis for amplitude in uV
title("MMN")             % Adding title
ylim([-20 20])           % Setting y axis limits
xlim([-50 280])          % Setting x axis limits
grid on                  % Adding grid

%% 

clear all
close all

% Define the time properties...
MaxT = 0.1;    % Maximum amount of time
Ti = 0.01;     % Time interval
t = 0:Ti:MaxT; % Time axis

% Creating frequency and amplitude components for controls and patients...
f = [0 8 14 27 48 49]; % Selecting my own frequencies within delta (2-4 Hz), theta (4-8Hz), alpha (8-16 Hz), beta (16-32Hz) and gamma (32-64Hz and beyond)

cont_amp = [-0.26 0.52 0.07 0.12 -0.09 0.06]/5; % Setting strengths of frequency waves for controls 
pat_amp = [-0.26 0.45 0.07 0.11 -0.08 0.07]/5;  % Setting strengths of frequency waves for schizophrenia patients with reduced beta oscillations and enhanced theta and gamma oscillations 

% Transforming Hz to rad per sec...
omega = 2*pi*f; % Radial frequency for each frequency

% Create the sin wave signal using wave = A*sin(2*pi*f*t) for controls...
for i = 1:length(f)                                   % Cycling over theta, alpha, beta and gamma
   cont_EEGsignal(i,:) = cont_amp(i)*sin(omega(i)*t); % Creating a wave = A* sin(2*pi*f*t) for each frequency band(i)*sin(omega(i)*t)
end

% Create the sin wave siganl using same method for patients...
for i = 1:length(f)                                  % Cycling over theta, alpha, beta and gamma
   pat_EEGsignal(i,:) = pat_amp(i)*sin(omega(i)*t);  % Creating a wave = A* sin(2*pi*f*t) for each frequency band(i)*sin(omega(i)*t)
end

% Adding noise to each signal for controls...
noise = 0.001; % Different noise for each frequency

for i = 1:length(f)                                                     % Cycling over theta, alpha, beta, gamma
   cont_EEGnoisy(i,:) = cont_EEGsignal(i,:) + randn(1,length(t))*noise; % Adding noise 
end

% Adding noise to each signal for patients...
for i = 1:length(f)                                                   % Cycling over theta, alpha, beta, gamma
   pat_EEGnoisy(i,:) = pat_EEGsignal(i,:) + randn(1,length(t))*noise; % Adding noise 
end

% Adding the waves together to create a composite wave for controls...
cont_compositewave = sum(cont_EEGnoisy); % Adding matrices along the first dimension unless otherwise specified 

% Plotting control composite wave...
figure
subplot(2, 1, 1) 
plot(t, cont_compositewave)     % Plotting time against amplitude for control composite wave
xlabel("Time (sec)")            % Labelling x axis
ylabel("Amplitude (mV)")        % Labelling y axis
title("Control Composite Wave") % Adding title to subplot 
grid on                         % Adding grid 

% Adding the waves together to create a composite wave for patients...
pat_compositewave = sum(pat_EEGnoisy); % Adding matrices along the first dimension unless otherwise specified 

subplot(2, 1, 2)
plot(t, pat_compositewave)      % Plotting time against amplitude for patient composite wave
xlabel("Time (sec)")            % Labelling x axis
ylabel("Amplitude (mV)")        % Labelling y axis
title("Patient Composite Wave") % Adding title to subplot
grid on                         % Adding grid 

% Fourier Transform using fft and then fftshift for controls...
cont_yEEG = fft(cont_compositewave); % Returning the FT in the positive and negative frequency space
cont_zEEG = fftshift(cont_yEEG);     % Correcting the order of the fft putting negtaive frequencies to the left and positive frequencies to the right

% Fourier Transform using fft and then fftshift for patients...
pat_yEEG = fft(pat_compositewave); % Returning the FT in the positive and negative frequency space
pat_zEEG = fftshift(pat_yEEG);     % Correcting the order of the fft putting negtaive frequencies to the left and positive frequencies to the right

% Obtaining the frequency axis for positive ones...
Fs = 1/Ti;                       % Sampling rate = 1/time interval which is same for both 
ly = length(cont_yEEG);          % Length of signal from fft is same for both so only performing for one group
freq = (-ly/2 : ly/2 -1)/ly*Fs ; % Since the FFT returns negative and positive frequencies scaling for my frequency axis in Hz

posindexbegins = ceil(length(freq)/2);        % Fidning where the positive index begins     
posfreqindex = [posindexbegins:length(freq)]; % Indices to plot from the mid point to the end

% Defining new vectors for the restricted FFT values for the noisy control and patient EEGs...
cont_FFTmagEEG = abs(cont_zEEG(posfreqindex)); % Vector with just these FFT values for the noisy control EEG
pat_FFTmagEEG = abs(pat_zEEG(posfreqindex));   % Vector with just these FFT values for the noisy patient EEG
posfreqs = freq(posfreqindex);                 % New x-axis just comprising just the positive frequencies

% Plot the One-side of the Fourier Transform...
figure
plot(posfreqs , cont_FFTmagEEG ,"b","LineWidth", 3)               % Magnitude of this signal plotted against the positive frequencies for controls in blue
hold on
plot(posfreqs, pat_FFTmagEEG, "r", "LineWidth", 3)                % Magnitude of this signal plotted against the positive frequencies for patients in red
xlabel("Frequency (Hz)")                                          % Labelling x for frequency
ylabel("Power (A.U.)")                                            % Labelling y for power in arbitrary units 
title("Frequency Domain for Controls and Schizophrenia Patients") % Adding title 
xlim("tight")                                                     % Setting x limit to be tight around maximm and minimum coordinates 
grid on                                                           % Adding grid 
legend({"Control Group", "Scz Patient Group"})                    % Adding legend to label line
