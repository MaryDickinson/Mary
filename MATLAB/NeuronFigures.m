% Script Name : NeuronFigures.m
% Created on : 17/10/2024
% Author : Mary Dickinson
% Purpose : Generate mean spike denisty and colormap figures for EEG data 

%%

% Loading data
clearvars; % clear all
load('holdoutdata.mat');
dat = holdoutdata;
clear holdout*

%%

% Initialising key variables

time = -500:750;  
condition_ids = 1:100;
hmiconfig.faces500 = 1:20;        % Condition numbers for face stimuli
hmiconfig.fruit500 = 21:40;       % Condition numbers for fruit stimuli
hmiconfig.places500 = 41:60;      % Condition numbers for place stimuli
hmiconfig.bodyp500 = 61:80;       % Condition numbers for body stimuli
hmiconfig.objct500 = 81:100;      % Condition numbers for object stimuli

% Prepare for plotting
num_conditions = length(condition_ids);
fontsize_sml = 8;

%% COLOUR PLOTTING

% Loop through each neuron
neuronNumbers = unique(dat(:,1));
numNeurons = length(neuronNumbers);

for nn=1:5 % numNeurons

    tempNeuronData = dat(dat(:,1)==neuronNumbers(nn), :);

    % Calculate average spike density function for each conditon
    temp_avgSpdenNeuron = nan(num_conditions, length(time));
    for cc = 1:num_conditions
        temp_avgSpdenNeuron(cc, :) = mean(tempNeuronData(tempNeuronData(:,2) == cc, 21:end), 'omitnan');
    end

    % Initialise figure
    figure; clf; cla;
    tiledlayout(1, 2, 'TileSpacing', 'loose', 'Padding', 'Compact');
    nexttile(1);
    set(gcf, 'Units', 'Normalized');
    set(gcf, 'Position', [0.1 0.1 0.8 0.8]);
    set(gca, 'FontName', 'Arial');
    plotorder= [hmiconfig.faces500 hmiconfig.fruit500 hmiconfig.places500 hmiconfig.bodyp500 hmiconfig.objct500];
    % Create a 2D pcolor plot
    pcolor(time, plotorder, temp_avgSpdenNeuron);
    shading flat;
    hold on
    colorbar('SouthOutside')

    % Add white lines between categories
    textLabels = {'Faces', 'Fruit', 'Places','BodyParts','Objects'};
    for sp = 20:20:100 % from 20 to 100 in step sizes of 20
        plot([time(1) time(end)], [sp sp], 'w-', 'LineWidth', 1);
        text(time(end) + 50, sp-10, textLabels{sp/20}, 'Rotation', 0, 'HorizontalAlignment', 'Left')
    end

    plot([0 0], [0 100], 'w-')
    xlabel('Time From Stimulus Onset (ms)')
    ylabel('Stimulus Number')
    title(['Neuron Colormap for Neuron ', num2str(neuronNumbers(nn))], 'FontSize', 14)

    nexttile(2);
    plot(-500:750,mean(dat(dat(:,1)==neuronNumbers(nn) & dat(:,3)==1, 21:end)),'k-') 
    hold on
    plot(-500:750,mean(dat(dat(:,1)==neuronNumbers(nn) & dat(:,3)==2, 21:end)),'b-')
    hold on
    plot(-500:750,mean(dat(dat(:,1)==neuronNumbers(nn) & dat(:,3)==3, 21:end)),'r-')
    hold on
    plot(-500:750,mean(dat(dat(:,1)==neuronNumbers(nn) & dat(:,3)==4, 21:end)),'g-')
    hold on 
    plot(-500:750,mean(dat(dat(:,1)==neuronNumbers(nn) & dat(:,3)==5, 21:end)),'m-')    
    xlabel('Time (ms)')
    ylabel('sp/s')
    title(['Spike Density Functions for Neuron ', num2str(neuronNumbers(nn))],'FontSize', 14)
    legend('Faces', 'Fruit', 'Places', 'Body', 'Object');

    savefig(['neuronPrintout_neuron', num2str(neuronNumbers(nn))])
    jpgfigname = ['~/Desktop/NEURON FIGURES/neuronPrintout_neuronNum', num2str(neuronNumbers(nn)), '.jpg'];
    print(gcf,jpgfigname,'-djpeg') % Generates an JPEG file of the figure
    clear temp*

end
