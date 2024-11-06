% Script Name : MATLABShortQuestions.m
% Created on : 14/02/2023
% Author : Mary Dickinson
% Purpose : Short questions for...
% Matrix properties
% Creating functions for neuron firing

clear all
close all
clc

% Q1
% Making 3x3 matrix A...
A = [-1 -2 1; -3 -1 7; 6.5 -8 2];

% Making matrix B...
B = [-1 2 4];

% Calculating C = A*(B*^T)...
T = randn(1); % Random number for T as not given 

C = A.*(B.^(T));

% Calculating X = A^-1...
X = A^-1; 

% Showing X is inverse of A using inv() function to inverse matrix...
invA = inv(A); 

disp(invA); % Displaying inverse of A elements
disp(X);    % Displaying X elements
% Both matrices and their corresponding elements are the same.

% Q2 

clear all
close all

% Creating 9x5 matrix Z of random numbers...
 Z = randn(9,5);

% Loop to identify numbers greater than or equal to -0.1...
N = 9*5; % Define the length of Z = 45 

for i = 1:45              % Creating loop for length of Z
    if Z(i) <= -0.1       % If element is less than or equal to -0.1  
       Z(i) = exp(Z(i));  % Replace element with exponential of the number
       disp(Z(i));        % Display the new element 
    else  
       disp(Z(i))         % If not, display old element 
       disp("Element of matrix Z is > -0.1."); 
    end                   % End if function
end                       % End loop

disp(Z) % Displaying new Z matrix 

% Q3 

clear all
close all 

% Defining stable variables across both cases...
alpha = -4; 
theta = -40;

% i) Creating matrix for the first case of x...
x1 = [ones(1, 10)*-20 ones(1, 10)*-60]; % -20 for 10 elements followed by -60 for 10 elements

% Plotting x1 to time...
t1 = 1:length(x1); % 20 time points in first case

figure
plot(t1, x1, "LineWidth", 5) % Creating line graph for first case of x against time 
grid on                     % Adding grid
title("First Case of X")    % Adding title 
xlabel("Time")              % Labelling x axis
ylabel("x")                 % Labelling y axis 

% Calculating probability of firing, S(x), for first case of x...
% s1 = 1./(1 + exp(1).^(-alpha.*(x1 - theta)))
s1 = my_sigmoid_function(x1, alpha, theta); % Using my sigmoid function (at end of script) to calculate probability of firing for x1

figure
plot(x1, s1, "LineWidth", 5)                  % Plotting sigmoid function for probability of firing against first case of x values
grid on                                       % Adding grid
title("Probability of Firing for Cases of x") % Adding title 
xlabel("x")                                   % Labelling x axis 
ylabel("Probability of Firing, S(x)")         % Labelling y axis 

% ii) Creating matrix for second case of x...
t2 = 1:200;                     % 200 time points in second case 
t2a = 1:100;                    % first 100 time points in second case 
t2b = 101:200;                  % last 100 time points in second case 
x2 = [20.*sin(t2a) 40.*sin(t2b)]; % 20sin( for 100 elements followed by 40sin(1) for 100 elements

% Plotting x2 to time..
figure
plot(t2, x2, "LineWidth", 5) % Creating line graph for second case of x against time 
grid on                     % Adding grid
title("Second Case of X")   % Adding title 
xlabel("Time")              % Labelling x axis
ylabel("x")                 % Labelling y axis 

% Calculating probability of firing, S(x), for second case of x...
% s2 = 1./(1 + exp(1).^(-alpha.*(x2 - theta)))
s2 = my_sigmoid_function(x2, alpha, theta); % Using my sigmoid function to calculate probability of firing for x2

figure
plot(x2, s2, "LineWidth", 5)                  % Plotting sigmoid function for probability of firing against first case of x values
grid on                                       % Adding grid
title("Probability of Firing for Cases of x") % Adding title 
xlabel("x")                                   % Labelling x axis 
ylabel("Probability of Firing, S(x)")         % Labelling y axis 

% My sigmoid function...
function s = my_sigmoid_function(x, alpha, theta)

s = 1./(1 + exp(1).^(-alpha.*(x - theta)));

return
end





