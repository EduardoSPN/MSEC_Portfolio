%ECON 612
%Assignment 2
%Eduardo Schiappa-Pietra Nieto

clc;
clear;
format compact;

%% PROBLEM 3

%%%%%%%%%%%%%% - PART A - iii - %%%%%%%%%%%%%%%%%%%%

phi=[-0.9; -0.5; 0; 0.5; 0.9];
omega = linspace(0,pi);
sigma_sq=1;
figure
title('Spectral Density')
xlabel('\omega')
ylabel('f(\omega)');
hold on
for i= 1:length(phi)
    for j= 1:length(omega)
        f(j,i)=(sigma_sq/(2*pi))*(1/(1+(phi(i)^2)-2*(phi(i))*(cos(omega(j)))));
    end
    plot(omega,f(:,i));
end
hold off

%%%%%%%%%%%%%% - PART B %%%%%%%%%%%%%%%%%%%%

delta=[-0.8; -0.4; 0; 0.4; 0.8];
omega = linspace(0,pi);
figure
title('Spectra ARMA(2,1)')
xlabel('\omega')
ylabel('f(\omega)');
hold on
for h= 1:length(delta)
    for j= 1:length(omega)
        num=(1+delta(h)*exp(-1i*omega(j)))*(1+delta(h)*exp(1i*omega(j)));
        denom=(1-0.4*exp(-1i*omega(j))-0.8*exp(-2*1i*omega(j)))*(1-0.4*exp(1i*omega(j))-0.8*exp(2*1i*omega(j)));
        sigma_sq= 0.2/(1+delta(h)^2);
        m(h,j)=(sigma_sq/(2*pi))*(num/denom);
    end
    plot(omega,m(h,:));

end
hold off;

%%%%%%%%%%%%%% - PART C  - i - %%%%%%%%%%%%%%%%%%%%

%Load Data
smpl_data = xlsread('Sample Data.xlsx');

%ACF
figure
title('ACF')
autocorr(smpl_data(:,2));

%PACF
figure
title('PACF')
parcorr(smpl_data(:,2));

%plot(smpl_data(:,2));

%%%%%%%%%%%%%% - PART C  - ii - %%%%%%%%%%%%%%%%%%%%
Y=smpl_data(:,2);

% nonparametric
autocov=xcov(Y,'biased');
start=(length(autocov)+1)/2;
lengthspan=length(autocov)-start; %Start at the median index
SPEC=[];

for omega=0:0.01:pi
temp1=cos(omega*[1:lengthspan]);
temp2=autocov(start+1:end);
SPEC=[SPEC;(sum(temp1'.*temp2)*2+autocov(start))];
end

figure
hold on
title('Non parametric Periodogram')
plot(0:0.01:pi,SPEC/2/pi)
hold off;

% Kernel smoothing
k= [1/16, 1/8, 3/16, 1/4, 3/16, 1/8, 1/16];
SPEC_W = zeros(size(SPEC,1),1);
SPEC_W(1:3)=SPEC(1:3);
SPEC_W(end-2:end)=SPEC(end-2:end);
for t=4:length(SPEC_W)-3
SPEC_W(t) = k*SPEC(t-3:t+3);
end

figure
hold on
title('Smoothed Non parametric Periodogram')
plot(0:0.01:pi,SPEC_W/2/pi)
hold off;

% parametric

% We fit an AR(2) which is the model we identified in i)
model1 = arima('ARLags', [1 2]);
EstModel1 = estimate(model1,Y);

omega=0:0.01:pi;

paraspec=1./((1-0.70992*exp(-1i*omega)-0.15983*exp(-2*1i*omega)).*...
    ((1-0.70992*exp(1i*omega)-0.15983*exp(2*1i*omega))));

figure
hold on
title('Parametric Periodogram')
plot(omega,paraspec*0.8535/2/pi)
hold off;


%% PROBLEM 4

%%%%%%%%%%%%%% - PART A - %%%%%%%%%%%%%%%%%%%%

%Load Data
house_data = xlsread('HOUSTNSA_2.xls');

y=house_data((2:end),2);
% Lets see the data 
plot(y);

% It is clear from the graph that the data displays a seasonal pattern

%ACF
f3 = figure(3);
set(f3,'Name', 'ACF');
autocorr(y)

%PACF
f4 = figure(4);
set(f4,'Name', 'PACF');
parcorr(y)

% Both functions show a oscillating pattern due to the seasonality

%%%%%%%%%%%%%% - PART B - %%%%%%%%%%%%%%%%%%%%

T=length(y)

for t=13:T
    x(t,1)= y(t)-y(t-12)
end

x=x(13:end)

plot(x)

%ACF
f5 = figure(5);
set(f5,'Name', 'ACF');  
autocorr(x,100)

%PACF
f6 = figure(6);
set(f6,'Name', 'PACF');
parcorr(x,100)

mult_arima = arima('Constant',0,'D',1,'Seasonality',12,...
    'MALags',[1],'SMALags',1, 'ARLags',[1 2])
EstModel1 = estimate(mult_arima,x);

%%%%%%%%%%%%%% - PART C - %%%%%%%%%%%%%%%%%%%%

%We will estimate the non-parametric periodogram
autocov_x=xcov(x,'biased');
start=(length(autocov_x)+1)/2;
lengthspan=length(autocov_x)-start; %Start at the median index
SPEC=[];

for omega=0:0.01:pi
temp1=cos(omega*[1:lengthspan]);
temp2=autocov_x(start+1:end);
SPEC=[SPEC;(sum(temp1'.*temp2)*2+autocov_x(start))];
end

figure
hold on
title('Sample Periodogram of X')
plot(0:0.01:pi,SPEC/2/pi)
hold off;


%%%%%%%%%%%%%% - PART D - %%%%%%%%%%%%%%%%%%%%

% Kernel smoothing
k= [1/16, 1/8, 3/16, 1/4, 3/16, 1/8, 1/16];
SPEC_W = zeros(size(SPEC,1),1);
SPEC_W(1:3)=SPEC(1:3);
SPEC_W(end-2:end)=SPEC(end-2:end);
for t=4:length(SPEC_W)-3
SPEC_W(t) = k*SPEC(t-3:t+3);
end

figure
hold on
title('Smoothed sample Periodogram')
plot(0:0.01:pi,SPEC_W/2/pi)

%%%%%%%%%%%%%% - PART E - %%%%%%%%%%%%%%%%%%%%

omega=0:0.01:pi;

num = (1+(-0.56611)*exp(-1i*omega)).*(1+(-0.56611)*exp(1i*omega))
denom = (1-(-0.27762)*exp(-1i*omega)-...
    (-0.075314)*exp(-2*1i*omega)).*(1-(-0.27762)*exp(1i*omega)-...
    (-0.075314)*exp(2*1i*omega))

paraspec_2 = (0.030961/(2*pi)).*(num./denom)

figure
hold on
title('Parametric Spectrum')
plot(omega,paraspec_2)


