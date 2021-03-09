clear;  
close all; 
clc;

cd('C:\Users\eschi\OneDrive - Duke University\Spring 2020\ECON 623 - Forecasting FM\HWs\HW_1')

%% Q1 %%

    %% a)
    sp500 = readtable('SP500.csv')
    usd_eur = readtable('USD_EUR.csv')
    
    %Cleaning Missing Values
    size(sp500)
    toDelete_1 = strcmp(sp500.SP500,'.');
    sp500(toDelete_1,:) = [];
    size(sp500)
    
    size(usd_eur)
    toDelete_2 = strcmp(usd_eur.DEXUSEU,'.');
    usd_eur(toDelete_2,:) = [];
    size(usd_eur)    
    
    %We turn data into numerical
    class(sp500.DATE)
    class(sp500.SP500)
    
    class(usd_eur.DATE)
    class(usd_eur.DEXUSEU)
 
    sp500.SP500 = str2double(sp500.SP500)
    usd_eur.DEXUSEU = str2double(usd_eur.DEXUSEU)
    
    % Continuously compounded Log-returns
    sp = sp500.SP500
    sp_ret = log(sp(2:end)./sp(1:end-1));
    
    ex = usd_eur.DEXUSEU
    ex_ret = log(ex(2:end)./ex(1:end-1));
    
    %for t = 2:height(sp500)
        %sp_ret(t,1) = (sp500.SP500(t)/sp500.SP500(t-1))-1  
    %end
    
    % Dates
    dates_sp = x2mdate(sp500.DATE, 0, 'datetime')
    dates_ex = x2mdate(usd_eur.DATE, 0, 'datetime')
    
    tstart = dates_sp(2)
    tend = dates_sp(end)
    
    % Plots 
    figure(1)
    plot(dates_sp(2:end), sp_ret)
    title('Continuously compounded log returns, S&P500')
    xlabel('Date')
    ylabel('Return')
    xlim([tstart tend]) 
    
    figure(2)
    plot(dates_ex(2:end), ex_ret)
    title('Log difference, USD/EUR')
    xlabel('Date')
    ylabel('Return')
    xlim([tstart tend])  
    
    %% b)
    stats = sum_stats(sp_ret)
    
    (...)
        

 
 
    
