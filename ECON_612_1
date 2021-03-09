
%PROBLEM 1


%A
    %We will use the S&P 500 data from 2009-09-21 until 2019-09-20. 
    %We import the data manually.
    sp_500=SP500;

    % Lets check for missing values
    sum(ismissing(sp_500));
    is_m = ismissing(sp_500.SP500);
    
    % Delete Missing
    sp_500_clean=sp_500(~is_m,:);
    sp_500_clean;

    display(sum(ismissing(sp_500_clean)));

    % Log Difference
    n=length(sp_500_clean.SP500);
    y=sp_500_clean.SP500;
    l_y=log(y(2:end,:));
    
    d_l_y=zeros(n-1,1);
    for t=2:n
        d_l_y(t)=(l_y(t)/l_y(t-1))-1
    end
    
%B    
    % plot
    time=sp_500_clean.DATE(2:end,:);
    plot(time,d_l_y);
    xlabel('Time');
    ylabel('var_SP500');
    title('First difference of log SP500');
    
%C
    %Summary statistics
    sum_stat(d_l_y)
    
%--------------------------------------------------------------------------
%PROBLEM 2

    N = 1000;    
    p = 2;       
    R = 5000;    

%A 
    beta = [-1 1]';

%Simulations
    for r = 1:R
        epsilon(:,r) = normrnd(0, 0.5,[N,1]); 
        X(:,:,r) = [ones(N,1), normrnd(0,1,[N,1])];
        Y(:,r) = X(:,:,r)* beta + epsilon(:,r);
    end 

% We calculate the required statistics
    for r = 1:R
        y_mean(:,r)  = mean(Y(:,r)); 
        y_var(:,r)   = var(Y(:,r));
        y_per_25(:,r)   = prctile(Y(:,r),25);
        y_per_75(:,r)   = prctile(Y(:,r),75); 
    end

%B

    %Average across simulations
    y_avg_mean  = mean(y_mean(1,:));
    y_avg_var   = mean(y_var(1,:));
    y_avg_p25   = mean(y_per_25(1,:));
    y_avg_p25   = mean(y_per_75(1,:));

%C - Separate file

%D 
    %With the function "ssr" we can estimate beta. We start at beta_0. Estimated values
    %for each iteration are stored in beta_hat.

    beta_0 = [-3 3]';

    for r= 1:R
        f = @(beta) ssr(X(:,:,r),Y(:,r),N,beta);
        beta_hat(:,r) = fminunc(f,beta_0) ;
    end

%E

    b_hat_mean = sum(beta_hat(1,:))/R

%--------------------------------------------------------------------------
%PROBLEM 3

%A - Function "simu" in separate file. The function simulates an AR(1)
%process

%B
    sim_y=simu(0,0.8,1,250)
    plot(sim_y)

%C
    phi1=0.8
    smpl_auto_corr = autocorr(sim_y,'NumLags',19,'NumSTD',2);

    %We calculate the theoretical and sample autocorrelations
    theo_auto_corr = zeros(20,1);
    theo_auto_corr(1) = phi1;
    for t=1:20
        theo_auto_corr(t) = phi1^t;
    end

   
    %We combine the graphs of both
    plot(theo_auto_corr)
    title('Theoretical vs Sample Autocorrelations')

    hold on

    plot(smpl_auto_corr)

    hold off





