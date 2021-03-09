
function[Mean, Median, Max, Min, SD, Skw, Kurt, JB_t, JB_p] = sum_stats(x)
    sta_vec = table();
    sta_vec.Mean=mean(x);
    sta_vec.Median=median(x);
    sta_vec.Max=max(x);
    sta_vec.Min=min(x);
    sta_vec.SD=std(x);
    sta_vec.Skw=skewness(x);
    sta_vec.Kurt=kurtosis(x);
    [h,p,jbstat,critval] = jbtest(x,0.05);
    sta_vec.JB_t= critval;
    sta_vec.JB_p= p;
end
    
    
