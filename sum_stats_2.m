
function [mean, median, max, min, sd, sk, ku, JBt, JBp] = sum_stats_2(x)
    mean= nanmean(x)
    median= nanmedian(x)
    max= nanmax(x)
    min= nanmin(x)
    sd= nanstd(x)
    sk= skewness(x)
    ku= kurtosis(x)
[p,jbstat] = jbtest(x,0.01)
end