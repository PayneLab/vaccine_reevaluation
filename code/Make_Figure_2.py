### IMPORTING LIBRARIES______________________________________________________
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import numpy as np
import csv

### CREATING FILE AND HEADERS FOR LINEAR REGRESSION RESULTS__________________
header = open('r2_all_50k.csv', 'a', newline='')
write_head = csv.writer(header)
write_head.writerow(['r2'])
header.close()

### LOOPING__________________________________________________________________
loop_count = 0
while loop_count < 50000:

    ### READING, SAMPLING, AND PREPARING THE DATA____________________________
    data = pd.read_csv('fig_1_data.csv')
    random_sample = data.sample(n=30)
    x = random_sample.iloc[:, 4].values
    x = np.reshape(x, (-1,1))
    y = random_sample.iloc[:, 1].values
    countries = random_sample.iloc[:, 0]

    ### RUNNING THE LINEAR REGRESSION MODEL AND GENERATING SUMMARY___________
    x = sm.add_constant(x)
    model = sm.OLS(y, x).fit()
    result = model.summary()
    
    ### EXPORTING RESULT_____________________________________________________
    ### SUMMARY DATA_________________________________________________________
    summary = model.summary().as_text()
    countries = str(countries)
    test_num = loop_count
    test_num +=1
    test_num = str(test_num)
    lr_result = open('summary_all_50k.txt', 'a')
    lr_result.writelines('***Test '
                        + test_num
                        + '***'  
                        + '\n\n'
                        + countries
                        + '\n\n'
                        + summary
                        + '\n\n\n'
                        )
    lr_result.close()

    ### R^2 DATA_____________________________________________________________
    r2 = str(model.rsquared)
    r2_result = open('r2_all_50k.csv', 'a', newline = '')
    write_r2 = csv.writer(r2_result)
    write_r2.writerow([r2]) 
    r2_result.close()
    
    ### ADDING TO LOOP COUNT_________________________________________________
    loop_count += 1

### GENERATING SUMMARY STATISTICS AND R^2 DISTRIBUTION_______________________
### READING IN GENERATED R^2 DATA____________________________________________
r2_data = pd.read_csv('vaccine/result_50k/r2_all_50k.csv', header  = [0])

### SUMMARY STATISTICS_______________________________________________________
r2_mean = r2_data.mean()
r2_mean = float(r2_mean)
r2_sd = r2_data.std()
r2_sd = float(r2_sd)
r2_median = r2_data.median()
r2_median = float(r2_median)
q3, q1 = np.percentile(r2_data, [75 ,25])
q3 = float(q3)
r2_iqr = q3 - q1
r2_iqr = float(r2_iqr)

### Z-SCORE AND OUTLIER THRESHOLD BASED ON IQR_______________________________
r2_orig_paper = 0.493
z_score = ((r2_orig_paper-r2_mean)/r2_sd)
outlier_thresh = (q3+(1.5*r2_iqr))

### WRITING RESULTS__________________________________________________________
r2_mean = str(r2_mean)
r2_sd = str(r2_sd)
z_score = str(z_score)
r2_median = str(r2_median)
r2_iqr = str(r2_iqr)
outlier_thresh = str(outlier_thresh)

final_result = open('final_result_50k.txt', 'a')
final_result.writelines('***Final Result***'
                        + '\n\n'
                        + 'mean:                  ' + r2_mean
                        + '\n'
                        + 'standard deviation:    ' + r2_sd
                        + '\n'
                        + 'z-score:               ' + z_score
                        + '\n\n'
                        + 'median:                ' + r2_median
                        + '\n'
                        + 'iqr:                   ' + r2_iqr
                        + '\n'
                        + 'q3 outlier threshold:  ' + outlier_thresh
                        )
final_result.close()

### GENERATING R^2 HISTOGRAM_________________________________________________
sup = str.maketrans('0123456789', '⁰¹²³⁴⁵⁶⁷⁸⁹')
r2_text = 'R2'.translate(sup)
r2_data = r2_data.values
n, bins, patches = plt.hist( x = r2_data, 
                                bins = (16),
                                color = 'grey',
                                edgecolor = '#000000',
                                alpha = 0.95,
                                rwidth = 0.7,
                                histtype = 'bar'
                                )
plt.xlabel(r2_text)
plt.ylabel('Count')
plt.title('Distribution of ' + r2_text + ' values')
plt.grid(axis='y',
        alpha=0.35
        )

bin_centers = np.diff(bins)*0.5 + bins[:-1]
for count, (freq, x, patch) in enumerate(zip(n, bin_centers, patches)):
        height = int(n[count])
        plt.annotate('{}'.format(height),
                        xy = (x, height),
                        xytext = (0,0.2),
                        textcoords = 'offset points',
                        ha = 'center', 
                        va = 'bottom'
                        )
plt.show()
