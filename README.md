# Author: Andreas Hadjiprocopis
# project name: DUTC
# date: 18-02-2018
# version: 1.0

The final report as PDF is [here](../master/RMARKDOWN_OUT_REPORTS/7.final_report.pdf).

The final report as HTML [here](../master/RMARKDOWN_OUT_REPORTS/7.final_report.html)

If for some reason the PDF report is not showing right, then
you will have to use the HTML report.
UNFORTUNATELY: Github refuses to render big HTML files
(it is beyond me how this is so as rendering is the responsibility of the local browser)
so you will have to save them locally before you view them if you see a message that file is too big for Github to handle.
(i.e. right click on the 'raw file' link, save and then on command line open 7.final_report.html)

If you want to reproduce the report please do:  bin/make_reports.bash

There are three types of analyses:
1. Data analysis: normality test, correlations between columns, distribution estimation: [here](../master/RMARKDOWN-REPORTS/timeseries.html)
2. Timeseries analysis: stationarity test, auto-regressive test, trends [here](../master/RMARKDOWN-REPORTS/timeseries.html)
3. Principal Component Analysis: [here](../master/RMARKDOWN-REPORTS/pca.html)

And also there is a list of 3rd party library dependencies,
an explanation on how data was cleaned (from NAs) and some questions
for the data provider.

WARNING: PDF renderings of the Rmarkdown files situated [here](../master/RMARKDOWN_IN)
are not adequate. They are poor and sub-standard. It is best to
view the HTML documents.

All individual reports are here as HTML
1. [dependencies](../master/RMARKDOWN_OUT_REPORTS/1.dependencies.html)
2. [clean data/remove NAs](../master/RMARKDOWN_OUT_REPORTS/2.clean_dataset.html)
3. [data analysis](../master/RMARKDOWN_OUT_REPORTS/3.data.html)
4. [timeseries analysis](../master/RMARKDOWN_OUT_REPORTS/4.timeseries.html)
5. [PCA analysis](../master/RMARKDOWN_OUT_REPORTS/5.pca.html)
6. [Questions](../master/RMARKDOWN_OUT_REPORTS/6.questions.html)
7. [All the above](../master/RMARKDOWN_OUT_REPORTS/7.final_report.html)

The final report PDF is [here](../master/RMARKDOWN_OUT_REPORTS/7.final_report.pdf)

bw

Andreas
