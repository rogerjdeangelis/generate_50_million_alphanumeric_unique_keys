Generate 50 million alphanumeric unique keys

github
https://github.com/rogerjdeangelis/generate_50_million_alphanumeric_unique_keys

see
https://listserv.uga.edu/cgi-bin/wa?A2=SAS-L;20911c96.1805b

see
https://stackoverflow.com/questions/29652356/generate-unique-alphanumeric-ids

see A5C1D2H2I1M1N2O1R2T1 profile
https://stackoverflow.com/users/1270695/a5c1d2h2i1m1n2o1r2t1

It took 107 seconds to create 5 million. did not want to wait fro 50,000,000 but should work.
May not be o(1).

INPUT
=====

   There is no input, just the requirement that we create 50 million
   unique alphanumeric ids

   Note we only need strings of lenght 5 because

   upper(26)*lower(26)*digits=62**5 = 916,132,832


PROCESS ( working code)
=======================

   stri_rand_strings(5000000, 5)


OUTPUT
======

WORK.WANT total obs=5,000,000

 Obs    RANFIV

   1    Yftoa
   2    BZGch
   3    O5k9C
   4    3P74M
   5    ylQIq
  ...

*          _       _   _
 ___  ___ | |_   _| |_(_) ___  _ __
/ __|/ _ \| | | | | __| |/ _ \| '_ \
\__ \ (_) | | |_| | |_| | (_) | | | |
|___/\___/|_|\__,_|\__|_|\___/|_| |_|

;

%utl_submit_wps64('
libname sd1 "d:/sd1";
options set=R_HOME "C:/Program Files/R/R-3.3.1";
libname wrk sas7bdat "%sysfunc(pathname(work))";
libname hlp sas7bdat "C:\progra~1\SASHome\SASFoundation\9.4\core\sashelp";
proc r;
submit;
source("C:/Program Files/R/R-3.3.1/etc/Rprofile.site", echo=T);
library(haven);
have<-read_sas("d:/sd1/have.sas7bdat");
head(have);
library(stringi);
x<-as.data.frame(stri_rand_strings(50000000, 5));
colnames(x)<-c("ranfiv");
head(x);
endsubmit;
import r=x data=wrk.want;
run;quit;
');

15        import r=have data=wrk.want;
NOTE: Creating data set 'WRK.want' from R data frame 'have'
NOTE: Data set "WRK.want" has 5000000 observation(s) and 1 variable(s)

16        run;
NOTE: Procedure r step took :
      real time : 1:47.608

