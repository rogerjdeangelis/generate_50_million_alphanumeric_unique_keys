# generate_50_million_alphanumeric_unique_keys
Generate 50 million alphanumeric unique keys. Keywords: sas sql join merge big data analytics macros oracle teradata mysql sas communities stackoverflow statistics artificial inteligence AI Python R Java Javascript WPS Matlab SPSS Scala Perl C C# Excel MS Access JSON graphics maps NLP natural language processing machine learning igraph DOSUBL DOW loop stackoverflow SAS community.


    see excelent solution on the end by which overcomes original issues

    Paul Dorfman
    Ian ian.wakeling@hanani.qistats.co.uk
    via listserv.uga.edu

    There is a bug in this code, stri_rand_strings uses replacement. I have added R code on the end to take care of duplicates.
    However it may make this technique too slow for 50 million uniques?

    Generate 50 million alphanumeric unique keys

      WPS Proc R

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

    My mistake. stri_rand_strings uses replacement.
    You need to dedup.

    see Julien Navarre dedup routine
    https://stackoverflow.com/users/2667955/julien-navarre

    Unfortunately this seems to really slow down processing?

    %utl_submit_wps64('
    libname sd1 "d:/sd1";
    options set=R_HOME "C:/Program Files/R/R-3.3.1";
    proc r;
    submit;
    source("C:/Program Files/R/R-3.3.1/etc/Rprofile.site", echo=T);
    set.seed(12345)
    library(stringi);
    idGenerator <- function(n, lengthId) {
    idList <- stringi::stri_rand_strings(n, lengthId, pattern = "[A-Za-z0-9]");
      while(any(duplicated(idList))) {;
        idList[which(duplicated(idList))]
        <- stringi::stri_rand_strings(sum(duplicated(idList), na.rm = TRUE), lengthId, pattern = "[A-Za-z0-9]");
    }; return(idList) };
    idGenerator(16,1);
    endsubmit;
    run;quit;
    ');


    The WPS System

     [1] "W" "h" "Z" "X" "K" "c" "U" "C" "8" "4" "e" "z" "0" "E" "q" "5"
     
     
     
         
         
    see excelent solution on the end by

    Paul Dorfman
    Ian ian.wakeling@hanani.qistats.co.uk
    via listserv.uga.edu


    %let K =    5 ;
    %let N =   52 ;
    %let KK = 500 ;

    data ID (keep = ID) ;
       retain K &K N &N KK &KK ;
       S  = put (compress (compress (collate(0, 255), "_"), , "kN"), $&N..) ;
       C  = comb (N, K) ;
       F  = fact (K) ;
       NN = C * F ;
       put (S K N KK C F NN) (=) ;
       array aid [&K] $1 _temporary_ ;
       do i1 =  1     to n - k + 1 ;
       aid[1] = char (s, i1) ;
       do i2 = i1 + 1 to n - k + 2 ;
       aid[2] = char (s, i2) ;
       do i3 = i2 + 1 to n - k + 3 ;
       aid[3] = char (s, i3) ;
       do i4 = i3 + 1 to n - k + 4 ;
       aid[4] = char (s, i4) ;
       do i5 = i4 + 1 to n - k + 5 ;
       aid[5] = char (s, i5) ;
       do i = 1 to F ;
         rc = allperm (i, of aid[*]) ;
         if rand("uniform") < divide (KK, NN) then do ;
           ID = put (cat (of aid[*]), $5.) ;
           output ;
           KK +- 1 ;
         end ;
         NN +- 1 ;
       end ;
       temp   = aid[1] ;
       aid[1] = aid[2] ;
       aid[2] = temp ;
       end ;
       end ;
       end ;
       end ;
       end ;
    run ;

    Kudos and best,
    Paul Dorfman



