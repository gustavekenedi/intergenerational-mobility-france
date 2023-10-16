libname dd "\\casd.fr\casdfs\Projets\DISPSAL\Data\EDP_EDP_2017";
PROC EXPORT DATA= dd.edp_be2017_individu
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_individu.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_naissance
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_naissance.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_descendance
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_descendance.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_panact
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_panact.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revenu_2011
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revenu_2011.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revdet_2011
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revdet_2011.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2011
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2011.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revenu_2012
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revenu_2012.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revdet_2012
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revdet_2012.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2012
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2012.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revenu_2013
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revenu_2013.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revdet_2013
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revdet_2013.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2013
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2013.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revenu_2014
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revenu_2014.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revdet_2014
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revdet_2014.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2014
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2014.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revenu_2015
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revenu_2015.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revdet_2015
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revdet_2015.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2015
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2015.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revenu_2016
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revenu_2016.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revdet_2016
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revdet_2016.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2016
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2016.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revenu_2017
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revenu_2017.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_revdet_2017
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_revdet_2017.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2017
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2017.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_fisc_individu_2017
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_fisc_individu_2017.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_rp1968
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_rp1968.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_rp1975
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_rp1975.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_rp1982
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_rp1982.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_rp1990
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_rp1990.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= dd.edp_be2017_rp1999
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2017\edp_be2017_rp1999.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
libname dd "\\casd.fr\casdfs\Projets\DISPSAL\Data\EDP_EDP_2019";
PROC EXPORT DATA= dd.edp_be2019_panact
            OUTFILE= "C:\Users\Public\Documents\gustave_k\original_data\EDP_2019\edp_be2019_panact.csv" 
            DBMS=CSV LABEL REPLACE;
     PUTNAMES=YES;
RUN;
/* DADS 96 */
proc export data = dads.panel2010(where = (AN = 1996))
			outfile = "C:\Users\Public\Documents\gustave_k\original_data\DADS_90\dads96.dta" 
			dbms = stata label replace;
run;
