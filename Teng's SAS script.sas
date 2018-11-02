/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*This is a small SAS program to perform nonparametric bootstraps for a regression
/*It is not efficient nor general*/
/*Inputs: 																								*/
/*	- NumberOfLoops: the number of bootstrap iterations
/*	- Dataset: A SAS dataset containing the response and covariate										*/
/*	- XVariable: The covariate for our regression model (gen. continuous numeric)						*/
/*	- YVariable: The response variable for our regression model (gen. continuous numeric)				*/
/*Outputs:																								*/
/*	- ResultHolder: A SAS dataset with NumberOfLoops rows and two columns, RandomIntercept & RandomSlope*/
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

%macro regBoot(NumberOfLoops, DataSet, XVariable, YVariable);


/*Number of rows in my dataset*/
 	data _null_;
  	set &DataSet NOBS=size;
  	call symput("NROW",size);
 	stop;
 	run;

/*loop over the number of randomisations required*/
%do i=1 %to &NumberOfLoops;


/*Sample my data with replacement*/
	proc surveyselect data=&DataSet out=bootData seed=-23434 method=urs noprint sampsize=&NROW;
	run;

/*Conduct a regression on this randomised dataset and get parameter estimates*/
	proc reg data=bootData outest=ParameterEstimates  noprint;
	Model &YVariable=&XVariable;
	run;
	quit;

/*Extract just the columns for slope and intercept for storage*/
	data Temp;
	set ParameterEstimates;
	keep Intercept &XVariable;
	run;

/*Create a new results dataset if the first iteration, append for following iterations*/
	data ResultHolder;
		%if &i=1 %then %do;
			set Temp;
		%end;
		%else %do;
			set ResultHolder Temp;
		%end;
	run;
	%end;
/*Rename the results something nice*/
data ResultHolder;
set ResultHolder;
rename Intercept=RandomIntercept &XVariable=RandomSlope;
run;
%mend;

options nonotes;
/*Run the macro*/
/* Start timer */
%let _timer_start = %sysfunc(datetime());
%regBoot(NumberOfLoops=100, DataSet=Randomset, XVariable=x, YVariable=y);
/* Stop timer */
data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;



/*generate random dataset to work with*/
data Randomset;
do i= 1 to 100;
x = 100*rand('uniform');
y = 2*x+4+5*rand('normal');
output;
end;
proc sort data=Randomset;by x; run;
proc print;
run;

/*improved macro*/
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*This is a small SAS program to perform nonparametric bootstraps for a regression
/*Inputs: 																								*/
/*	- NumberOfRep: the number of bootstrap iterations
/*	- Dataset: A SAS dataset containing the response and covariate										*/
/*	- XVariable: The covariate for our regression model (gen. continuous numeric)						*/
/*	- YVariable: The response variable for our regression model (gen. continuous numeric)				*/
/*Outputs:																								*/
/*	- ResultHolder_new: A SAS dataset with NumberOfRep rows and two columns, RandomIntercept & RandomSlope*/

/*Improve Logic: the previous macro uses loop. For each loop, it will generate a newly sampled dataset*/
/*and calculate the coefficients. The updated macro will create resample all the new dataset first into one dataset */
/*and do regression in one dataset according to index of iterations.*/
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

%macro regBoot_new(NumberOfRep, DataSet, XVariable, YVariable);

/*Number of rows in my dataset*/
 	data _null_;
  	set &DataSet NOBS=size;
  	call symput("NROW",size);
 	stop;
 	run;

/*Sample my data with replacement and repeat NumOfRep times and form a single dataset*/
	proc surveyselect data=&Dataset out=bootData_new seed = -23434 method = urs sampsize=&NROW rep = &NumberOfRep outhits noprint;
    run;

/*Conduct a regression on this single dataset according to repeats*/
	proc reg data=bootData_new outest=ParameterEstimates_new noprint;
	Model &YVariable=&XVariable;
	by Replicate;
	run;
	quit;

/*Extract just the columns for slope and intercept for storage*/
	data ResultHolder_new;
	set ParameterEstimates_new;
	keep Intercept &XVariable;
	run;

%mend;

/*run new macro*/
/* Start timer */
%let _timer_start = %sysfunc(datetime());
%regBoot_new(NumberOfRep=100, DataSet=Randomset, XVariable=x, YVariable=y);
/* Stop timer */
data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;





/*add RTF and plot to the macro*/
%macro regBoot_new(NumberOfRep, DataSet, XVariable, YVariable);

/*Number of rows in my dataset*/
 	data _null_;
  	set &DataSet NOBS=size;
  	call symput("NROW",size);
 	stop;
 	run;

/*Sample my data with replacement and repeat NumOfRep times and form a single dataset*/
	proc surveyselect data=&Dataset out=bootData_new seed = -23434 method = urs sampsize=&NROW rep = &NumberOfRep outhits noprint;
    run;

/*Conduct a regression on this single dataset according to repeats*/
	proc reg data=bootData_new outest=ParameterEstimates_new noprint;
	Model &YVariable=&XVariable;
	by Replicate;
	run;
	quit;

/*Extract just the columns for slope and intercept for storage*/
	data ResultHolder_new;
	set ParameterEstimates_new;
	keep Intercept &XVariable;
	run;
/*rename col */
	data ResultHolder_new;
    set ResultHolder_new;
    rename Intercept=RandomIntercept &XVariable=RandomSlope;
    run;

/*calculate means for each paramters*/
	proc means data=ResultHolder_new;
	var RandomIntercept RandomSlope;
	output out=means mean=;
	keep Intercept RandomSlope;
	run; 

/*calculate confidence interval*/
	proc univariate data=ResultHolder_new noprint;
	var RandomIntercept RandomSlope;
	output out=regBootCI pctlpts=2.5 97.5 pctlpre=RandomIntercept_ RandomSlope_; 
	run;

/*plot distribution and output with RTF*/
	ods escapechar = '^';
    ods rtf body = 'Bootstrapping Analysis' style = styles.rtf;

    proc print data=means;
	title '^S={protectspecialchars = on} Mean of coefficients';
    run;

	proc print data=regBootCI;
	title '^S={protectspecialchars = on} 95% Confidence Intervals';
    run;

	proc gchart data=ResultHolder_new;
	title 'Coefficients Distribution';
	vbar RandomIntercept RandomSlope;
	run;
	ods rtf close;
%mend;

/*run macro*/
%regBoot_new(NumberOfRep=100, DataSet=Randomset, XVariable=x, YVariable=y);






