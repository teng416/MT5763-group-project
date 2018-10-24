/*import data sets*/
PROC IMPORT OUT= Work.PotPlant_18_Set1 
            DATAFILE= "C:\Users\Administrator\Desktop\MT5763\PotPlants_18_Set1.csv"
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Work.PotPlant_18_Set2 
            DATAFILE="C:\Users\Administrator\Desktop\MT5763\PotPlants_18_Set2.csv"
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Work.PotPlant_18_Set3
            DATAFILE= "C:\Users\Administrator\Desktop\MT5763\PotPlants_18_Set3.csv"
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Work.PotPlant_18_Set_Summary 
            DATAFILE= "C:\Users\Administrator\Desktop\MT5763\PotPlants_18_Summary.csv"
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

Data Potplant_18_set2_v2;
set Potplant_18_set2;
if SampleName = ""
then delete;  *delete the last 3 rows for stacking datasets;
rename SampleName = Sample_Name; *rename the variable for stacking datasets;
run;

Data plant_data_combined;
set Potplant_18_Set1 Potplant_18_set2_v2 Potplant_18_set3;
run;

data plant_data;
set plant_data_combined;
if Th="." then delete;  *delete rows with NA and missing data;
if Group = "pot" then Group = "pm"; *change the pot to pm for analysis;
rename Group = Soil;
run;

proc sort data = plant_data out = plant_data_sort; * sort the data by pots typles;
by Soil;
run;

data plant_data_sort1; *add index column to facilitate plot;
set plant_data_sort;
Sample = _n_;
run;

/* scatter plot for Ca*/
proc template;
define statgraph sgdesign;
dynamic _Sample _Ca _Soil;
begingraph;
   entrytitle halign=center 'Ca levels in different soil types';
   entryfootnote halign=left '';
   layout lattice / rowdatarange=data columndatarange=data rowgutter=10 columngutter=10;
      layout overlay;
         scatterplot x=_Sample y=_Ca / group=_Soil name='scatter';
		 discretelegend "scatter";
      endlayout;
   endlayout;
endgraph;
end;
run;

proc sgrender data=WORK.PLANT_DATA_SORT1 template=sgdesign;
dynamic _SAMPLE="SAMPLE" _Ca="Ca" _SOIL="SOIL";
run;

/*Scatter plot for Sr*/
proc template;
define statgraph sgdesign;
dynamic _Sample _Sr _Soil;
begingraph;
   entrytitle halign=center 'Sr levels in different soil types';
   entryfootnote halign=left '';
   layout lattice / rowdatarange=data columndatarange=data rowgutter=10 columngutter=10;
      layout overlay;
         scatterplot x=_Sample y=_Sr / group=_SOIL name='scatter';
		 discretelegend "scatter";
      endlayout;
   endlayout;
endgraph;
end;
run;

proc sgrender data=WORK.PLANT_DATA_SORT1 template=sgdesign;
dynamic _SAMPLE="SAMPLE" _Sr="Sr" _SOIL="SOIL";
run;

/*calculate mean and sd*/
proc summary data = plant_data;
class Soil;
Var Ca;
Output mean = Ca_mean std = Ca_sd
run;
proc print; run;

proc summary data = plant_data;
class Soil;
Var Sr;
Output mean = Sr_mean std = Sr_sd
run;
proc print; run;

/*ANOVA and post hoc for Ca*/
proc glm data = plant_data;
class Soil;
model Ca = Soil;
means Soil / Tukey;
output out = Ca_result r = Ca_resid;
run;

symbol v = spot;
/*qq plot*/
proc univariate data = Ca_result noprint;
qqplot Ca_resid / normal (mu=est sigma=est color=blue l=1)
square;
run;

/*ANOVA and post hoc for Ca*/
proc glm data = plant_data;
class Soil;
model Sr = Soil;
means Soil / Tukey;
output out = Sr_result r = Sr_resid;
run;

symbol v = spot;
/*qq plot*/
proc univariate data = Sr_result noprint;
qqplot Sr_resid / normal (mu=est sigma=est color=blue l=1)
square;
run;

/*calculate correlation*/
data corelation_data;
set plant_data;
keep Ba Ga Ca Sr Zn;
run;
proc corr data = corelation_data;
run;

/*scattering polot for 3 pairs of elements*/
proc corr data= plant_data plots = scatter;
var Ba Ga;
run;

proc corr data= plant_data plots = scatter;
var Ca Sr;
run;

proc corr data= plant_data plots = scatter;
var Zn Ba;
run;

/*linear regression and diagnose*/
proc reg data = plant_data;
model Ba = Ga;
run;quit;

proc reg data = plant_data;
model Ca = Sr;
run;quit;

proc reg data = plant_data;
model Zn = Ba;
run;quit;



