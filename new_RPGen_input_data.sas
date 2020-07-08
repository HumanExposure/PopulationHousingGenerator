
libname rp "C:/main/HEM/RPGen";

data avars;
   set x;
   file "C:/main/HEM/RPGen/AHS2017vars.csv";
   if (_N_=1) then put "variable, description";
   put NAME ',' LABEL;
run;
data rp.AHS1;
   set rp.ahs2017n;
   if (numpeople<1 or numadults<1) then delete;
   keep acprimary acsecndry bathrooms bedrooms bld control cookfuel cooktype 
        dishwash dist division dryer elecamt fridge garage gasamt heatfuel heattype
        hhage hhgrad hincp kitchens laundy lotsize numadults numelders numoldkids 
        numpeople numyngkids oilamt omb13cbsa otheramt paintpeel sewtype totrooms
		unitfloors unitsize walk washer watsource weight yrbuilt;
run;


data rp.ahs2;
   set rp.ahs1;
   if (division in (1,2))   then region=1;
   if (division in (3,4))   then region=2;
   if (division in (5,6,7)) then region=3;
   if (division in (8,9))   then region=4;
   if (omb13cbsa>=99998)    then urban=0; else urban=1;
   location = 2*region -1 + urban;
   if (location=.) then delete;
   if (bld=2 or bld=3)       then housetyp=1;
   if (bld in (4,5,6,7,8,9)) then housetyp=2;
   if (bld in (1,10))        then housetyp=3;
   if (housetyp=.)  then delete;
   numchild = numoldkids+numyngkids;
   if (numadults=1 and numchild=0) then famcat=1;
   if (numadults=1 and numchild>0) then famcat=2;
   if (numadults>1 and numchild=0) then famcat=3;
   if (numadults>1 and numchild>0) then famcat=4;
   if (famcat=.) then delete;
   income = hincp;
   if (hincp<0) then income=0;
run;
proc sort data=rp.ahs2; by location income; run;
proc sql noprint;
   select count(*) into :ahs1 from rp.ahs2 where location=1;
   select count(*) into :ahs2 from rp.ahs2 where location=2;
   select count(*) into :ahs3 from rp.ahs2 where location=3;
   select count(*) into :ahs4 from rp.ahs2 where location=4;
   select count(*) into :ahs5 from rp.ahs2 where location=5;
   select count(*) into :ahs6 from rp.ahs2 where location=6;
   select count(*) into :ahs7 from rp.ahs2 where location=7;
   select count(*) into :ahs8 from rp.ahs2 where location=8;
quit;
data rp.ahs3;
   set rp.ahs2;
   by location;
   retain cut 0 num 0 inccat 1;
   if (first.location) then do;
      num    = 0;
	  inccat = 1;
	  if (location=1) then cut=&ahs1/3;
      if (location=2) then cut=&ahs2/3;  
	  if (location=3) then cut=&ahs3/3;
	  if (location=4) then cut=&ahs4/3;
      if (location=5) then cut=&ahs5/3;
      if (location=6) then cut=&ahs6/3;  
	  if (location=7) then cut=&ahs7/3;
	  if (location=8) then cut=&ahs8/3;
   end;
   num = num+1;
   if(num>cut) then do; inccat = inccat+1; num=0; end;
   pool = 36*(location-1) + 12*(housetyp-1) + 3*(famcat-1)+inccat;
   drop cut num hincp
run;
proc sort data=rp.ahs3; by pool; run;

data _null_;
   set rp.ahs3;
   if (unitsize="1") then unitsf = 400;
   if (unitsize="2") then unitsf = 600;
   if (unitsize="3") then unitsf = 900;
   if (unitsize="4") then unitsf = 1250;
   if (unitsize="5") then unitsf = 1750;
   if (unitsize="6") then unitsf = 2250;
   if (unitsize="7") then unitsf = 2750;
   if (unitsize="8") then unitsf = 3500;
   if (unitsize="9") then unitsf = 5000;
   if (unitsize="M") then unitsf = 1500;
   if (lotsize="1")  then lot_sf = 3000;
   if (lotsize="2")  then lot_sf = 7000;
   if (lotsize="3")  then lot_sf = 15000;
   if (lotsize="4")  then lot_sf = 30000;
   if (lotsize="5")  then lot_sf = 100000;
   if (lotsize="6")  then lot_sf = 300000;
   if (lotsize="7")  then lot_sf = 600000;
   if (lotsize="N")  then lot_sf = -2;
   if (sewtype="M")  then sewtype = "0";
   if (hincp<0)      then hincp = 0;
   file "C:/main/HEM/RPGen/input/AHS2019.csv";
   if (_N_=1) then put "pool, acprim, acsec, baths, bedrms, built, hequip, heatfuel, lot, pwt, rooms, sewdis, unitsf, water, control, income";
   put pool "," acprimary "," acsecndry "," bathrooms "," bedrooms "," yrbuilt "," heattype "," heatfuel "," lot_sf ","
       weight "," totrooms "," sewtype "," unitsf "," watsource "," control "," hincp;
run;

proc contents data=rp.recs2015_public_v4 out=y; run; quit;
data rvars;
   set y;
   file "C:/main/HEM/RPGen/RECS2015vars.csv";
   if (_N_=1) then put "variable, description";
   put NAME ';' LABEL;
run;
data rp.recs1;
   set rp.recs2015_public_v4;
   keep adqinsul aircond atticfin basefin bedrooms cdd30yr cellar cooltype cwasher dishwash division dntheat 
        doeid drafty dryer dryruse elcool elperiph elwarm equipm fowarm fuelheat hdd30yr highceil hhage 
        kownrent lpwarm moneypy moisture ncombath nhafbath notmoist numadult numchild numberac numcfan 
        numlaptop numtablet nweight othrooms oven ovenfuel ovenuse outgrill pool prkgplc1 recbath regionc stove 
        stovefuel stoven stovenfuel stories swimpool temphomeac tempniteac totrooms tvcolor typehuq uatyp10 
        ugwarm usefo uselp useng usewood washload wdwarm windows yearmaderange;
run;

data rp.recs2;
   set rp.recs1;
   if (uatyp10="R") then urban=0; else urban=1;
   location = 2*regionc -1 + urban;
   if (location=.) then delete;
   if (typehuq in (2,3)) then housetyp=1;
   if (typehuq in (4,5)) then housetyp=2;
   if (typehuq=1)        then housetyp=3;
   if (housetyp=.)  then delete;
   if (numadult=1 and numchild=0) then famcat=1;
   if (numadult=1 and numchild>0) then famcat=2;
   if (numadult>1 and numchild=0) then famcat=3;
   if (numadult>1 and numchild>0) then famcat=4;
   if (famcat=.) then delete;
   income = moneypy;
   rename pool=poolh;
run;
proc sort data=rp.recs2; by location income; run;
proc sql noprint;
   select count(*) into :recs1 from rp.recs2 where location=1;
   select count(*) into :recs2 from rp.recs2 where location=2;
   select count(*) into :recs3 from rp.recs2 where location=3;
   select count(*) into :recs4 from rp.recs2 where location=4;
   select count(*) into :recs5 from rp.recs2 where location=5;
   select count(*) into :recs6 from rp.recs2 where location=6;
   select count(*) into :recs7 from rp.recs2 where location=7;
   select count(*) into :recs8 from rp.recs2 where location=8;
quit;
data rp.recs3;
   set rp.recs2;
   by location;
   retain cut 0 num 0 inccat 1;
   if (first.location) then do;
      num    = 0;
	  inccat = 1;
	  if (location=1) then cut=&recs1/3;
      if (location=2) then cut=&recs2/3;  
	  if (location=3) then cut=&recs3/3;
	  if (location=4) then cut=&recs4/3;
      if (location=5) then cut=&recs5/3;
      if (location=6) then cut=&recs6/3;  
	  if (location=7) then cut=&recs7/3;
	  if (location=8) then cut=&recs8/3;
   end;
   num = num+1;
   if(num>cut) then do; inccat = inccat+1; num=0; end;
   pool = 36*(location-1) + 12*(housetyp-1) + 3*(famcat-1)+inccat;
run;

data _null_;
   set rp.recs3;
   length header $500;
   header = "pool, doeid, nweight, hdd30yr, cdd30yr, kownrent, stories, stove, stovefuel,"||
      "oven, ovenfuel, ovenuse, outgrill, dishwash, cwasher, washload, dryer, dryruse, tvcolor," ||
      "numlaptop, numtablet, elperiph, moisture, prkgplc1, cooltype, tempniteac, numberac, numcfan," ||
      "notmoist, highceil, windows, adqinsul, drafty, swim, cellar, region, urban, housetyp, " ||
      "famcat, inccat";
   if (stoven>0) then do;
      stove = stove + stoven;
	  oven  = oven  + stoven;
	  stovefuel = stovenfuel;
	  ovenfuel  = stovenfuel;
   end;
   if (poolh=1 or swimpool=1) then swim=2; else swim=0;
   swim = swim + recbath; 
   file "C:/main/HEM/RPGen/input/RECS2019.csv";
   if (_N_=1) then put header;
   put pool "," doeid  "," nweight "," hdd30yr "," cdd30yr "," kownrent "," stories "," stove "," stovefuel ","
       oven "," ovenfuel "," ovenuse "," outgrill "," dishwash "," cwasher "," washload "," dryer ","
       dryruse "," tvcolor "," numlaptop "," numtablet "," elperiph "," moisture "," prkgplc1 "," cooltype ","
       tempniteac "," numberac "," numcfan "," notmoist "," highceil "," windows "," adqinsul "," drafty ","
       swim "," cellar "," regionc "," urban "," housetyp "," famcat "," inccat;
run;





libname pums1 "C:/main/HEM/RPGen/unix_pus";
libname pums2 "C:/main/HEM/RPGen/unix_hus";


data density;
   infile "C:/main/HEM/RPGen/puma_density.csv" dsd dlm=',' firstobs=2;
   length n 8 compid $7 name $60  area 8 pop 8 dens 8 ur $1;
   input n compid name area pop dens ur;
run;
proc sort data=density; by compid; run;


data pums2.h1a;
   set pums2.psam_husa;
   if (type=1);    * drop group quarters;
   if (np>0);      * drop empty houses;
   if (region<=4); * drop Puerto Rico;
   length compid $7;
   keep adjinc hht hincp np region serialno st puma bld nr npf r18 wgtp veh compid;
   compid=st||puma;
run;
proc sort data=pums2.h1a; by compid; run;

  
data pums2.h2a bad;
   merge pums2.h1a (in=in1) density (in=in2);
   by compid;
   drop name dens;
   if (in1);
   if (in2=0) then output bad;
   if (in2=1) then output pums2.h2a;
run;
proc sort data=pums2.h2a; by serialno; run;


data pums1.p1a;
   set pums1.psam_pusa;
   keep agep sex serialno pwgtp rac1p hisp jwmnp;
run;
proc sort data=pums1.p1a; by serialno; run;

data pums1.both1a reject bad;
   merge pums2.h2a(in=in1) pums1.p1a(in=in2);
   by serialno;
   if (in1=1 and in2=0) then output bad;
   if (in1=0 and in2=1) then output reject;
   if (in1 and in2) then output pums1.both1a;
run;


data pums2.h1b;
   set pums2.psam_husb;
   if (type=1);    * drop group quarters;
   if (np>0);      * drop empty houses;
   if (region<=4); * drop Puerto Rico;
   length compid $7;
   keep adjinc hht hincp np region serialno st puma bld nr npf r18 wgtp veh compid;
   compid=st||puma;
run;
proc sort data=pums2.h1b; by compid; run;
   
data pums2.h2b bad;
   merge pums2.h1b (in=in1) density (in=in2);
   by compid;
   drop name dens;
   if (in1);
   if (in2=0) then output bad;
   if (in2=1) then output pums2.h2b;
run;
proc sort data=pums2.h2b; by serialno; run;


data pums1.p1b;
   set pums1.psam_pusb;
   keep agep sex serialno pwgtp rac1p hisp jwmnp;
run;
proc sort data=pums1.p1b; by serialno; run;

data pums1.both1b reject bad;
   merge pums2.h2b(in=in1) pums1.p1b(in=in2);
   by serialno;
   if (in1=1 and in2=0) then output bad;
   if (in1=0 and in2=1) then output reject;
   if (in1 and in2) then output pums1.both1b;
run;


data pums2.h1c;
   set pums2.psam_husc;
   if (type=1);    * drop group quarters;
   if (np>0);      * drop empty houses;
   if (region<=4); * drop Puerto Rico;
   length compid $7;
   keep adjinc hht hincp np region serialno st puma bld nr npf r18 wgtp veh compid;
   compid=st||puma;
run;
proc sort data=pums2.h1c; by compid; run;
   
data pums2.h2c bad;
   merge pums2.h1c (in=in1) density (in=in2);
   by compid;
   drop name dens;
   if (in1);
   if (in2=0) then output bad;
   if (in2=1) then output pums2.h2c;
run;
proc sort data=pums2.h2c; by serialno; run;

data pums1.p1c;
   set pums1.psam_pusc;
   keep agep sex serialno pwgtp rac1p hisp jwmnp;
run;
proc sort data=pums1.p1c; by serialno; run;

data pums1.both1c reject bad;
   merge pums2.h2c(in=in1) pums1.p1c(in=in2);
   by serialno;
   if (in1=1 and in2=0) then output bad;
   if (in1=0 and in2=1) then output reject;
   if (in1 and in2) then output pums1.both1c;
run;



data pums2.h1d;
   set pums2.psam_husd;
   if (type=1);    * drop group quarters;
   if (np>0);      * drop empty houses;
   if (region<=4); * drop Puerto Rico;
   length compid $7;
   keep adjinc hht hincp np region serialno st puma bld nr npf r18 wgtp veh compid;
   compid=st||puma;
run;
proc sort data=pums2.h1d; by compid; run;
   
data pums2.h2d bad;
   merge pums2.h1d (in=in1) density (in=in2);
   by compid;
   drop name dens;
   if (in1);
   if (in2=0) then output bad;
   if (in2=1) then output pums2.h2d;
run;
proc sort data=pums2.h2d; by serialno; run;


data pums1.p1d;
   set pums1.psam_pusd;
   keep agep sex serialno pwgtp rac1p hisp jwmnp;
run;
proc sort data=pums1.p1d; by serialno; run;

data pums1.both1d reject bad;
   merge pums2.h2d(in=in1) pums1.p1d(in=in2);
   by serialno;
   if (in1=1 and in2=0) then output bad;
   if (in1=0 and in2=1) then output reject;
   if (in1 and in2) then output pums1.both1d;
run;

data pums1.pums_1 pums1.pums_2 pums1.pums_3 pums1.pums_4
     pums1.pums_5 pums1.pums_6 pums1.pums_7 pums1.pums_8;
   set pums1.both1a pums1.both1b pums1.both1c pums1.both1d;
   length ethnic $1 race $1 gender $1;
   if (hisp="01") then ethnic="N";
   if (hisp="02") then ethnic="M";
   if (hisp>"02") then ethnic="O";
   if (rac1p="1")  then race="W";
   if (rac1p="2")  then race="B";
   if (rac1p="3")  then race="N";
   if (rac1p="4")  then race="N";
   if (rac1p="5")  then race="N";
   if (rac1p="6")  then race="A";
   if (rac1p="7")  then race="P";
   if (rac1p="8")  then race="O";
   if (rac1p="9")  then race="O";
   if (sex=1)  then gender="M";
   if (sex=2)  then gender="F";
   if (UR="U") then urban=1;
   if (UR="R") then urban=0;
   if (HINCP<0) then HINCP=0;
   if (bld in ("02","03")) then housetyp=1;
   if (bld in ("04","05","06","07","08","09")) then housetyp=2;
   if (bld in ("01","10","")) then housetyp=3;
   numreg = region*1;
   location = 1+2*(numreg-1)+urban;
   if (np=1 and agep<18) then delete;
   if (location=1) then output pums1.pums_1;
   if (location=2) then output pums1.pums_2;
   if (location=3) then output pums1.pums_3;
   if (location=4) then output pums1.pums_4;
   if (location=5) then output pums1.pums_5;
   if (location=6) then output pums1.pums_6;
   if (location=7) then output pums1.pums_7;
   if (location=8) then output pums1.pums_8;
run;


%macro loc(n);
proc sort data=pums1.pums_&n; by serialno; run;
data pums1.np_&n;
   set pums1.pums_&n;
   by serialno;
   length ages $40 genders $20 age1 $1 age2 $2;
   retain person 0 nadult 0 nchild 0 famcat 0 ages genders;
   keep serialno nadult nchild famcat ages genders income;
   if (first.serialno) then do; nadult=0; nchild=0; person=0;
      ages="........................................"; genders="...................."; end;
   person = person+1;
   if (agep>=18) then nadult = nadult+1;
   if (agep<18)  then nchild = nchild+1;
   if (agep>=10) then age2 = agep;
   if (agep<10)  then do; age1=agep; age2="0"||age1; end;
   substr(ages,2*person-1,2) = age2;
   substr(genders,person,1)  = gender;
   if (last.serialno) then do;
      if (nadult=0) then delete;
	  if (nadult=1 and nchild=0) then famcat=1;
	  if (nadult=1 and nchild>0) then famcat=2;
	  if (nadult>1 and nchild=0) then famcat=3;
	  if (nadult>1 and nchild>0) then famcat=4;
	  income = hincp*adjinc;
      output;
   end;
run;

data pums1.loc&n;
   merge pums1.pums_&n pums1.np_&n;
   by serialno;
   drop wgtp hht npf nr r18 puma st n sex hisp rac1p region ur adjinc hincp bld area pop;
   rename jwmnp=commute numreg=region;
run;
%mend;

%loc(1);
%loc(2);
%loc(3);
%loc(4);
%loc(5);
%loc(6);
%loc(7);
%loc(8);

proc sql noprint;
   select count(*) into :num1 from pums1.loc1;
   select count(*) into :num2 from pums1.loc2;
   select count(*) into :num3 from pums1.loc3;
   select count(*) into :num4 from pums1.loc4;
   select count(*) into :num5 from pums1.loc5;
   select count(*) into :num6 from pums1.loc6;
   select count(*) into :num7 from pums1.loc7;
   select count(*) into :num8 from pums1.loc8;
run; quit;

proc sort data=pums1.loc1; by income; run;
data pums1.loc1a;
   set pums1.loc1;
   if (famcat=.) then delete;
   if      (_N_>2*&num1/3) then inccat=3;
   else if (_N_>  &num1/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;
proc freq data=pums1.loc1a; tables pool; run;

proc sort data=pums1.loc2; by income; run;
data pums1.loc2a;
   set pums1.loc2;
   if (famcat=.) then delete;
   if      (_N_>2*&num2/3) then inccat=3;
   else if (_N_>  &num2/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;
proc freq data=pums1.loc2a; tables pool; run;


proc sort data=pums1.loc3; by income; run;
data pums1.loc3a;
   set pums1.loc3;
   if (famcat=.) then delete;
   if      (_N_>2*&num3/3) then inccat=3;
   else if (_N_>  &num3/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;
proc freq data=pums1.loc3a; tables pool; run;


proc sort data=pums1.loc4; by income; run;
data pums1.loc4a;
   set pums1.loc4;
   if (famcat=.) then delete;
   if      (_N_>2*&num4/3) then inccat=3;
   else if (_N_>  &num4/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;
proc freq data=pums1.loc4a; tables pool; run;


proc sort data=pums1.loc5; by income; run;
data pums1.loc5a;
   set pums1.loc5;
   if (famcat=.) then delete;
   if      (_N_>2*&num5/3) then inccat=3;
   else if (_N_>  &num5/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;
proc freq data=pums1.loc5a; tables pool; run;


proc sort data=pums1.loc6; by income; run;
data pums1.loc6a;
   set pums1.loc6;
   if (famcat=.) then delete;
   if      (_N_>2*&num6/3) then inccat=3;
   else if (_N_>  &num6/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;
proc freq data=pums1.loc6a; tables pool; run;


proc sort data=pums1.loc7; by income; run;
data pums1.loc7a;
   set pums1.loc7;
   if (famcat=.) then delete;
   if      (_N_>2*&num7/3) then inccat=3;
   else if (_N_>  &num7/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;
proc freq data=pums1.loc7a; tables pool; run;



proc sort data=pums1.loc8; by income; run;
data pums1.loc8a;
   set pums1.loc8;
   if (famcat=.) then delete;
   if      (_N_>2*&num8/3) then inccat=3;
   else if (_N_>  &num8/3) then inccat=2;
   else                         inccat=1;
   pool = 72*(region-1) + 36*urban + 12*(housetyp-1) + 3*(famcat-1) + inccat;
run;

data loc8a;
   set pums1.loc8a;
   length st $2;
   st = substr(compid,1,2);
run;
proc freq data=loc8a; tables st; run;


data pums1.reg1;
   set pums1.loc1a pums1.loc2a;
   header = "pool, compid, recno, gender, race, ethnicity, age, pwgtp, ages, genders, commute, vehicles";
   file "C:/main/HEM/RPGen/pums_reg1.csv" dsd dlm=',';
   if _N_=1 then put header;
   put pool compid serialno gender race ethnic agep pwgtp ages genders commute veh;
run;

data pums1.reg2;
   set pums1.loc3a pums1.loc4a;
   header = "pool, compid, recno, gender, race, ethnicity, age, pwgtp, ages, genders, commute, vehicles";
   file "C:/main/HEM/RPGen/pums_reg2.csv" dsd dlm=',';
   if _N_=1 then put header;
   put pool compid serialno gender race ethnic agep pwgtp ages genders commute veh;
run;

data pums1.reg3;
   set pums1.loc5a pums1.loc6a;
   header = "pool, compid, recno, gender, race, ethnicity, age, pwgtp, ages, genders, commute, vehicles";
   file "C:/main/HEM/RPGen/pums_reg3.csv" dsd dlm=',';
   if _N_=1 then put header;
   put pool compid serialno gender race ethnic agep pwgtp ages genders commute veh;
run;

data pums1.reg4;
   set pums1.loc7a pums1.loc8a;
   header = "pool, compid, recno, gender, race, ethnicity, age, pwgtp, ages, genders, commute, vehicles";
   file "C:/main/HEM/RPGen/pums_reg4.csv" dsd dlm=',';
   if _N_=1 then put header;
   put pool compid serialno gender race ethnic agep pwgtp ages genders commute veh;
run;

proc contents data=pums1.loc1a; run;
proc freq data=pums1.loc1a; tables veh; run;
