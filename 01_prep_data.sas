	OPTIONS nofmterr nolabel;

	*set libraries; 
		libname s "\Source"; 
		libname p "\Posted";

	*load macros; 
		%let all_cog=cvlt_frl, ro_dr, ro_copy, blocks, dspan_f, dfor, ta_time, bnt_spon, wg_anim, 
			 wg_f, wg_s, tb_time, strink, cdsum, cdglobl, cfscore, cbscore;

	*load data; 
		data _studydates; set s.gem_dates; year_base=year(randomdt); keep idno randomdt date: year_base; run;
		data _np; set s.neuropsych; keep idno visitno age cvlt_frl ro_dr ro_copy blocks dspan_f dfor ta_time bnt_spon wg_anim wg_f wg_s tb_time strink npdt ; run; 
		data _cdr; set s.cdr; keep idno visitno cdsum cdglobl cd_dt; run; 	
		data _3mse; set s.mmse; keep idno visitno cfscore; run; 
		data _adas; set s.adas; keep idno visitno cbscore cbdt;  run; 
		data _demdxsummary; set s.demdxsummary; if idno=3200183 then visitno=11; keep idno visitno; run; 
		data _hh; set s.healthhabit; keep idno hhsmkst; run; 
		data _gemsap; set s.gems; keep pid agernd  male racef edu4 clinic bmi bpsystol bpdiasto pa_score smoke apoe_4f ndi_20 pm25hist_20yrave no2_preds_10yrave; run;  
		data _marital; set s.intmedhistory; keep idno visitno mhstatus mhstatuscur mhcurspc ; run; 
		data _ids; set s.ids; pid= publicid+100000; run; 

	****************************************; 
	**** Create long dataset so each row is a person-visit, each person has 16 visits, include study time variables,
	**** cog scores, dementia data; 

	*step 1: create long dataset with id, visit; 
		data dat01; 
			set _studydates; 
				date2=randomDt; 
			format date1-date16 mmddyy10.;
			drop randomdt; 
		run;

		data dat02; 
			set dat01; 
			array xdate(1:16) date1-date16; 
			do visitno=1 to 16; 
				date=xdate(visitno); output; 
			end; 
			drop date1-date16;
			format date mmddyy10.;
		run;

	*step 2: merge all cognitive data (np, cdr, 3mse, adas) to dat03; 
		proc sort data=_np;   by idno visitno; run; 
		proc sort data=_cdr;  by idno visitno; run; 
		proc sort data=_3mse; by idno visitno; run; 
		proc sort data=_adas; by idno visitno; run; 

		data dat03; 
			merge dat02 _np _cdr _3mse _adas; 
			by idno visitno; 
			s=1; *create an attendance variable; 
			if sum(&all_cog)=. then s=0;
		run; 

	*step 3: create study time variables (year, visit, study time, calendar time).
		need to split into two steps to account for screener exam administered before baseline visit;

		data dat04_screener; 
			set dat03; 
				if s=1 and date=. then date=npdt; *fix some missing dates; 
				if s=1 and date=. and npdt=. then date=cddt; *fix some missing dates; 
			where visitno=1;
				year=year(date);
		run; 
		
		data dat04_visits; 
			set dat03; 
				if s=1 and date=. then date=npdt; *fix some missing dates; 
				if s=1 and date=. and npdt=. then date=cddt; *fix some missing dates; 
			where visitno ge 2;
				by idno; 
				retain base_date;
				if first.idno then base_date=date;
				year=year(date);
				study_days=date-base_date; 	
				study_months=study_days/30.437;
				study_years=study_days/365.242;
		run; 

		*merge datasets; 
		data dat05; 
			set dat04_screener dat04_visits; 
		run;

		proc sort data=dat05; by idno descending visitno ; run; 			

		data dat06; 
			set dat05; 
				base_date2=lag1(base_date); 
				if visitno=1 then do; 
					study_days=date-base_date2;
					study_months=study_days/30.437;
					study_years=study_days/365.242; end;
			drop base_date:; 
		run;

		proc sort data=dat06; by idno visitno ; run; 

		data dat07; 
			retain idno date visitno year study_days study_months study_years;
			set dat06; 
		run; 

	*step 4: add dementia data- dementia dx, dementia, and ever dementia (ever dementia in this study); 
		proc sort data=dat07; by idno visitno; run; 
		proc sort data=_demdxsummary; by idno visitno; run; 

		data dat08; 
			merge dat07 (in=x) _demdxsummary (in=y); 
			by idno visitno; 
			if y then demdx=1; 
		run; 

		data dat09; 
			set dat08; 
			dem=demdx;
			by idno; 
			retain test; 
			if first.idno then call missing(test); *reset retained value for the first visit of each id; 
			if dem=1 then test=1; *when dem=1 then set the retained value; 
			if test=1 then dem=1; *if retaind value is set, then assign it to 1; 

			if dem=. then dem=0;
			if demdx=. then demdx=0;

			if demdx=0 and dem=0 then keep=1; 
			if demdx=1 and dem=1 then keep=1; 
			if demdx=0 and dem=1 then keep=0; *eventually exclude rows once there's a dementia dx;

			drop test; 
				
		run; 

		proc sql; 	
			create table dat10 as
			select *, sum(demdx) as dem_ever
			from dat09
			group by idno; 
		quit; 

		*remove outliers; 
			data dat11; 
				set dat10; 
				if visitno < 1 or visitno > 16 then delete; 
			run; 	
			proc sort data=dat11; by idno visitno; run; 

		
	*step 5: create standardize domain-secific cog function at each visit;
		*first some data prep: need to remove visit 1 (screening) and bring down results from visit and use those for visit 2 and inverse 
		the timed tests so higher is better for all tests - easier to interpret; 
			
		data dat12_subset; 
			set dat11; 
			where visitno in (1 2);
		run; 

		proc sql; 
			create table dat13_subset as 
			select idno, 
					max(date) as date, 
					max(visitno) as visitno, 
					max(year) as year, 
					max(study_days) as study_days,
					max(study_months) as study_months,
					max(study_years) as study_years,
					max(dfor) as dfor,
					max(ta_time) as ta_time,
					max(tb_time) as tb_time,
					max(cvlt_frl) as cvlt_frl,
					max(wg_f) as wg_f, 
					max(wg_s) as wg_s, 
					max(wg_anim) as wg_anim,
					max(blocks) as blocks,
					max(ro_copy) as ro_copy,
					max(ro_dr) as ro_dr,
					max(strink) as strink,
					max(bnt_spon) as bnt_spon,
					max(npdt)  as npdt,
					max(cdsum) as cdsum,
					max(cdglobl) as cdglobl,
					max(cddt) as cddt,
					max(cfscore) as cfscore,
					max(cbdt) as cbdt,
					max(cbscore) as cbscore,  
					max(s) as s,
					max(demdx) as demdx,
					max(dem) as dem,
					max(dem_ever) as dem_ever, 
					max(year_base) as year_base, 
					max(keep) as keep
			from dat12_subset
			group by idno
			order by idno
			; 
		quit;

		data dat14_subset; 
			set dat11; 
			where visitno > 2; 
		run; 

		data dat15; 
			set dat13_subset dat14_subset; 
			by idno; 			
				ta_time_inv=240-ta_time; *inverse trails tests so higher number = better performance; 
				tb_time_inv=240-tb_time;
				cbscore_inv=64-cbscore; *inverse adas-cog test;
		run; 

		%macro stat (test=); 
			proc means data=dat15; *calculate mean/std for each test using distribution of baseline population; 
				where visitno=2; 
				var &test.; 
				output out=stat_&test.(drop= _type_ _freq_) mean=&test._mu std=&test._sigma; 
				format &test. 6.3;
			run; 

			data stat1;
				merge stat_:; 
			run;

			data stat2 (drop=i); 
				set stat1; 
				do i=1 to 46035; *number of rows in dat15... that need mean and sd variables for all tests repeated down;
					output; 
				end; 
			run; 

		data dat16; 
			merge dat15 stat2; 
		run;

		proc stdize data=dat16 method=in(dat16) out=dat17_&test. oprefix sprefix=z_ pstat;   
			var &test. ; 
			location &test._mu; 
			scale &test._sigma;  
			format &test.  6.2;
		run;
			
		proc sort data=dat17_&test.; by idno visitno; run; 

		%mend stat; 
			%stat(test=cfscore);
			%stat(test=cbscore_inv); 
			%stat(test=cdsum); 
			%stat(test=cdglobl);
			%stat(test=cvlt_frl); 
			%stat(test=ro_dr);
			%stat(test=ro_copy);
			%stat(test=blocks);
			%stat(test=dfor);
			%stat(test=ta_time_inv);
			%stat(test=bnt_spon);
			%stat(test=wg_f);
			%stat(test=wg_s);
			%stat(test=wg_anim);
			%stat(test=tb_time_inv);
			%stat(test=strink);

		data dat18; 
			merge dat17_:;
			by idno visitno; 
			z_wg_all=(z_wg_anim+z_wg_f+z_wg_s)/3;
			keep idno age date visitno year year_base study_: z_: s dem: keep; 
		run;

		*CHECK to see how many instances when one test within a domain group is missing;
		%macro test (domain=, test1=, test2=);
		data test_&domain.; 
			set dat18;			
					if &test1. ne . AND &test2. ne . then impute_&domain.=0;  
					if &test1. = . and &test2. ne . then impute_&domain.=1; 
					if &test1. ne . and &test2. = . then impute_&domain.=1; 
					if &test1. = . and &test2. = . then impute_&domain.=0; 
			keep idno visitno &test1. &test2.  impute_&domain.; 
		run; 

		proc sort data=test_&domain.; 
			by impute_&domain.; 
		run; 
					
		data test_impute_&domain.; 	*89 tests; 
			set test_&domain.; 
			where impute_&domain.=1 and visitno in (2 9 10 11 12 13 14 15 16); ; 
		run; 
		
		proc sql; *66 unique indivs; 
			select count(distinct idno) as distinct_idno
			from test_impute_&domain.;
		quit;  
		%mend test; 
			%test(domain=mem, test1=z_cvlt_frl, test2=z_ro_dr); 	
			%test(domain=exec, test1=z_tb_time_inv, test2=z_strink); 
			%test(domain=atten, test1=z_dfor, test2=z_ta_time_inv); 
			%test(domain=lang, test1=z_bnt_spon, test2=z_wg_all); 
			%test(domain=constr, test1=z_ro_copy, test2=z_blocks); 
		
		*create standardized domains and impute missing if 1 test within a domain is missing; 
		%macro domain (domain=, test1=, test2=);
		data dat18_&domain.; 
			set dat18; 
					z_domain_&domain.=(z_&test1.+z_&test2.)/2; 
				 if z_domain_&domain. = . then do; 				 	
				 	z_domain_&domain.=max (z_&test1., z_&test2.); end;
		run; 

		proc sort data=dat18_&domain.; by idno visitno; run;

		%mend domain; 
			%domain(domain=mem, test1=cvlt_frl, test2=ro_dr); 
			%domain(domain=constr, test1=ro_copy, test2=blocks); 
			%domain(domain=atten, test1=dfor, test2=ta_time_inv); 
			%domain(domain=lang, test1=bnt_spon, test2=wg_all); 
			%domain(domain=exec, test1=tb_time_inv, test2=strink); 

		data dat19; 
			merge dat18_:;
			by idno visitno; 
			sum_domain=sum(z_domain_mem,z_domain_exec,z_domain_atten,z_domain_lang,z_domain_constr);
				if sum_domain=. then s_np=0; 
				else if sum_domain ne 0 then s_np=1;
			keep idno date age visitno year year_base study_: s_np dem: z_: keep; 
		run;

		*carry down imputed domains for missing domains (will use for weight creation); 
		%macro domain(domain=); 
		data dat20_&domain.;
			set dat19; 
			by idno visitno; 
			retain temp; 
			if first.idno then temp=.;
			if z_domain_&domain. ne . then temp=z_domain_&domain.; 
				else if z_domain_&domain.=. then z_domain_imp_&domain.=temp; 
			if z_domain_imp_&domain.=. then z_domain_imp_&domain.=temp;

			retain baseline_&domain.; 
			if first.idno then baseline_&domain.=z_domain_&domain.;

			s_&domain.=.;
				if z_domain_&domain.=. then s_&domain.=0; 
					else if z_domain_&domain. ne . then  s_&domain.=1; 
		run;

		proc sort data= dat20_&domain.; by idno visitno; run;

		%mend domain; 
			%domain(domain=mem); 
			%domain(domain=constr); 
			%domain(domain=atten); 
			%domain(domain=lang); 
			%domain(domain=exec); 

		data dat21; 
			merge dat20_:; 
			by idno visitno; 
		run; 	
		
	*step 5: prep gems cohort+ap data (_gemsap);
		data dat19_gemsap; 
			set _gemsap; 
				if male="1: male" then male="1";
					else if male= "0: female" then male="0";
				if racef="non-minority race" then white=1;
					else if racef="minority race" then white=0; 
				if trt="ginkgo" then trt=1; 
					else if trt="placebo" then trt=0; 
				if clinic="3: wfu" then clinic=2;  
					else if clinic="4: ucd" then clinic=1; 
					else if clinic="5: jhu" then clinic=4;
					else if clinic="6: pitt" then clinic=3; 
				new_male=input(male, comma9.);
				new_edu4=input(edu4, comma9.);
				new_pa_score=input(pa_score, comma9.); 
				new_smoke=input(smoke, comma9.); 
				new_clinic=input(clinic, comma9.);
				new_apoe4=input(apoe_4f, comma9.);
				drop racef male edu4 pa_score smoke clinic apoe_4f; 
				rename new_male=male 
					   new_edu4=edu_4 
    				   new_pa_score=pa_score
					   new_smoke=smoke
					   new_clinic=clinic 
					   new_apoe4=apoe4; 

		run; 

		data dat20_gemsap;
			merge dat19_gemsap _ids;
			by pid; 
			drop pid; 
		run; 

		*carry down time fixed covs and cog scores/domains; 
		data dat21_gemsap; 
				set dat20_gemsap; 
				by idno; 
				do visitno=2 to 16; *number of rows in dat15... that need mean and sd variables for all tests repeated down;
					output; 
				end; 
			run; 

		*create living condition and marital status variable and add to "dat19_gemsap" ;
		data dat21_living; 
			set _marital;

			*where do you live (1=alone, 2=w/ spouse, 3=with other family/friend, 4=retirement home/facility); 
				where mhstatuscur ne .;
				if mhstatuscur=6 then do;
					if find(mhcurspc,"WIFE") then living_new=2;
					if find(mhcurspc,"HUSBAND") then living_new=2;
					if find(mhcurspc,"SPOUSE") then living_new=2;
					if find(mhcurspc,"HUSBAN") then living_new=2;
					if find(mhcurspc,"SIGNIFICANT") then living_new=2;
					if find(mhcurspc,"RETIREMENT") then living_new=4;
					if find(mhcurspc,"DAUGHTER") then living_new=3;
					if find(mhcurspc,"SON") then living_new=3;
					if find(mhcurspc,"FAMILY") then living_new=3;
					if find(mhcurspc,"SISTER") then living_new=3;
					if find(mhcurspc,"UNASSISTED LIVING") then living_new=1;
					if find(mhcurspc,"SENIOR") then living_new=4;
					if find(mhcurspc,"BATHROOM") then living_new=4;
					if find(mhcurspc,"MARRIED") then living_new=2;
					if find(mhcurspc,"MAINTAIN HOME FOR MYSELF AND") then living_new=3;
					if find(mhcurspc,"CHILDREN") then living_new=3;
					if find(mhcurspc,"CHILDREN") then living_new=3;
					if find(mhcurspc,"DOG") then living_new=1;
					if find(mhcurspc,"LIVING IN MY OWN APARTMENT") then living_new=1;
					if find(mhcurspc,"RELIGIOUS") then living_new=4;
					if find(mhcurspc,"MONASTERY") then living_new=4;
					if find(mhcurspc,"CONDOMINIUM") then living_new=1;
					if find(mhcurspc,"SENIR") then living_new=4;
					if find(mhcurspc,"SENIR") then living_new=4;
					if find(mhcurspc,"FRIEND") then living_new=3;
					if find(mhcurspc,"ETIREMENT") then living_new=4;
					if find(mhcurspc,"CONVENT") then living_new=4;
					if find(mhcurspc,"COMPANION") then living_new=4;
					if find(mhcurspc,"ASSISTED LIVING") then living_new=4;
					if find(mhcurspc,"HEALTH CENTER") then living_new=4;
					if find(mhcurspc,"ALSO LIVING WITH OUR HANDICAPP") then living_new=3;
					if find(mhcurspc,"ALONE") then living_new=1;
					if find(mhcurspc,"DOWNSTAIRS-SHE LIVES UPSTAIRS") then living_new=3; 
					if find(mhcurspc, "GRANDCHILDREN") then living_new=3; 
				end;	
	
				if mhstatuscur ne 6 then living_new=mhstatuscur; 
				if mhstatuscur=5 then living_new=4; 

				*collapse groups; 
					home_partner=living_new; 
						if living_new in (1 3 4) then home_partner=0; *home_status 1= living alone or other, 2=spouse/partner; 
						if living_new=2 then home_partner=1; 
				*marital status; 
				if mhstatus=1 then married=1; *currently married; 
					else if mhstatus in (2 3 4 5) then married=2; *previously or never married - majority are widowed; 

			keep idno visitno home_partner married; 
		run; 

		data dat22_living; 
			set dat21_living; 
			where visitno=4; 
			keep idno home_partner married; 
		run; 

	*merge gems covariate and ap data with cog scores; 
		proc sort data=dat21_gemsap; by idno visitno; run; 
		proc sort data=dat21; by idno visitno; run; 
		proc sort data=dat22_living; by idno; run; 
		proc sort data=_hh; by idno; run; 

		data dat22; 
			merge dat21 dat21_gemsap; 
			by idno visitno; 
		run; 

		proc sort data=dat22; by idno; run; 

		data dat23; 
			merge dat22 dat22_living _hh; 
			by idno; 
		run; 

		data dat24; set dat23; where keep=1; drop keep; run; *drops obs after dem dx; 

		data dat25; 
			set dat24; 
			if visitno=2 and date ne . then totalpop=1; 
			if visitno ne 2 or . then do; 
				if date lt '01Aug2004'd then invite_subset=1; 
				else if date ge '01Aug2004'd then invite_subset=0; 
				if date=. then early_visit=.; end; 
		run; 

		proc univariate data=dat25 noprint; var pm25hist_20yrave; output out=cutoff_ap pctlpre=p_ pctlpts= 33.3 66.7 100; run; 

		data dat26; 
			merge dat25 cutoff_ap;
			retain pm25_cutoff_T1 pm25_cutoff_T2 pm25_cutoff_T3; 
			if _n_=1 then do; 
				pm25_cutoff_T1=p_33_3; 
				pm25_cutoff_T2=p_66_7; 
				pm25_cutoff_T3=p_100; end; 
			pm25_tert=.; 
				if pm25hist_20yrave < pm25_cutoff_T1 then pm25_tert=1; 
					else if pm25hist_20yrave < pm25_cutoff_T2 then pm25_tert=2; 
					else if pm25hist_20yrave =< pm25_cutoff_T3 then pm25_tert=3; 
			drop p_33_3 p_66_7 p_100; 
		run; 

	data dat27; *some final data managemnet; 
		set dat26; 
			pm25_5=PM25Hist_20YRAVE/5;
			age=.;
				age=agernd+study_years; 
			age_c=age-78.61;
			age_10=age/10; 
			age_c10=age_10-7.86;
			no2_5=no2_preds_10yrave/5; 
			practice=1; 
			if HHsmkSt=1 then ever_smoke=0; 
				else if HHsmkSt in (2 3 4) then ever_smoke=1; 
				else if HHsmkSt in (9 .) then ever_smoke=.; 
			by idno; 
			if first.idno then do; 
				practice=0; end; 
	run;

	proc sort data=dat27; by idno; run; 
	proc sql; 
		create table final_dat as 
		select *, 
			max(study_years) as total_years, 
			sum(s_np) as total_visit
		from dat27
		group by idno; 
	quit; 

		data p.dat_ap_cog; set final_dat; run; 


 


		

 


