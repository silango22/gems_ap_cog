
*set options, load and subset data; 
	options nofmterr nolabel;

	libname p "\Posted";
	data _ap_cog; set p.dat_ap_cog; run; 

	data dat; 
		set _ap_cog; 
		where visitno in (2 8 9 10 11 12 13 14 15 16) and pm25_5 ne .; *only keeping visits where everyone was invited; 
		pm25_iqr=pm25hist_20yrave/2.37595;
		no2_iqr=no2_preds_10yrave/6.65324; 
		keep idno visitno dem_ever s_: agernd bmi bpsystol bpdiasto male edu_4 clinic ndi_20 pm25_tert age age_c age_10 age_c10 pm25_5 no2_5 pm25_iqr no2_iqr practice pm25hist_20yrave no2_preds_10yrave 
		    	 z_domain: baseline: pa_score ever_smoke txgroup study_years total_years total_visit home_partner married year_base demdx s_np z_tb_time_inv;
	run; 
	
/*---------------------------------------------------------MAIN RESULTS---------------------------------------------------------*/
/*TABLE 1. Neuropsych Battery and Cog Domains (no code needed)-----------------------------------------------------------------*/ 
/*TABLE 2. Descriptives---------------------------------------------------------------------------------------------------------*/ 
	*all participants; 
		proc tabulate data=dat missing;
			where visitno=2 and s_np=1;
			class male edu_4 married home_partner ever_smoke dem_ever; 
			var agernd bmi total_years total_visit ndi_20; 
			table (agernd bmi  total_years total_visit ndi_20), (mean std); 
			table (male edu_4 married home_partner ever_smoke dem_ever), (n colpctn);
			format male male.;
		run; 

		*stratified by pm25 tertile; 
		proc sort data=dat; by pm25_tert; run; 
		proc tabulate data=dat missing;
			where visitno=2 and s_np=1;
			var agernd bmi total_years total_visit ndi_20; 
			class pm25_tert male edu_4 married home_partner ever_smoke dem_ever; 
			table (agernd bmi total_years total_visit ndi_20),(pm25_tert)*(mean std); 
			table (male edu_4 married home_partner ever_smoke dem_ever),(pm25_tert)*(n colpctn);
			format male male.;
		run; 

/*TABLE 3. ap and cognitive level - ap, age, ap*age----------------------------------------------------------------------------*/ 

	*First create stabilized weights & truncate top and bottom percentiles;
		%let covs_all=txgroup clinic agernd male edu_4 married home_partner ever_smoke bmi bpsystol bpdiasto pm25hist_20yrave 
			 no2_preds_10yrave ndi_20 baseline_mem baseline_atten baseline_constr 
			 baseline_exec baseline_lang; 

		%macro visit (visitno=, domain=, covs=); 
			proc logistic data=dat;
				where visitno=&visitno.; 
				class s_np txgroup clinic male edu_4 married home_partner ever_smoke; 
				model s_np (event='1')= &covs_all;
				output out=denom_&visitno. prob=pdenom; 
			run; 

			proc logistic data=dat noprint;
				where visitno=&visitno.;  
				model s_np(event='1')=pm25hist_20yrave; output out=num_&visitno. prob=pnum; 
			run;

			data dat_ipw_&visitno.; 
				merge num_&visitno. denom_&visitno.; 
				w=1/pdenom; 
				sw=pnum/pdenom;
			run; 

			proc univariate data=dat_ipw_&visitno. noprint; var sw; output out=cutoff_&visitno. pctlpre=p_ pctlpts=2.5 97.5; run;

			data dat_sw_&visitno._1; 
				merge dat_ipw_&visitno. cutoff_&visitno.; 
				retain p2_5 p97_5; 
					if _n_=1 then do; 
						p2_5=p_2_5; 
						p97_5=p_97_5; end; 
				if sw > p97_5 then delete; 
				if sw < p2_5 then delete; 
				drop p_2_5 p_97_5 _level_;
				keep idno visitno sw; 
			run;
		*merge visit-specific weights into dataset; 
			data dat1;
				set dat_sw:;
			run; 

			proc sort data=dat1; by idno visitno; run; 
			proc sort data=dat; by idno visitno; run;
			
			data dat2; 
				merge dat1 dat; 
				by idno visitno; 
				if visitno=2 then sw=1;
			run; 

			proc sort data=dat2; by idno visitno; run;

		%mend; 
			%visit(visitno=3, covs=&covs_all); 
			%visit(visitno=4, covs=&covs_all); 
			%visit(visitno=5, covs=&covs_all); 
			%visit(visitno=6, covs=&covs_all); 
			%visit(visitno=7, covs=&covs_all);
			%visit(visitno=8, covs=&covs_all); 
			%visit(visitno=9, covs=&covs_all); 
			%visit(visitno=10, covs=&covs_all); 
			%visit(visitno=11, covs=&covs_all); 
			%visit(visitno=12, covs=&covs_all);
			%visit(visitno=13, covs=&covs_all); 
			%visit(visitno=14, covs=&covs_all); 
			%visit(visitno=15, covs=&covs_all); 
			%visit(visitno=16, covs=&covs_all); 

	data dat3; 
		set dat2;
		agernd_c=agernd-78.4717438; 
	run; 

	%macro model(domain=, ap=);
		ods output solutionf=&ap._&domain.;
		proc mixed data=dat3 method=ml; 
			class idno edu_4 clinic (ref="3"); 
			model z_domain_&domain.=&ap. study_years ndi_20 agernd_c txgroup clinic male edu_4 practice 
									&ap.*study_years
									agernd_c*study_years									
									/solution cl; 
			random intercept study_years /subject=idno; 
			weight sw;
		run;

		data T3_&ap._&domain.; 
			set &ap._&domain.; 
			where effect in ("&ap." "&ap.*study_years") ;
			domain="&domain.";
			ap="&ap.";
			est=round(estimate, 0.001) || ' ('||round(lower, 0.001) || ', ' || round(upper, 0.001) || ')'; 
			keep domain ap effect est;
		run; 
	%mend model; 
		%model(domain=mem, ap=pm25_iqr); 
		%model(domain=exec, ap=pm25_iqr); 
		%model(domain=atten, ap=pm25_iqr); 
		%model(domain=lang, ap=pm25_iqr); 
		%model(domain=constr, ap=pm25_iqr); 

		%model(domain=mem, ap=no2_iqr); 
		%model(domain=exec, ap=no2_iqr); 
		%model(domain=atten, ap=no2_iqr); 
		%model(domain=lang, ap=no2_iqr); 
		%model(domain=constr, ap=no2_iqr); 

	data T3_pm25; set T3_pm25_iqr_mem T3_pm25_iqr_exec T3_pm25_iqr_atten T3_pm25_iqr_lang T3_pm25_iqr_constr; run; 
	data T3_no2;  set  T3_no2_iqr_mem  T3_no2_iqr_exec  T3_no2_iqr_atten  T3_no2_iqr_lang  T3_no2_iqr_constr; run; 
		
	proc print data=T3_pm25; run; 
	proc print data=T3_no2; run; 

/*FIGURES 1 and 2. Associations between air pollution and cognitive decline--------------------------------------------------------*/ 
				
	*models for plots; 
		%macro model(domain=, ap=, ap1=, ap2=, file=, color1=, color2=, title1=, title2=);
			proc mixed data=dat2 covtest noclprint noitprint method=ml; 
				class idno edu_4 clinic; 
				model z_domain_&domain.=&ap. study_years agernd txgroup clinic male edu_4 practice ndi_20 
								        &ap.*study_years
										agernd*study_years
										/solution; 
				random intercept study_years/subject=idno; 
				weight sw;
				store stored; 
			run;

		*plot predicted trajectories; 
			data predict; *set covs to ref or mean; 
				txgroup=0;
				practice=1;
				male=0;
				clinic=3;
				edu_4=0;
				ndi_20=0;
				agernd=78;
				do &ap. = &ap1., &ap2.; *10th and 90th percentile at baseline; 
				do study_years = 0 to 6 by 1; 
					output; 
				end; 
				end;
			run; 

			proc plm source=stored; 
				 score data=predict out=plot_&ap._&domain. predicted=pred lclm=lower uclm=upper; 
			run; 

			proc sort data=plot_&ap._&domain.; by &ap.; run; 

	 		ods graphics on / attrpriority=none imagename="&file._&domain." imagefmt=jpg noborder width=2.5in height=2.5in;
			ods html image_dpi=300; 
		proc sgplot data=plot_&ap._&domain. noautolegend; 
					styleattrs datacolors=(&color1. &color2.) datacontrastcolors=(&color1. &color2.) datalinepatterns=(solid solid);
					band x=study_years upper=upper lower=lower / group=&ap. transparency=0.8 name="ci";
					series x=study_years y=pred / group=&ap.; 
					yaxis values=(-1.0 to 0.2 by 0.2) label="z-score"; 
					xaxis label = "years after enrollment";
					title "&title1."; 
					keylegend "ci" /title="&title2." noborder;
			run;
		%mend model; 
			%model(domain=mem, ap=pm25hist_20yrave, ap1=15, ap2=20, file=F2_pm25, color1=cxee9b00, color2=cxae2012, title1= Memory, title2=PM2.5 (µg/m³)); 
			%model(domain=exec, ap=pm25hist_20yrave, ap1=15, ap2=20, file=F2_pm25, color1=cxee9b00, color2=cxae2012, title1= Executive Function, title2=PM2.5 (µg/m³)); 
			%model(domain=atten, ap=pm25hist_20yrave, ap1=15, ap2=20, file=F2_pm25, color1=cxee9b00, color2=cxae2012, title1=Attention, title2=PM2.5 (µg/m³));
			%model(domain=lang, ap=pm25hist_20yrave, ap1=15, ap2=20, file=F2_pm25, color1=cxee9b00, color2=cxae2012, title1=Language, title2=PM2.5 (µg/m³)); 
			%model(domain=constr, ap=pm25hist_20yrave, ap1=15, ap2=20, file=F2_pm25, color1=cxee9b00, color2=cxae2012, title1=Visuospatial Function, title2=PM2.5 (µg/m³)); 

			%model(domain=mem, ap=no2_preds_10yrave, ap1=10, ap2=22, file=F3_no2, color1=cx778da9, color2=cx1b263b, title1=Memory, title2=NO2 (ppb)); 
			%model(domain=exec, ap=no2_preds_10yrave, ap1=10, ap2=22, file=F3_no2, color1=cx778da9, color2=cx1b263b, title1= Executive Function, title2=NO2 (ppb)); 
			%model(domain=atten, ap=no2_preds_10yrave, ap1=10, ap2=22, file=F3_no2, color1=cx778da9, color2=cx1b263b, title1=Attention, title2=NO2 (ppb));
			%model(domain=lang, ap=no2_preds_10yrave, ap1=10, ap2=22, file=F3_no2, color1=cx778da9, color2=cx1b263b, title1=Language, title2=NO2 (ppb)); 
			%model(domain=constr, ap=no2_preds_10yrave, ap1=10, ap2=22, file=F3_no2, color1=cx778da9, color2=cx1b263b, title1=Visuospatial Function, title2=NO2 (ppb)); 
	
/*-----------------------------------------------SUPPLEMENTARY RESULTS-----------------------------------------------*/
/*SUPP FIGURE 1. NP battery administration for sample participants (no code needed)-------*/
/*SUPP FIGURE 2. Cognitive performance over study visits (descriptive plots to justify dropping visits 3-7)-------*/
	%macro s2plot(domain=, title=);
		ods graphics on / imagename="Fig_S2_&domain." imagefmt=png noborder width=4in height=3in ;
		proc sgplot data=_ap_cog noautolegend; 
			vline visitno / 
				response=z_domain_&domain. stat=mean limitstat=stderr 
				lineattrs=(color=black) markers markerattrs=(color=black symbol=squareFilled); 
			title "&title.";
			xaxis label = "Visit Number"; 
			yaxis label= "z-score"; 
		run;
	%mend s2plot; 
		%s2plot(domain=mem, title=Memory); 
		%s2plot(domain=exec, title=Executive Function); 
		%s2plot(domain=atten, title=Attention); 
		%s2plot(domain=lang, title=Language); 
		%s2plot(domain=constr, title=Construction); 

/*SUPP FIGURE 3. Covariate balance plot (standardized mean difference)------------------------------------------*/ 
	%let covs_all= no2_preds_10yrave pm25hist_20yrave ndi_20 
				   baseline_constr baseline_lang baseline_atten baseline_exec baseline_mem 
				   bpdiasto bpsystol bmi ever_smoke home_partner married edu_4 male agernd clinic txgroup; 

	%macro covbal (visitno=, covs=); 
		data dat_&visitno.; set dat; where visitno=&visitno.; run; 
		ods output stddiff=covbal1_v&visitno.;
		proc psmatch data=dat_&visitno. region=allobs; 
			class s_np;
			psmodel s_np(treated='1')=&covs.;
			assess ps var=(&covs.)/weight=atewgt; 
		run; 

		data covbal2_v&visitno.; 
			set covbal1_v&visitno.; 
			where obs="All" or obs="Weighted";
			keep  variable obs stddiff; 
		run;

	proc format; 
			value $var "Prop Score"="propensity score"
				            "txGroup" = "treatment" 
							"agernd"="age_0"
							"edu_4" = "edu"
							"z_domain_imp_mem" = "z_memory"
							"z_domain_imp_exec" = "z_exec_function"
							"z_domain_imp_atten" = "z_attention" 
							"z_domain_imp_lang" = "z_language"
							"z_domain_imp_constr" = "z_visuospatial"
							"NDI_20" = "NDI"
							"PM25Hist_20YRAVE" = "PM2.5"
							"NO2_preds_10YRAVE" = "NO2"; 
			value $obs "All" = "Unweighted"			
			;

		run; 
		ods graphics on / reset=index imagename="FS3_v&visitno." imagefmt=pdf attrpriority=none width=4in height=5in;
		proc sgplot data=covbal2_v&visitno.;
			styleattrs  datacontrastcolors=(cxe41a1c cx377eb8) datasymbols=(squarefilled trianglefilled) datalinepatterns=(solid solid);
			scatter x=stddiff y=variable/group=obs markerattrs=(size=8); 
			refline 0 /axis=x transparency=.5 lineattrs=(thickness=1 color=cx999999 pattern=solid);
			yaxis display =(nolabel); 
			xaxis values=(-0.5 to 0.75 by 0.25)  label="Standardized Mean Difference";
			title "Visit &visitno."; 
			format variable $var. obs $obs.; 
		run;
	%mend; 
		%covbal(visitno=8, covs= &covs_all); 
		%covbal(visitno=12, covs= &covs_all); 

/*SUPP TABLE 1. Unweighted model results ------------------------------------------*/ 
	%macro model(domain=, ap=);
		ods output solutionf=&ap._&domain.;
		proc mixed data=test method=ml; 
			class idno edu_4 clinic (ref="3"); 
			model z_domain_&domain.=&ap. study_years ndi_20 agernd_c txgroup clinic male edu_4 practice 
									&ap.*study_years
									agernd_c*study_years									
									/solution cl; 
			random intercept study_years /subject=idno; 
		run;

		data ST1_&ap._&domain.; 
			set &ap._&domain.; 
			where effect in ("&ap." "&ap.*study_years") ;
			domain="&domain.";
			ap="&ap.";
			est=round(estimate, 0.001) || ' ('||round(lower, 0.001) || ', ' || round(upper, 0.001) || ')'; 
			keep domain ap effect est;
		run; 
	%mend model; 
		%model(domain=mem, ap=pm25_iqr); 
		%model(domain=exec, ap=pm25_iqr); 
		%model(domain=atten, ap=pm25_iqr); 
		%model(domain=lang, ap=pm25_iqr); 
		%model(domain=constr, ap=pm25_iqr); 

		%model(domain=mem, ap=no2_iqr); 
		%model(domain=exec, ap=no2_iqr); 
		%model(domain=atten, ap=no2_iqr); 
		%model(domain=lang, ap=no2_iqr); 
		%model(domain=constr, ap=no2_iqr); 

	data ST1_pm25; set ST1_pm25_iqr_mem ST1_pm25_iqr_exec ST1_pm25_iqr_atten ST1_pm25_iqr_lang ST1_pm25_iqr_constr; run; 
	data ST1_no2;  set  ST1_no2_iqr_mem  ST1_no2_iqr_exec  ST1_no2_iqr_atten  ST1_no2_iqr_lang  ST1_no2_iqr_constr; run; 
		
	proc print data=ST1_pm25; run; 
	proc print data=ST1_no2; run; 

/*SUPP TABLE 2. Move baseline to visit 8 --------------------------------------------------*/  
data supp_dat; 
		set _ap_cog; 
		where visitno in (8 9 10 11 12 13 14 15 16) and pm25_5 ne .; *only keeping visits where everyone was invited; 
		pm25_iqr=pm25hist_20yrave/2.37595;
		no2_iqr=no2_preds_10yrave/6.65324; 
		agernd_c=agernd-78.4717438; 
		keep idno visitno agernd_c dem_ever s_: agernd bmi bpsystol bpdiasto male edu_4 clinic ndi_20 pm25_tert age age_c age_10 age_c10 pm25_5 no2_5 pm25_iqr no2_iqr practice pm25hist_20yrave no2_preds_10yrave 
		    	 z_domain: baseline: pa_score ever_smoke txgroup study_years total_years total_visit home_partner married year_base demdx s_np;
	run; 

	*First create stabilized weights & truncate top and bottom percentiles;
		%let covs_all=txgroup clinic agernd male edu_4 married home_partner ever_smoke bmi bpsystol bpdiasto pm25hist_20yrave 
			 no2_preds_10yrave ndi_20 baseline_mem baseline_atten baseline_constr 
			 baseline_exec baseline_lang; 

		%macro visit (visitno=, domain=, covs=); 
			proc logistic data=supp_dat;
				where visitno=&visitno.; 
				class s_np txgroup clinic male edu_4 married home_partner ever_smoke; 
				model s_np (event='1')= &covs_all;
				output out=denom_&visitno. prob=pdenom; 
			run; 

			proc logistic data=supp_dat noprint;
				where visitno=&visitno.;  
				model s_np(event='1')=pm25hist_20yrave; output out=num_&visitno. prob=pnum; 
			run;

			data dat_ipw_&visitno.; 
				merge num_&visitno. denom_&visitno.; 
				w=1/pdenom; 
				sw=pnum/pdenom;
			run; 

			proc univariate data=dat_ipw_&visitno. noprint; var sw; output out=cutoff_&visitno. pctlpre=p_ pctlpts=2.5 97.5; run;

			data dat_sw_&visitno._1; 
				merge dat_ipw_&visitno. cutoff_&visitno.; 
				retain p2_5 p97_5; 
					if _n_=1 then do; 
						p2_5=p_2_5; 
						p97_5=p_97_5; end; 
				if sw > p97_5 then delete; 
				if sw < p2_5 then delete; 
				drop p_2_5 p_97_5 _level_;
				keep idno visitno sw; 
			run;
		*merge visit-specific weights into dataset; 
			data dat1;
				set dat_sw:;
			run; 

			proc sort data=dat1; by idno visitno; run; 
			proc sort data=supp_dat; by idno visitno; run;
			
			data dat2; 
				merge dat1 supp_dat; 
				by idno visitno; 
				if visitno=8 then sw=1;
			run; 

			proc sort data=dat2; by idno visitno; run;

		%mend; 
			%visit(visitno=9, covs=&covs_all); 
			%visit(visitno=10, covs=&covs_all); 
			%visit(visitno=11, covs=&covs_all); 
			%visit(visitno=12, covs=&covs_all);
			%visit(visitno=13, covs=&covs_all); 
			%visit(visitno=14, covs=&covs_all); 
			%visit(visitno=15, covs=&covs_all); 
			%visit(visitno=16, covs=&covs_all); 

	data sens_movebaseline; 
		set dat2;
		agernd_c=agernd-78.4717438; 
		study_yrs2=study_years-3; 
	run; 

	%macro model(domain=, ap=);
		ods output solutionf=&ap._&domain.;
		proc mixed data=sens_movebaseline method=ml; 
			class idno edu_4 clinic (ref="3"); 
			model z_domain_&domain.=&ap. study_yrs2 ndi_20 agernd_c txgroup clinic male edu_4 practice 
									&ap.*study_yrs2
									agernd_c*study_yrs2									
									/solution cl; 
			random intercept study_yrs2 /subject=idno; 
			weight sw;
		run;

		data T3_&ap._&domain.; 
			set &ap._&domain.; 
			where effect in ("&ap." "&ap.*study_yrs2") ;
			domain="&domain.";
			ap="&ap.";
			est=round(estimate, 0.001) || ' ('||round(lower, 0.001) || ', ' || round(upper, 0.001) || ')'; 
			keep domain ap effect est;
		run; 
	%mend model; 
		%model(domain=mem, ap=pm25_iqr); 
		%model(domain=exec, ap=pm25_iqr); 
		%model(domain=atten, ap=pm25_iqr); 
		%model(domain=lang, ap=pm25_iqr); 
		%model(domain=constr, ap=pm25_iqr); 

		%model(domain=mem, ap=no2_iqr); 
		%model(domain=exec, ap=no2_iqr); 
		%model(domain=atten, ap=no2_iqr); 
		%model(domain=lang, ap=no2_iqr); 
		%model(domain=constr, ap=no2_iqr); 

	data ST3_pm25; set T3_pm25_iqr_mem T3_pm25_iqr_exec T3_pm25_iqr_atten T3_pm25_iqr_lang T3_pm25_iqr_constr; run; 
	data ST3_no2;  set  T3_no2_iqr_mem  T3_no2_iqr_exec  T3_no2_iqr_atten  T3_no2_iqr_lang  T3_no2_iqr_constr; run; 
		
	proc print data=ST3_pm25; run; 
	proc print data=ST3_no2; run; 

/*SUPP TABLE 3. change time axis to age insatead of years --------------------------------------------------*/  
	data supp_dat; 
		set test; 
		age_c=age-78; 
	run;

	%macro model(domain=, ap=);
		ods output solutionf=&ap._&domain.;
		proc mixed data=supp_dat method=ml; 
			class idno edu_4 clinic (ref="3"); 
			model z_domain_&domain.=&ap. age_c ndi_20 agernd_c txgroup clinic male edu_4 practice year_base
									&ap.*age_c
									age_c*year_base									
									/solution cl; 
			random intercept age_c /subject=idno; 
			weight sw;
		run;

		data T3_&ap._&domain.; 
			set &ap._&domain.; 
			where effect in ("&ap." "&ap.*age_c") ;
			domain="&domain.";
			ap="&ap.";
			est=round(estimate, 0.001) || ' ('||round(lower, 0.001) || ', ' || round(upper, 0.001) || ')'; 
			keep domain ap effect est;
		run; 
	%mend model; 
		%model(domain=mem, ap=pm25_iqr); 
		%model(domain=exec, ap=pm25_iqr); 
		%model(domain=atten, ap=pm25_iqr); 
		%model(domain=lang, ap=pm25_iqr); 
		%model(domain=constr, ap=pm25_iqr); 

		%model(domain=mem, ap=no2_iqr); 
		%model(domain=exec, ap=no2_iqr); 
		%model(domain=atten, ap=no2_iqr); 
		%model(domain=lang, ap=no2_iqr); 
		%model(domain=constr, ap=no2_iqr); 

	data ST4_pm25; set T3_pm25_iqr_mem T3_pm25_iqr_exec T3_pm25_iqr_atten T3_pm25_iqr_lang T3_pm25_iqr_constr; run; 
	data ST4_no2;  set  T3_no2_iqr_mem  T3_no2_iqr_exec  T3_no2_iqr_atten  T3_no2_iqr_lang  T3_no2_iqr_constr; run; 
		
	proc print data=ST4_pm25; run; 
	proc print data=ST4_no2; run; 


/*SUPP TABLES 4 and 5 . stratify by age at baseline  --------------------------------------------------*/

proc univariate data=test; *min=72, max=96, median=78, 75th = 80;
	where visitno=2; 
	var agernd; 
	histogram;
run;

data test; 
	set dat2; 
	age0_older=.;
	if agernd gt 80 then age0_older=1; *75th percentile; 
		else if agernd le 80 then age0_older=0; 
	agernd_c=agernd-78.4717438; 
run;

%macro model(domain=, ap=, older=);
		ods output solutionf=&ap._&domain.;
		proc mixed data=test method=ml; 
			where age0_older=&older.;
			class idno edu_4 clinic (ref="3"); 
			model z_domain_&domain.=&ap. study_years ndi_20 agernd_c txgroup clinic male edu_4 practice 
									&ap.*study_years
									agernd_c*study_years									
									/solution cl; 
			random intercept study_years /subject=idno; 
			weight sw;
		run;

		data T3_&ap._&domain._&older.; 
			set &ap._&domain.; 
			where effect in ("&ap." "&ap.*study_years") ;
			domain="&domain.";
			ap="&ap.";
			est=round(estimate, 0.001) || ' ('||round(lower, 0.001) || ', ' || round(upper, 0.001) || ')'; 
			keep domain ap effect est;
		run; 
	%mend model; 
		%model(domain=mem, ap=pm25_iqr, older=0); 
		%model(domain=exec, ap=pm25_iqr, older=0); 
		%model(domain=atten, ap=pm25_iqr, older=0); 
		%model(domain=lang, ap=pm25_iqr, older=0); 
		%model(domain=constr, ap=pm25_iqr, older=0); 

		%model(domain=mem, ap=pm25_iqr, older=1); 
		%model(domain=exec, ap=pm25_iqr, older=1); 
		%model(domain=atten, ap=pm25_iqr, older=1); 
		%model(domain=lang, ap=pm25_iqr, older=1); 
		%model(domain=constr, ap=pm25_iqr, older=1); 

		%model(domain=mem, ap=no2_iqr, older=0); 
		%model(domain=exec, ap=no2_iqr, older=0); 
		%model(domain=atten, ap=no2_iqr, older=0); 
		%model(domain=lang, ap=no2_iqr, older=0); 
		%model(domain=constr, ap=no2_iqr, older=0); 

		%model(domain=mem, ap=no2_iqr, older=1); 
		%model(domain=exec, ap=no2_iqr, older=1); 
		%model(domain=atten, ap=no2_iqr, older=1); 
		%model(domain=lang, ap=no2_iqr, older=1); 
		%model(domain=constr, ap=no2_iqr, older=1); 

	data ST4_pm25_0; set T3_pm25_iqr_mem_0 T3_pm25_iqr_exec_0 T3_pm25_iqr_atten_0 T3_pm25_iqr_lang_0 T3_pm25_iqr_constr_0; run; 
	data ST4_pm25_1; set T3_pm25_iqr_mem_1 T3_pm25_iqr_exec_1 T3_pm25_iqr_atten_1 T3_pm25_iqr_lang_1 T3_pm25_iqr_constr_1; run; 

	data ST4_no2_0; set T3_no2_iqr_mem_0 T3_no2_iqr_exec_0 T3_no2_iqr_atten_0 T3_no2_iqr_lang_0 T3_no2_iqr_constr_0; run; 
	data ST4_no2_1; set T3_no2_iqr_mem_1 T3_no2_iqr_exec_1 T3_no2_iqr_atten_1 T3_no2_iqr_lang_1 T3_no2_iqr_constr_1; run; 
		
	proc print data=ST4_pm25_0; run; 
	proc print data=ST4_pm25_1; run; 
	proc print data=ST4_no2_0; run; 
	proc print data=ST4_no2_1; run; 

/*SUPP TABLE 6. Drop obs with missing scores - no imputation ----------------------*/ 

*CHECK to see how many instances when one test within a domain group is missing;
%macro test (domain=, test1=, test2=);
	data test_&domain.; 
			set dat18;			 *dat18 from prep data v2 code; 
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
			where impute_&domain.=1 and visitno in (2 8 9 10 11 12 13 14 15 16);
			keep idno visitno impute_&domain.; 
		run; 

	*create dataset of id, visitno, and domain so we can exclude these from analysis; 
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

		data impute1; *dataset of all obs, with indicator for impute yes/no; 
			set test_impute_: ; 
		run;

		*merge with main dataset;

		proc sort data=test; by idno visitno; run; 
		proc sort data=impute1; by idno visitno; run; 

		data dat_noimpute; 
			merge test impute1;
			by idno visitno; 
		run; 
			
		*run models; 
		%macro model(domain=, ap=);
	ods output solutionf=&ap._&domain.;
	proc mixed data=dat_noimpute method=ml; 
		where impute_&domain. ne 1;
		class idno edu_4 clinic (ref="3"); 
		model z_domain_&domain.=&ap. study_years ndi_20 agernd_c txgroup clinic male edu_4 practice 
								&ap.*study_years
								agernd_c*study_years									
								/solution cl; 
		random intercept study_years /subject=idno; 
		weight sw;
	run;

	data T3_&ap._&domain.; 
		set &ap._&domain.; 
		where effect in ("&ap." "&ap.*study_years") ;
		domain="&domain.";
		ap="&ap.";
		est=round(estimate, 0.001) || ' ('||round(lower, 0.001) || ', ' || round(upper, 0.001) || ')'; 
		keep domain ap effect est;
	run; 
%mend model; 
	%model(domain=mem, ap=pm25_iqr); 
	%model(domain=exec, ap=pm25_iqr); 
	%model(domain=atten, ap=pm25_iqr); 
	%model(domain=lang, ap=pm25_iqr); 
	%model(domain=constr, ap=pm25_iqr); 

	%model(domain=mem, ap=no2_iqr); 
	%model(domain=exec, ap=no2_iqr); 
	%model(domain=atten, ap=no2_iqr); 
	%model(domain=lang, ap=no2_iqr); 
	%model(domain=constr, ap=no2_iqr); 

data T3_pm25; set T3_pm25_iqr_mem T3_pm25_iqr_exec T3_pm25_iqr_atten T3_pm25_iqr_lang T3_pm25_iqr_constr; run; 
data T3_no2;  set  T3_no2_iqr_mem  T3_no2_iqr_exec  T3_no2_iqr_atten  T3_no2_iqr_lang  T3_no2_iqr_constr; run; 
	
proc print data=T3_pm25; run; 
proc print data=T3_no2; run; 

/*Supp table 7. exec function - trails b only*; */
		%macro model(domain=, ap=);
		ods output solutionf=&ap._trailsb;
		proc mixed data=test method=ml; 
			class idno edu_4 clinic (ref="3"); 
			model z_tb_time_inv=&ap. study_years ndi_20 agernd_c txgroup clinic male edu_4 practice 
									&ap.*study_years
									agernd_c*study_years									
									/solution cl; 
			random intercept study_years /subject=idno; 
			weight sw;
		run;

		data T3_&ap._trailsb; 
			set &ap._trailsb; 
			where effect in ("&ap." "&ap.*study_years") ;
			domain="trailsb";
			ap="&ap.";
			est=round(estimate, 0.001) || ' ('||round(lower, 0.001) || ', ' || round(upper, 0.001) || ')'; 
			keep domain ap effect est;
		run; 
	%mend model; 
		%model(ap=pm25_iqr); 
		%model(ap=no2_iqr); 

	data T3_pm25; set T3_pm25_iqr_trailsb; run; 
	data T3_no2;  set  T3_no2_iqr_trailsb; run; 
		
	proc print data=T3_pm25; run; 
	proc print data=T3_no2; run; 


/*-----------------------------------------------EXTRA ANALYSES FOR TEXT -----------------------------------------------*/

*maximum # exams and corresponding fu time; 
	proc univariate data=dat; 
	where visitno=2; 
	var total_visit total_years; 
	run; 

	data test; set dat; where total_visit=9; run; 

*mean ap exposure and IQR; 
	proc univariate data=dat; where visitno=2; 
		var pm25hist_20yrave NO2_preds_10YRAVE; 
	run;

*ipw descriptives; 
	proc sql; 
		create table ipw_stat as 
		select idno, visitno, sw,
			exp(sum(log(sw))) as sw_product /*create product of weights by idno*/
				from dat2
		group by idno ;
	quit;  

	proc univariate data=ipw_stat; 
		var sw_product; 
	run;  
	
