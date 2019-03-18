# MetaAnalysisJaegerEngelmannVasishth2017
Code and data to accompany the article by 


    Jaeger, Engelmann, Vasishth  (2017). 
	Similarity-based interference in sentence comprehension: 
	Literature review and Bayesian meta-analysis. 
	Journal of Memory and Language. doi:10.1016/j.jml.2017.01.004


	@article{JaegerEngelmannVasishth2017,
	  Author = {J{\"a}ger, Lena A. and Engelmann, Felix and Vasishth, Shravan},
	  doi = {10.1016/j.jml.2017.01.004},
	  Title = {Similarity-based interference in sentence comprehension: {Literature review and Bayesian meta-analysis}},
	  abstract = {We report a comprehensive review of the published reading studies on retrieval interference in reflexive-/reciprocal-antecedent and subject-verb dependencies. We also provide a quantitative random-effects meta-analysis of self-paced and eyetracking reading studies. We show that the empirical evidence is only partly consistent with cue-based retrieval as implemented in the ACT-R-based model of sentence processing by Lewis \& Vasishth 2005 (LV05) and that there are important differences between the reviewed dependency types. In non-agreement subject-verb dependencies, there is evidence for inhibitory interference in configurations where the correct dependent fully matches the retrieval cues. This is consistent with the LV05 cue-based retrieval account. By contrast, in subject-verb agreement as well as in reflexive-/reciprocal-antecedent dependencies, no evidence for interference is found in configurations with a fully cue-matching subject. In configurations with only a partially cue-matching subject or antecedent, the meta-analysis revealed facilitatory interference in subject-verb agreement and inhibitory interference in reflexives/reciprocals. The former is consistent with the LV05 account, but the latter is not. Moreover, the meta-analysis revealed that  (i) interference type (proactive versus retroactive) leads to different effects in the reviewed dependency types; and (ii) the prominence of the distractor has an important impact on the interference effect. 
	In sum, the meta-analysis suggests that the LV05 needs important modifications to account for (i) the unexplained interference patterns and (ii) the differences between the dependency types. More generally, the meta-analysis provides a quantitative empirical basis for comparing the predictions of competing accounts of retrieval processes in sentence comprehension.},
	  Year = {2017},
	  volume = {94},
	  pages = {316-339},
	  journal={Journal of Memory and Language},
	  code = {https://github.com/vasishth/MetaAnalysisJaegerEngelmannVasishth2017}
	  }


# Directory structure 

-data

  contains summary stats used in meta-analysis 

-inst
  
  contains a purl'd output from paper, all code chunks that were in the paper
 
-R 
 
 R functions for paper				

-StanModels
 
 code for random effects meta-analysis
	
-documentation
 
 description of data extraction procedure

-vignettes
 
 Rmd and html files containing (hopefully!) reproducible code


# SessionInfo

If something doesn't work, please check that there isn't any problem with version differences in packages. 


	R version 3.3.2 (2016-10-31)
	Platform: x86_64-apple-darwin13.4.0 (64-bit)
	Running under: macOS Sierra 10.12

	locale:
	[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

	attached base packages:
	[1] grid      parallel  stats     graphics  grDevices utils     datasets 
	[8] methods   base     

	other attached packages:
	[1] xtable_1.8-2       rjags_4-6          coda_0.18-1       
	[4] dplyr_0.4.3        rstan_2.14.1       StanHeaders_2.14.0
	[7] ggplot2_2.2.0     

	loaded via a namespace (and not attached):
	 [1] Rcpp_0.12.8      knitr_1.15.1     magrittr_1.5     munsell_0.4.3   
	 [5] colorspace_1.2-6 lattice_0.20-34  R6_2.1.2         plyr_1.8.3      
	 [9] tools_3.3.2      gtable_0.2.0     DBI_0.4-1        lazyeval_0.2.0  	
	[13] assertthat_0.1   digest_0.6.9     tibble_1.2       gridExtra_2.2.1 
	[17] codetools_0.2-15 inline_0.3.14    labeling_0.3     scales_0.4.1    
	[21] stats4_3.3.2 