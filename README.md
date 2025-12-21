# EMA_Raw2Net
'EMA_Raw2Net' is a serie of R scripts that span from raw Ecological Momentary Assessment (EMA) data until network analysis using mlVAR (multi-level Vector Autoregressive Modeling).
Specifically this pipeline is tailored for a dataset (fully anonymized) investigating whether manipulating frequency of monitoring affects daily unhealthy snacking and craving. 
Participants were randmly assigned to either a low (3 times/day), medium (6 times/day) and high (9 times/day) monitoring condition. Assessment was done using smartphone-based EMA.
The duration of the EMA protocol was 3 weeks independent of the condition.
Each code is commented to ensure a smoother use and, hopefully, step-by-step understanding.

PIPELINE:
Pre-processing phase (input data = raw data)
1. Read-in and merge raw EMA data
2. EMA data preprocessing 
3. Split dataset per pp (optional, alternative analyses)

4. GLMM (generalized linear mixed-effects model; one scrpt per variable). Input data = Processed data >> EMA_Merged_AllFreq
5. Net_estim_EMA_Monitoring (Network analysis with mlVAR). Input data = Processed_data >> Low_freq; Medium_Freq; High_Freq

IMPORTANT: data (raw and processed) can be downloaded here: https://osf.io/fsqpv/ 

Change input data path before running the scripts. 
