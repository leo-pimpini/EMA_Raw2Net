# EMA_Raw2Net

‘EMA_Raw2Net’ is a series of R scripts that take raw Ecological Momentary Assessment (EMA) data, restructure and preprocess it, and then apply linear models (using zero-inflated models when necessary) and network analysis (mlVAR) to the dataset. Specifically this pipeline is adapted for a dataset (fully anonymized) and study that investigates whether frequency of monitoring affects daily life (unhealthy) snacking and craving. 
Participants were randomly assigned to either a low (3 times/day), medium (6 times/day) and high (9 times/day) monitoring BS condition. Assessment was done using smartphone-based EMA.
The duration of the EMA-protocol was 21 days independent of the condition. A minimum compliance thresholds of 75% per week was set in each condition.  
Each code is commented to ensure a smoother use, reproducibility and (hopefully) understanding.

PIPELINE:
Pre-processing phase (input data = raw data)
1. Read-in and merge raw EMA data
2. EMA data preprocessing 
3. Split dataset per pp (optional, alternative analyses)
4. GLMM (generalized linear mixed-effects model; one scrpt per variable). Input data = Processed data -> EMA_Merged_AllFreq
5. Net_estim_EMA_Monitoring (Network analysis with mlVAR). Input data = Processed_data -> Low_freq / Medium_Freq / High_Freq (run one subset at a time)

IMPORTANT: data (raw and processed) may be downloaded here: https://osf.io/fsqpv/ 

Edit working path before running each script. 
