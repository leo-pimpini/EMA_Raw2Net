# EMA_Raw2Net
'EMA_Raw2Net' is a serie of four R scripts that span from raw Ecological Momentary Assessment (EMA) data until network analysis using mlVAR (multi-level Vector Autoregressive Modeling).
Specifically this pipeline is tailored for a dataset (fully anonymized) investigating whether manipulating frequecy of monitoring affects daily unhealthy snacking and craving. 
Participants were randmly assigned to either a low (3 times/day), medium (6 times/day) and high (9 times/day) monitoring condition. Assessment was done using smartphone-based EMA.
The duration of the EMA protocol was 3 weeks independent of the condition. For an overview, see the preprint here: 
Each code is commented to ensure a smoother use and, hopefully, step-by-step understanding.

Pipeline:
Pre-processing phase (input data = raw data)
1. Read-in and merge raw EMA data
2. EMA data preprocessing 
3. Split dataset per pp (optional, for addiitonal analyses)

4. GLMM (general linear mized effects model)
5. Net_estim_EMA_Monitoring (Network analysis; input data = Processed_data)

IMPORTANT: data (raw + processed) can be downloaded here: https://osf.io/fsqpv/ 
