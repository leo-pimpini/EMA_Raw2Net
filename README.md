# Raw2Net
Raw2Net is an R pipeline that goes from raw Ecological Momentary Assessment (EMA) data until network analysis estimation (mlVAR, vector autoregressive modeling).
Data (fully anonymized) belongs to a  smartphone EMA study investigating whether manipulating frequecy of monitoring  affects (self)reported unhealthy snacking and craving. 
Participants were randmly assigned to either a low (3 times/day), medium (6 times/day) and high (9 times/day) monitoring condition. The EMA protocol lasted 3 weeks independent of the condition.
The full preprint of the study can be found here: 

Pipeline:
Pre-processing phase (input data = raw data)
1. Read-in and merge raw EMA data
2. EMA data preprocessing 
3. Split data per pp (optional, for addiitonal analyses)

Network analysis phase (input data = Processed_data)

4. Net_estim_EMA_Monitoring

IMPORTANT: fully anonimyzed data (raw + processed) can be downloaded here: 
