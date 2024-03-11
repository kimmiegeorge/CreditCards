DiD Analysis with prior quarter used as control time period and non-suspect firms used as control firms 

CreateSuspectEAMonths.txt 
- creates TreatmentMonths dataset 
- determines suspect quarters and pulls months in suspect quarter months and 3 months prior for these firms 

CreateControlMonths.txt 
- creates ControlMonths dataset
- non suspect firms during treatment events and the 3 months prior to the treatment event 

CreateTreatControlMonths.txt 
- binds together treatment months and control months 

CreateDiDData.txt 
- uses treat and control months to create the full stacked DiD file 
