# Celtic Sea cod trawling activity analysis 2023

Repository for code required for the analysis of fishing activity by trawler vessels  capturing cod in the Celtic Sea area (27.7.e - 27.7.k).

> The analysis have been structured in the following sections and processes:


 
##  1. DATA PREPARATION. EFLALO & TACSAT LOAD AND FORMATING     

 

### Load the EFLALO ( logbook) and TACSAT (VMS) data in specified format


#### EFLALO data must  include fishing trips selected using following criteria: 

   - Area of Interest: 27.7e to 27.7.
   - Time period: 2017 - 2021
   - Gear used: bottom otter trawler gears ( OTB, OT, OTT, PTB )
   - Mesh size: 70 - 119 and over

#### EFLALO data  must include fishing trips landing information by species captured : 

   - Total weight (kg) and value (euro) of landings by species landed 
   - The weight (LE_KG) and value (LE_EURO) fields must be present for cod (LE_KG_COD, LE_EURO_COD) and all the other species combined (LE_KG_OTHERS, LE_EURO_OTHERS)
   - If these haven't been aggregated in advance, it is provided a code to obtain it in line 87 




####  1. Load and format EFLALO
  
####  2. Load TACSAT data for trips with any cod landing
  
  
  
 
##  2. VMS & LOGBOOK LINKED ANALYSIS: COUPLE CATCHES TO VMS LOCATIONS  
 
    
    
    
  ####  1. Separate logbook records with and without associated VMS position records 
  
  
  ####  2. Link Logbook and relted VMS locations. Apportion landing values by VMS locations using "splitamongpings" function in VMSTool Package 
  
  
    
##  3. OUTPUT AGGREGATION & FORMATTING                       
    
    
This section transforms the raw VMS locations with logbooks indicators (tacsatEflalo) into an aggregated dataset with the following structure: 
    
#### Categories for aggregation: 
    
        - Year: Temporal resolution of the final output . 
        - Quarter: Temporal resolution of the final output. 
    
        - C-Square 0.05: Represents the spatial resolution of the aggregated output. The resolution will enable high resolution analysis but preserving the anonymity of individual vessels activity . 
        
        - Mesh size range: The 2 mesh size categories ('70-99' & '100-119+') enable the spatio-temporal fishing activity trend analysis and comparison of the activity of the 2 fleet segments . 
        - Country: GBR  or EU ( or individual member states ) . To analyse the fishing activity by fleet country . 
    
#### Fishing activity indicators by category:
    
        - effort: Total fishing effort (in hours)  by aggregation category
        - effort*kwh: Total fishing effort (in hours) * engine power (kwh)  by aggregation category
        - kg_cod/others: Total cod/others weight (kg) captured by above category
        - val_cod/others: Total cod/others 1st sales value (euro) by above category
        - avg_len:  Average length of the vessels by aggregation category
        - avg_sp:  Average fishing speeds of the vessels by aggregation category
        
        
 #### 3.2 Aggregate the data by defined categories and create the final output
    
