# Celtic Sea cod trawling activity analysis 2023

Repository with the  code required for the analysis of fishing activity by trawler vessels capturing cod in the Celtic Sea area (27.7.e - 27.7.k).

RECOMMENDED WORKFLOW: 

https://github.com/CefasRepRes/celtic_sea_cod_trawling_analysis_2023/blob/main/R/analysis_workflow.R

OUTPUTS OF THE PROCESSING WORKFLOW: 

   - TABLE 1: Logbook effort and capture indicators aggregated by year, quarter, ices rectangle and mesh range. 
   - TABLE 2: VMS linked to logbook effort and capture indicators aggregated by year, quarter, C-SQaure 0.05 and mesh range. 

> The analysis have been structured in the following sections and processes:


 
##  1. DATA PREPARATION. EFLALO & TACSAT LOAD AND FORMATING     

 

### Load the EFLALO ( logbook) and TACSAT (VMS) data in specified format


#### EFLALO data must  include fishing trips selected using following criteria: 

   - **Area of Interest**: 27.7e to 27.7.
   - **Time period**: 2017 - 2021
   - **Gear**: bottom otter trawler gears ( OTB, OT, OTT, PTB )
   - **Mesh size**: 70 mm to maximum mesh sizes used ( 70 >= ) 

#### EFLALO data  must include fishing trips landing information by species captured : 

   - **Total weight (kg) and value (euro)** of landings by species landed 
   - The weight (LE_KG) and value (LE_EURO) fields must be present for cod (LE_KG_COD, LE_EURO_COD), nephrops (NEP), angler fish (ANF), hake (HKE), lemon sole (LEZ) , haddock (HAD) , whiting (WHI)  and all the other species combined (LE_KG_OTHERS, LE_EURO_OTHERS).
   - If these haven't been aggregated in advance, it is provided a code to obtain it 
   
   The weight (LE_KG) and value (LE_EURO) fields must be present for cod (LE_KG_COD, LE_EURO_COD), nephrops (NEP), angler fish (ANF), hake (HKE),
       ## lemon sole (LEZ) , haddock (HAD) , whiting (WHI)  and all the other species combined (LE_KG_OTHERS, LE_EURO_OTHERS).


#### TACSAT data must include the VMS for fishing trips selected using following criteria:
  
   -  **TACSAT data related to EFLALO trips with any cod captured**: The TACSAT data must be extracted for the fishign trips wqith any cod landing already extracted for EFLALO. This will ensure teh TACSAT obtained follow same criteria than the fishing trips in EFLALO. 
   
  
  
####  1. Load and format EFLALO & TACSAT: 

   - Create in EFLALO the mesh size categories field: '70-99' and '100 >'
   - Select EFLALO trips with any COD capture 
   - Standardize the dates formats both in EFLALO and TACSAT ( benefit analysis stage ) 
 
##  2. VMS & LOGBOOK LINKED ANALYSIS: COUPLE CATCHES TO VMS LOCATIONS  
 
    
    
    
  ####  1. Separate logbook records with and without associated VMS position records 
  
  
  ####  2. Link Logbook and relted VMS locations. Apportion landing values by VMS locations using "splitamongpings" function in VMSTool Package 
  
  
    
##  3. OUTPUT AGGREGATION & FORMATTING     

 ### TABLE 1
    
 ### Table 1 output aggregates the  logbook datatset (eflalo_ot_2017_2021) with the following structure: 
    
#### Categories for aggregation  
    
   - **Year**: Temporal resolution of the final output 
   - **Quarter**: Temporal resolution of the final output 
   - **ICES Rectangle**: The spatial resolution of the aggregated output uses the reported ICES statistical rectangle in the e-logbooks.
   - **Mesh size range**: The 2 mesh size categories ('70-99' & '100-119+') enable the spatio-temporal fishing activity trend analysis and comparison of the activity of the 2 fleet segments 
   - **Country**: GBR  or EU ( or individual member states ) . To analyse the fishing activity by fleet country . 
    
#### Fishing activity indicators by category:
    
   - **effort**: Total fishing effort (in days)  by aggregation category
   - **effort*kwh**: Total fishing effort (in days) * engine power (kwh)  by aggregation category
   - **kg_cod/others**: Total cod/others weight (kg) captured by above category
   - **val_cod/others**: Total cod/others 1st sales value (euro) by above category
   - **avg_len**:  Average length of the vessels by aggregation category
   

        
 ####   Aggregate the data by defined categories and create the TABLE 1 final output
    
    
 ### TABLE 2
    
 ### Table 2 output aggregates the  VMS linked to logbooks  (tacsatEflalo) with the following structure: 
    
#### Categories for aggregation  
    
   - **Year**: Temporal resolution of the final output 
   - **Quarter**: Temporal resolution of the final output 
   - **C-Square 0.05**: Represents the spatial resolution of the aggregated output. The resolution will enable high resolution analysis but preserving the anonymity of individual vessels activity 
   - **Mesh size range**: The 2 mesh size categories ('70-99' & '100-119+') enable the spatio-temporal fishing activity trend analysis and comparison of the activity of the 2 fleet segments 
   - **Country**: GBR  or EU ( or individual member states ) . To analyse the fishing activity by fleet country . 
    
#### Fishing activity indicators by category:
    
   - **effort**: Total fishing effort (in hours)  by aggregation category
   - **effort*kwh**: Total fishing effort (in hours) * engine power (kwh)  by aggregation category
   - **kg_cod/others**: Total cod/others weight (kg) captured by above category
   - **val_cod/others**: Total cod/others 1st sales value (euro) by above category
   - **avg_len**:  Average length of the vessels by aggregation category
   - **avg_sp**:  Average fishing speeds of the vessels by aggregation category

        
 ####  Aggregate the data by defined categories and create the TABLE 2 final output
    
