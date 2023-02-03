## Load required libraries

 
library(lubridate)
library(dbplyr)
library(tidyverse)
library(sf)
library(vmstools)




################################################################################
##
##
##  1. DATA PREPARATION. EFLALO & TACSAT LOAD AND FORMATING               #####
##
##
################################################################################

## Load the EFLALO ( logbook) and TACSAT (VMS) data in specified format


## EFLALO data must  include fishing trips selected using following criteria: 

  ## - Area of Interest: 27.7e to 27.7.k
  ## - Time period: 2017 - 2021
  ## - Gear used: bottom otter trawler gears ( OTB, OT, OTT, PTB )
  ## - Mesh size range: The 2 mesh size categories ('70-99' & '100-119+')

## EFLALO data  must include fishing trips landing information by species captured : 

  ## - Total weight (kg) and value (euro) of landings by species landed 
  ## - The weight (LE_KG) and value (LE_EURO) fields must be present for cod (LE_KG_COD, LE_EURO_COD) and all the other species combined (LE_KG_OTHERS, LE_EURO_OTHERS)
      ## - If these haven't been aggregated in advance, a code is provided to obtain it in line 87 




  ## 1. Load and format EFLALO
  
  load (eflalo_ot_2017_2021.RData )
  
   ## 1.2. Filter EFLALO only for  fishing trips with any COD captured 
  
    # 1.2.1 select the trips id (ft_Ref) with COD landings reported
    
    eflalo_cod_trips = eflalo_ot_2017_2021%>%filter(LE_SPE == 'COD')%>%distinct(FT_REF)
    
    # 1.2.2 USe the list to filter EFLALO for whole trip information with any capture of COD
    
    eflalo_cod = eflalo_ot_2017_2021%>%filter(FT_REF %in% eflalo_cod_trips$FT_REF )
    
  ## 1.3. Format EFLALO with required fields types
    
    
    # 1.3.1 Date formats (choose "lubridate" function ymd () for Year/ month/day or dmy() day/ month/ year based on your system date formats preferences)
    
    eflalo_cod$FT_DDAT =  ymd( eflalo_cod$FT_DDAT , tz = "GMT" )  
    eflalo_cod$FT_LDAT =   ymd(eflalo_cod$FT_LDAT  , tz = "GMT" ) 
    eflalo_cod$LE_CDAT =   ymd(eflalo_cod$LE_CDAT  , tz = "GMT" ) 
    eflalo_cod$FT_DDATIM = ymd_hms( paste ( eflalo_cod$FT_DDAT ,eflalo_cod$FT_DTIME         ) , tz = "GMT"   ) 
    eflalo_cod$FT_LDATIM = ymd_hms( paste ( eflalo_cod$FT_LDAT ,eflalo_cod$FT_LTIME         )  , tz = "GMT"  ) 
    eflalo_cod$year = lubridate::year(eflalo_cod$FT_DDATIM )
    eflalo_cod$month = lubridate::month(eflalo_cod$FT_LDATIM)
    eflalo_cod$quarter = lubridate::quarter(eflalo_cod$FT_LDATIM)
    
    # 1.3.2 Create country identifier field
    
    eflalo_cod$VE_COU = 'GBR'
    
    # 1.3.3 Create a field for the MESH SIZE CATEGORY:  '70-99' or  '100-119+'
    
    eflalo_cod = eflalo_cod%>%mutate(MESH_RANGE = ifelse(LE_MSZ >= 70 & LE_MSZ<100 ,'70-99',  '100-119+'   ) )  
    
    # 1.3.4 (OPTIONAL)  If not done before, aggregate total landings and value by COD and OTHERS species categories
    
    
    
    eflalo_cod =  eflalo_cod%>%
                  mutate(LE_KG_OTHERS   = rowSums( select(., starts_with("LE_KG") , -"LE_KG_COD")    , na.rm = T ) ) %>%        ## rum across species le_kg except LE_KG_COD
                  mutate(LE_EURO_OTHERS = rowSums( select(., starts_with("LE_EURO"), -"LE_EURO_COD") , na.rm = T ) ) %>%        ## rum across species le_euro except LE_EURO_COD
                  select( -starts_with("LE_EURO") , -starts_with( "LE_KG") , "LE_KG_COD","LE_EURO_COD","LE_KG_OTHERS", "LE_EURO_OTHERS"  ) %>%   
                  as.data.frame() 
    
    
 

  ## 2. Load TACSAT data for trips with any cod landing
      ## The TACSAT loaded must be already QC with not VMS point on land or in port , etc. . 
      ## VMS locations identified as fishing already flagged in field SI_STATE

    
  load (tacsat_ot_2017_2021.RData )
  

    # 2.1  Date formats (choose "lubridate" function ymd () for Year/ month/day or dmy() day/ month/ year based on your system date formats preferences)
  
    tacsat$SI_DATE  =  ymd( tacsat$SI_DATE  , tz = "GMT"  )  
    tacsat$SI_DATIM  =   ymd_hms(paste ( tacsat$SI_DATE ,tacsat$SI_TIME ), tz = "GMT" ) 
    
    # 2.1  Create the field FT_REF in common with EFLALO based on the TACSAT field trip id in SI_FT
    
    tacsat['FT_REF'] = tacsat$SI_FT
    
    # 3.1  Convert SI_STATE fishing state value "f" into 1 ( vessel fishing at given VMS location)  and rest values into 0 ( vessel not fishing at given VMS location)
    
    tacsat$SI_STATE[which(tacsat$SI_STATE != "f")] = 0
    tacsat$SI_STATE[which(tacsat$SI_STATE == "f")] = 1



################################################################################
##
##
##  2. VMS & LOGBOOK LINKED ANALYSIS: COUPLE CATCHES TO VMS LOCATIONS      #####
##
##
################################################################################
    
    
    
  ## 1. Separate logbook records with and without associated VMS position records 
    
    ## 1.1. Subset logbook fishing trips with associated VMS records
    
    eflaloM           = subset(eflalo_cod,FT_REF %in% unique(tacsat$FT_REF))
    
    ## 1.2. Subset logbook fishing trips with no associated VMS records
    
    eflaloNM          = subset(eflalo_cod,!FT_REF %in% unique(tacsat$FT_REF))
   
    ## 1.3 Evaluate the outputs of this process
    
    print(paste0('Dimension of eflalo with not associated tacsat records: ',  dim(eflaloNM)   ))
    print(paste0('Dimension of eflalo with associated  tacsat records: ',  dim(eflaloM)[1]   ))
    
    
    
  ## 2. Link Logbook and related VMS locations. Apportion landing values by VMS locations using "splitamongpings" function in VMSTool Package   
    
    ## 2.1 Prepared EFLALO and TACASAT must be in data.frame format
    
    tacsat_df = tacsat%>%as.data.frame()
    eflaloM_df = eflaloM%>%as.data.frame()
    
    ## 2.2 Transfer required fields from EFLALO into TACSAT data frame
    
    tacsat_df$LE_GEAR  = eflaloM_df$LE_GEAR[ match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$LE_MSZ   = eflaloM_df$LE_MSZ [ match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$VE_LEN   = eflaloM_df$VE_LEN [ match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$VE_KW    = eflaloM_df$VE_KW  [ match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$LE_RECT  = eflaloM_df$LE_RECT[ match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$LE_MET   = eflaloM_df$LE_MET[  match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$LE_WIDTH = eflaloM_df$LE_WIDTH[match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$VE_FLT   = eflaloM_df$VE_FLT[  match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$LE_CDAT  = eflaloM_df$LE_CDAT[ match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    tacsat_df$VE_COU   = eflaloM_df$VE_COU[  match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    
    tacsat_df$MESH_RANGE   = eflaloM_df$MESH_RANGE [  match(tacsat_df$FT_REF, eflaloM_df$FT_REF)]
    
    
    
     
    
    ## 2.2 Run the function splitamongpings 
    
    
    tacsatEflalo  = vmstools::splitAmongPings ( tacsat=tacsat_df, 
                                                eflalo=eflaloM_df, 
                                                variable="all",
                                                level="day",
                                                conserve=T )
    
    
    ## 2.3 Compare tacsataEflalo apportioned total weight results with EFLALOM totals weight
      ## Analyse if the weight in logbooks has been transfered as expected into the VMS locations in tacsatEflalo dataframe
    
 
    tacsatEflalo%>%summarise(kg_cod = sum(LE_KG_COD, na.rm = T), kg_others = sum(LE_KG_OTHERS, na.rm = T)  )
    eflaloM_df%>%summarise(kg_cod = sum(LE_KG_COD, na.rm = T), kg_others = sum(LE_KG_OTHERS, na.rm = T)  )
    
    
    
    ################################################################################
    ##
    ##
    ##  3. OUTPUT AGGREGATION & FORMATTING                                     #####
    ##
    ##
    ################################################################################
    
    
    ## This section transforms the raw VMS locations with logbooks indicators (tacsatEflalo) into an aggregated dataset with the following structure: 
    
      ## Categories for aggregation: 
    
        ## Year: Temporal resolution of the final output . 
        ## Quarter: Temporal resolution of the final output. 
    
        ## C-Square 0.05: Represents the spatial resolution of the aggregated output. The resolution will enable high resolution analysis but preserving the anonymity of individual vessels activity . 
        
        ## Mesh size range: The 2 mesh size categories ('70-99' & '100-119+') enable the spatio-temporal fishing activity trend analysis and comparison of the activity of the 2 fleet segments . 
        ## Country: GBR  or EU ( or individual member states ) . To analyse the fishing activity by fleet country . 
    
      ## Fishing activity indicators by category:
    
        ## effort: Total fishing effort (in hours)  by aggregation category
        ## effort*kwh: Total fishing effort (in hours) * engine power (kwh)  by aggregation category
        ## kg_cod/others: Total cod/others weight (kg) captured by above category
        ## val_cod/others: Total cod/others 1st sales value (euro) by above category
        ## avg_len:  Average length of the vessels by aggregation category
        ## avg_sp:  Average fishing speeds of the vessels by aggregation category
    
        
    ##  3.1 Prepare the data prior aggregation
    
      ## 3.1.1 Add the field with the id of the C-Square covering the area of each VMS location 
      
      tacsatEflalo$csquare   <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
      
      ## 3.1.2 Add the field with the quarter of the year for  aggregation 
      
      tacsatEflalo$si_year_q = lubridate::quarter(tacsatEflalo$SI_DATE)
      
      ## 3.1.3 Convert the field nams to lower case to facilitate further analysis coding 
      
      names(tacsatEflalo) = tolower(names(tacsatEflalo))
    
    
    ##  3.2 Aggregate the data by defined categories 
    
    tacsat_eflalo_aggregated =  tacsatEflalo%>%
                                group_by(si_year, si_year_q,csquare,  mesh_range,ve_cou  )%>%
                                summarise(
                                          avg_sp = mean(si_sp), 
                                          #avg_len = mean(ve_len),
                                          effort_h = sum(intv), 
                                          effort_kwh = sum(intv*ve_kw),
                                          kg_cod = sum(le_kg_cod, na.rm = T), 
                                          kg_others = sum(le_kg_others, na.rm = T), 
                                          val_cod = sum(le_euro_cod, na.rm = T), 
                                          val_others = sum(le_euro_others, na.rm = T), 
                                 ) %>%ungroup()

      ## 3.3 Save the final outputs in a file to 
      
       country = tacsat_eflalo_aggregated%>%distinct(ve_cou)
       save(tacsat_eflalo_aggregated, file = paste0("tacsat_eflalo_aggregated", country, ".RData") ) 

    
    
