---
title: "Parent Survey Reunification Analysis Redux"
output:
  html_document:
    theme: cerulean
    toc: yes
  pdf_document:
    toc: yes
  word_document: default
---


# Overview

Our overall goal in this analysis is to examine the timing of permanency outcomes for the children of parents who were surveyed in a comprehensive survey of child-welfare involved parents in Washington State in 2008. Although the survey addressed parents who were served in-home and out-of-home, the analyses here will be limited to those parents who had at least one child placed in out-of-home care. This steps taken here will build toward a competing-risk event-history model.  

# Getting the Data

## Accessing the Original Survey Data

In order to solve a gap in institutional knowledge concerning the precise exclusion criteria used by prior POC staff, all available data files from the original SACWIS matches were exported to the POC2 SQL Server. The source files are in SPSS format and were exported using the SPSS database export utility over an ODBC connection. The source files are available to authorized POC analysts at the following path: 

`\\poc2\Practice Model\PARENT SURVEY I\Reunification Analysis_Parent Survey\Original Data Files\Matt`

All files with at least a `dcid` (parent id for the survey) and a `Person ID` (child id from SACWIS) were imported to the SQL server in `dbCoreAdministrativeTables` in a schema names `survey`. These files were then unioned into a single table `ps_tbl_children` using the following SQL script: 


        if object_id('dbo.ps_tbl_children') is not null 
          drop table dbo.ps_tbl_children
        
        select
          dcid
          ,id_prsn_child
          ,min(placement_date) placement_date 
        into dbo.ps_tbl_children 
        from(
          select 
        		dcid
        		,person_id id_prsn_child
        		,placement_date
        	from 
        		dbo.ps_open_matt_match_back
        	union
        	select 
        		dcid
        		,id_prsn id_prsn_child
        		,coalesce(placement_date, place_start) placement_date
        	from 
        		dbo.ps_open_mtt_match_back_famlink_missed_36			
        	union
        	select 
        		study_id dcid
        		,person_id id_prsn_child
        		,placement_date
        	from 
        		dbo.ps_Original_Openwithplacements_BA_added	
        	union
        	select 
        		dcid
        		,id_prsn id_prsn_child
        		,place_start placement_date
        	from 
        		dbo.ps_still_open_match_back_from_famlink) tmp
        group by 
        	dcid
        	,id_prsn_child
        ) tmp


This provides us with a cross-walk between the `dcid` and `Person ID` (hereafter `id_prsn_child`) for further use in our analysis.

We next load the data from the parent survey itself. These data are available in another SPSS file located at the following path:  

`\\poc2\Practice Model\PARENT SURVEY I\Parent Data\Original Data\POC FINAL DATA.sav`

As with the files described above, we export the data from SPSS to SQL Server on POC2 using the SPSS database utility. The data are stored in a table named `ps_tbl_parent_data`. We can join the two tables together using a SQL script. Here, however, we will do this directly in R using the `RODBC` package.


```r
#assign a connection object
cn <- odbcConnect("POC")
```

```
## Warning in odbcDriverConnect("DSN=POC"): [RODBC] ERROR: state IM002, code
## 0, message [Microsoft][ODBC Driver Manager] Data source name not found and
## no default driver specified
```

```
## Warning in odbcDriverConnect("DSN=POC"): ODBC connection failed
```

```r
#use the connection object to send a query to our database
dat1 <- sqlQuery(cn, 
                "select 
                  *
                  ,id_prsn_child
                from ps_tbl_parent_data tpd
                	join ps_tbl_children tc
                	on tpd.dcid=tc.dcid")
```

```
## Error in sqlQuery(cn, "select \n                  *\n                  ,id_prsn_child\n                from ps_tbl_parent_data tpd\n                \tjoin ps_tbl_children tc\n                \ton tpd.dcid=tc.dcid"): first argument is not an open RODBC channel
```

```r
county_xwalk <- 

#count the number of parents
dat1_par <- length(unique(dat1$DCID))
```

```
## Error in unique(dat1$DCID): object 'dat1' not found
```

```r
dat1_par
```

```
## Error in eval(expr, envir, enclos): object 'dat1_par' not found
```

```r
#count the number of children
dat1_chi <- length(unique(dat1$id_prsn_child))
```

```
## Error in unique(dat1$id_prsn_child): object 'dat1' not found
```

```r
dat1_chi
```

```
## Error in eval(expr, envir, enclos): object 'dat1_chi' not found
```










