---Aggregate Totals

SELECT YEAR, MONTH,
COUNT (CASE WHEN FBI_CD = '01A' THEN 1 ELSE NULL END) "01A",
COUNT (CASE WHEN FBI_CD = '02' THEN 1 ELSE NULL END) "02",
COUNT (CASE WHEN FBI_CD = '03' THEN 1 ELSE NULL END) "03",
COUNT (CASE WHEN FBI_CD = '04A' THEN 1 ELSE NULL END) "04A",
COUNT (CASE WHEN FBI_CD = '04B' THEN 1 ELSE NULL END) "04B",
COUNT (CASE WHEN FBI_CD = '05' THEN 1 ELSE NULL END) "05",
COUNT (CASE WHEN FBI_CD = '06' THEN 1 ELSE NULL END) "06",
COUNT (CASE WHEN FBI_CD = '07' THEN 1 ELSE NULL END) "07",
COUNT (CASE WHEN FBI_CD = '09' THEN 1 ELSE NULL END) "09"
  FROM 

-- Because each reporting category has its own idiosyncratic reporting rules, it became 
-- sensible to partition querying into multiple components that are combined via UNION 
-- statements. The SELECT statement above uses the final tabular byproduct of these combined 
-- queries - organizing results crosstabularly. 

-- The query below identifies records for murder/non-negligent homicide.
-- This category is addressed separately primarily because we rely on the Hom Desk
-- table.

    (SELECT hom.ID, hom.RD_NO RD, 1 NO_ID, '01A' FBI_CD, TO_CHAR(hom.INJURY_DATE,'MM') MONTH,
            TO_CHAR(hom.INJURY_DATE,'YYYY') YEAR
        FROM CHRIS_DWH.HOM_HOMICIDES_DW_REDIST hom
        WHERE substr(hom.HOMICIDE_NO,1,1) Not in ('I','J')
        AND hom.INJURY_DATE between to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                                AND to_date('12/31/2016 23:59:59', 'MM/DD/YYYY HH24:MI:SS')
        AND (hom.ACTIVE_I = 'Y') 
        AND to_number(hom.CURR_DISTRICT) between 1 and 25
        
-- The query below identifies records for criminal sexual assault and aggravated assault/battery
-- These two categories are addressed separately because, besides murder/non-negligent homicide, 
-- they are the only categories reported as # of VICTIMS as opposed to # of INCIDENTS.  

    UNION
      SELECT cr.ID, cr.RD, vic.NO_ID, vic.VFBI_CD FBI_CD, cr.MONTH, cr.YEAR
      FROM CHRIS_DWH.CRIMES_ALLV cr 
      INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
      INNER JOIN CHRIS_DWH.VICTIMS_ALLV vic ON cr.ID = vic.CASE_ID
      WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                             AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
      AND iu.FBI_CD IN ('02','04A','04B') 
      AND iu.CD NOT IN ('0487')
      AND vic.VFBI_CD IN ('02','04A','04B')
      AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
      AND cr.RD IS NOT NULL
      AND to_number(cr.DISTRICT) BETWEEN 1 and 25

-- The query below identifies records for robbery, motor vehicle theft, arson, and burglaries where location
-- code is not a motor vehicle (these cases should be reported as larceny/theft). The four categories
-- are placed together because, for each, we report # of incidents. 

    UNION
      SELECT cr.ID, cr.RD, 1 NO_ID, iu.FBI_CD, cr.MONTH, cr.YEAR
      FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
      INNER JOIN CHRIS_DWH.FBI_CODES fbi ON iu.FBI_CD = fbi.CD
      WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                             AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
      AND (iu.FBI_CD IN ('03','07','09') OR (iu.FBI_CD IN ('05') AND cr.LOCATION NOT IN 
      ('309', '262', '126', '259')))
      AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
      AND cr.RD IS NOT NULL
      AND to_number(cr.DISTRICT) BETWEEN 1 and 25
      
-- For UCR reporting, in instances when a motor vehicle theft occurs in conjunction with larceny/theft, 
-- the former is reported as opposed to the latter, even though larceny/theft is technically higher in 
-- the UCR hierarchy. The query below flags such cases, and artificially characterizes the case as a 
-- motor vehicle theft for UCR reporting purposes. 
      
    UNION
      SELECT DISTINCT ID, RD, 1 NO_ID, '07' FBI_CD, MONTH, YEAR
      FROM (SELECT cr.ID, cr.RD, cr.MONTH, cr.YEAR
          FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          WHERE (cr.DATEOCC) BETWEEN to_date ('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                                  AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
          AND (iu.FBI_CD IN ('06') OR ((iu.FBI_CD = '05') AND (cr.LOCATION IN ('309', '262', '126', '259')))) 
          AND allcr.IUCR_CODE_CD IN ('0910','0915','0917','0918','0920','0925','0927',
            '0928','0930','0935','0937','0938')
          AND iu.CD NOT IN ('0840','0841','0842','0843')
          AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
          AND cr.RD IS NOT NULL
          AND to_number(cr.DISTRICT) BETWEEN 1 and 25) 
          
-- The query below identifies all remaining reportable larceny/theft cases, excluding cases identified
-- in the query immediately above. In essence the goal of the query is to tally all cases carried in the 
-- CPD system as larceny/theft, excluding ones that we artificially changed to motor vehicle theft above. 
          
    UNION
      SELECT DISTINCT cr.ID, cr.RD, 1 NO_ID, '06' FBI_CD, cr.MONTH, cr.YEAR
      FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
      WHERE (cr.ID) NOT IN 
        (SELECT cr.ID
          FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          WHERE (cr.DATEOCC) BETWEEN to_date ('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')                                     
                                  AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
          AND (iu.FBI_CD IN ('06') OR ((iu.FBI_CD = '05') AND (cr.LOCATION IN ('309', '262', '126', '259'))))
          AND allcr.IUCR_CODE_CD IN ('0910','0915','0917','0918','0920','0925','0927',
            '0928','0930','0935','0937','0938')
          AND iu.CD NOT IN ('0840','0841','0842','0843')
          AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
          AND cr.RD IS NOT NULL
          AND to_number(cr.DISTRICT) BETWEEN 1 and 25) 
      AND (cr.DATEOCC) BETWEEN to_date ('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                            AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
      AND (iu.FBI_CD IN ('06') OR ((iu.FBI_CD = '05') AND (cr.LOCATION IN ('309', '262', '126', '259'))))
      AND iu.CD NOT IN ('0840','0841','0842','0843')
      AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
      AND cr.RD IS NOT NULL
      AND to_number(cr.DISTRICT) BETWEEN 1 and 25   

-- Arson is an exception to the hierarchy rule. For arson, we are to report every incident, regardless
-- whether the arson event occurs in conjunction with a crime that is higher in the UCR hierarchy. 
-- In a query above, we identified cases where the most serious violation is arson. Here, we identify
-- cases where, because the arson occurred in conjunction with a more serious offense, the arson was masked. 
-- Note: CPD policy requires Dept members to assign a unique RD to arson cases. But, a handful of arson cases
-- still get buried due to hierarchy. 
      
    UNION
      SELECT DISTINCT ID, RD, 1 NO_ID, '09' FBI_CD, MONTH, YEAR
      FROM (SELECT cr.ID, cr.RD, cr.MONTH, cr.YEAR
          FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                                 AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
          AND iu.FBI_CD IN ('01A','02','03','04A','04B','05','06','07') 
          AND allcr.IUCR_CODE_CD IN ('1010','1020','1025','1090')
          AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
          AND cr.RD IS NOT NULL
          AND to_number(cr.DISTRICT) BETWEEN 1 and 25)

-- UCR guidelines require that cases classified under UCR 1310 - Criminal Damage to Property and 1320 -\
-- Criminal Damage to Vehicle, be reported as arson if the damage was caused by fire / the property was burned.  
-- The query below identifies these cases.  
          
    UNION
      SELECT DISTINCT ID, RD, 1 NO_ID, '09' FBI_CD, MONTH, YEAR
      FROM (SELECT cr.ID, cr.RD, cr.MONTH, cr.YEAR
        FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          INNER JOIN MV_OWNER.INV_PROPERTIES_MV prop ON cr.ID = prop.CASE_ID
        WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                               AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
        AND iu.FBI_CD NOT IN ('09') 
        AND allcr.IUCR_CODE_CD IN ('1310', '1320')
        AND prop.BURNED_I = 'Y' 
        AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
        AND cr.RD IS NOT NULL
        AND to_number(cr.DISTRICT) BETWEEN 1 and 25))
  GROUP BY YEAR, MONTH
  ORDER BY YEAR, MONTH
  ;

  
--Records

SELECT hom.ID, hom.RD_NO RD, 1 NO_ID, '01A' FBI_CD, TO_CHAR(hom.INJURY_DATE,'MM') MONTH,
      TO_CHAR(hom.INJURY_DATE,'YYYY') YEAR
      FROM CHRIS_DWH.HOM_HOMICIDES_DW_REDIST hom
      WHERE substr(hom.HOMICIDE_NO,1,1) Not in ('I','J')
      AND hom.INJURY_DATE between to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                              AND to_date('12/31/2016 23:59:59', 'MM/DD/YYYY HH24:MI:SS')
      AND (hom.ACTIVE_I = 'Y') 
      AND to_number(hom.CURR_DISTRICT) between 1 and 25
UNION
  SELECT cr.ID, cr.RD, vic.NO_ID, vic.VFBI_CD FBI_CD, cr.MONTH, cr.YEAR
      FROM CHRIS_DWH.CRIMES_ALLV cr 
      INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
      INNER JOIN CHRIS_DWH.VICTIMS_ALLV vic ON cr.ID = vic.CASE_ID
      WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                             AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
      AND iu.FBI_CD IN ('02','04A','04B') 
      AND iu.CD NOT IN ('0487')
      AND vic.VFBI_CD IN ('02','04A','04B')
      AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
      AND cr.RD IS NOT NULL
      AND to_number(cr.DISTRICT) BETWEEN 1 and 25
UNION
  SELECT cr.ID, cr.RD, 1 NO_ID, iu.FBI_CD, cr.MONTH, cr.YEAR
      FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
      INNER JOIN CHRIS_DWH.FBI_CODES fbi ON iu.FBI_CD = fbi.CD
      WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                             AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
      AND (iu.FBI_CD IN ('03','07','09') OR (iu.FBI_CD IN ('05') AND cr.LOCATION NOT IN 
      ('309', '262', '126', '259')))
      AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
      AND cr.RD IS NOT NULL
      AND to_number(cr.DISTRICT) BETWEEN 1 and 25
UNION
  SELECT DISTINCT ID, RD, 1 NO_ID, '07' FBI_CD, MONTH, YEAR
      FROM (SELECT cr.ID, cr.RD, cr.MONTH, cr.YEAR
          FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          WHERE (cr.DATEOCC) BETWEEN to_date ('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                                  AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
          AND (iu.FBI_CD IN ('06') OR ((iu.FBI_CD = '05') AND (cr.LOCATION IN ('309', '262', '126', '259')))) 
          AND allcr.IUCR_CODE_CD IN ('0910','0915','0917','0918','0920','0925','0927',
            '0928','0930','0935','0937','0938')
          AND iu.CD NOT IN ('0840','0841','0842','0843')
          AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
          AND cr.RD IS NOT NULL
          AND to_number(cr.DISTRICT) BETWEEN 1 and 25) 
UNION
  SELECT DISTINCT cr.ID, cr.RD, 1 NO_ID, '06' FBI_CD, cr.MONTH, cr.YEAR
      FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
      WHERE (cr.ID) NOT IN 
        (SELECT cr.ID
          FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          WHERE (cr.DATEOCC) BETWEEN to_date ('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                                  AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
          AND (iu.FBI_CD IN ('06') OR ((iu.FBI_CD = '05') AND (cr.LOCATION IN ('309', '262', '126', '259'))))
          AND allcr.IUCR_CODE_CD IN ('0910','0915','0917','0918','0920','0925','0927',
            '0928','0930','0935','0937','0938')
          AND iu.CD NOT IN ('0840','0841','0842','0843')
          AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
          AND cr.RD IS NOT NULL
          AND to_number(cr.DISTRICT) BETWEEN 1 and 25) 
      AND (cr.DATEOCC) BETWEEN to_date ('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                            AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
      AND (iu.FBI_CD IN ('06') OR ((iu.FBI_CD = '05') AND (cr.LOCATION IN ('309', '262', '126', '259'))))
      AND iu.CD NOT IN ('0840','0841','0842','0843')
      AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
      AND cr.RD IS NOT NULL
      AND to_number(cr.DISTRICT) BETWEEN 1 and 25   
UNION
  SELECT DISTINCT ID, RD, 1 NO_ID, '09' FBI_CD, MONTH, YEAR
      FROM (SELECT cr.ID, cr.RD, cr.MONTH, cr.YEAR
          FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                                 AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
          AND iu.FBI_CD IN ('01A','02','03','04A','04B','05','06','07') 
          AND allcr.IUCR_CODE_CD IN ('1010','1020','1025','1090')
          AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
          AND cr.RD IS NOT NULL
          AND to_number(cr.DISTRICT) BETWEEN 1 and 25)
UNION
  SELECT DISTINCT ID, RD, 1 NO_ID, '09' FBI_CD, MONTH, YEAR
      FROM (SELECT cr.ID, cr.RD, cr.MONTH, cr.YEAR
        FROM CHRIS_DWH.CRIMES_ALLV cr INNER JOIN CHRIS_DWH.IUCR_CODES iu ON cr.CURR_IUCR = iu.CD
          INNER JOIN MV_OWNER.INV_IUCR_CODES_MV allcr ON cr.ID = allcr.CASE_ID
          INNER JOIN MV_OWNER.INV_PROPERTIES_MV prop ON cr.ID = prop.CASE_ID
        WHERE (cr.DATEOCC) BETWEEN to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS')
                               AND to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
        AND iu.FBI_CD NOT IN ('09') 
        AND allcr.IUCR_CODE_CD IN ('1310', '1320')
        AND prop.BURNED_I = 'Y' 
        AND substr(cr.STATUS,1,1) NOT IN ('2','7') 
        AND cr.RD IS NOT NULL
        AND to_number(cr.DISTRICT) BETWEEN 1 and 25) 
;