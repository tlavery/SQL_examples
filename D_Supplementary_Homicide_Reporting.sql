-- SUPPLEMENTAL HOMICIDE REPORTING REQUIREMENTS INCLUDE NUMEROUS IDIOSYNCRASIES THAT NECESSITATE
-- SPECIALIZED SUBQUERIES. 

-- LOGICAL STRUCTURE: 

-- MASTER TABLE (1,2,3 BELOW) DRAWS IN INFO FROM WEAPON TABLE (4 BELOW), SITUATION TABLE (5 BELOW), CIRCUMSTANCE TABLE (6 BELOW). 
-- 4,5,6 NEEDED BECAUSE OF IDIOSYNCRASIES. 

select distinct
     fulldata.MONTH, 
     fulldata.YEAR,
     fulldata.CASE#_TYPE1, 
     fulldata.CASE#_TYPE2, 
     fulldata.V_COUNT,
     fulldata.HOM_TYPE,
     fulldata.SIT_CODE, 
     fulldata.CIRC_CODE, 
     fulldata.CIRC_SUBCODE, 
     fulldata.WEAP_CODE, 
     fulldata.VIC#,
     fulldata.VIC_AGE,
     fulldata.VIC_SEX,
     fulldata.VIC_RACE,
     fulldata.VIC_ETH,
     fulldata.OFF#, 
     fulldata.OFF_AGE,
     fulldata.OFF_SEX,
     fulldata.OFF_RACE,
     fulldata.OFF_ETH,
     fulldata.REL_CODE
     from

-- 1. THE QUERY ABOVE PRODUCES FINAL OUTPUT. THE QUERY REMOVES UNNECESSARY FIELDS AND 
--    IS LIMITED TO REPORTABLE FIELDS, IN ANTICIPATED ORDER. 

(
     select distinct 
     MNTH MONTH, 
     YR YEAR,
     RD_NO CASE#_TYPE1, 
     HOMICIDE_NO CASE#_TYPE2, 
     case when SITUATIONCD in ('B', 'F') then 1 else HOFFID end HOFFID2,   --**In order to achieve the one-to-one relationship between victim and offender in Situation Codes 'B' and 'F', I had to re-code the Homicide Offender ID (hoff.id - HOFFID) as '1' for those offenders since each HOFFID is unique/distinct.
     dense_rank() over(order by HOMICIDE_NO) V_COUNT,
     HOMTYPECD HOM_TYPE,
     SITUATIONCD SIT_CODE, 
     CCODE CIRC_CODE, 
     ' ' CIRC_SUBCODE, 
     case when CCODE = '81' and INJURY_DESCR = 'SHOT' then '12'  --I'm assuming that officers used a handgun in justified police shootings. The weapons table provides the weapon info of the offender,hence I have to determine what the P.O. used based off of the injury description.
     when CCODE = '81' and INJURY_DESCR != 'SHOT' then ' ' else WEAPONCD end WEAP_CODE, --If the subject killed by the P.O. is not 'shot' then we have to find out what weapon was used and manually enter the code. 
     dense_rank() over(partition by RD_NO order by HOMICIDE_NO) VIC#,
     case when CCODE = '81' then OFFAGE else VICAGE end VIC_AGE,
     case when CCODE = '81' then OFFSEX else VICSEX end VIC_SEX,
     case when CCODE = '81' then OFFRACE else VICRACE end VIC_RACE,
     case when CCODE = '81' then OFFETHNICITY else VICETHNICITY end VIC_ETH,
     case when SITUATIONCD in ('B', 'F') then 1 else dense_rank() over(partition by HOMICIDE_NO order by HOFFID asc) end OFF#, --I had to rank each offender in Situation Codes 'B' and 'F' as '1' in order for the DISTINCT function to work; otherwise it would still return multiple rows in homicides (with Situation Codes 'B' and 'F') where there was more than 1 offender.
     case when CCODE != '81' then OFFAGE else VICAGE end OFF_AGE,
     case when CCODE != '81' then OFFSEX else VICSEX end OFF_SEX,
     case when CCODE != '81' then OFFRACE else VICRACE end OFF_RACE,
     case when CCODE != '81' then OFFETHNICITY else VICETHNICITY end OFF_ETH,
     RELATIONCD REL_CODE
     from

-- 2. THE QUERY ABOVE TAKES "ALMOST FINISHED" OUTPUT, AND ACHIEVES TWO FINAL FLOURISHES:
--    (1) THE HOMICIDE TABLE HOUSES INFORMATION ON SUSPECTS (PERSONS NOT ARRESTED YET, BUT
--    UNDER INVESTIGATION). BUT, FOR UCR WE ARE ONLY REPORTING OFFENDER INFORMATION FOR 
--    CLEARED CASES. SO, THE SCRIPT ABOVE "TRICKS" SQL INTO NOT ADDING EXTRA UNDESIRED OFFENDER
--    RECORDS, (2) UCR GUIDELINES REQUIRE THAT, FOR JUSTIFIABLE HOMICIDES, THE PERSON SHOT IS 
--    CHARACTERIZED AS THE VICTIM. HOWEVER, THIS INDIVIDUAL IS CHARACTERIZED AS THE OFFENDER IN
--    HOMICIDE TABLE. SO, THE SCRIPT ABOVE MAKES THE NECESSARY SWITCH IN ROLE. 

(
      SELECT to_char(hom.injury_date, 'MM') MNTH, to_char(hom.injury_date, 'YYYY') YR, 
        to_char(hom.injury_date, 'HH24MI') TIMEMIL, weapid,
        case when substr(hom.homicide_no,1,1) in ('I') then 'B' else 'A' end HOMTYPECD,
        hom.cleared_i,
        sitcd.sitcode SITUATIONCD,
        case when substr(hom.homicide_no,1,1) in ('J') and (hom.victim_last_nme like '%#%' or  
                  hom.victim_last_nme like '%P.O.%' or hom.victim_last_nme like '%PO%' or
                  hom.victim_first_nme like '%#%' or hom.victim_first_nme like '%P.O.%' or
                  hom.victim_first_nme like '%PO%') then '81'
             when substr(hom.homicide_no,1,1) in ('J') and (hom.victim_last_nme not like '%#%' or  
                  hom.victim_last_nme not like '%P.O.%' or hom.victim_last_nme not like '%PO%' or
                  hom.victim_first_nme not like '%#%' or hom.victim_first_nme not like '%P.O.%' or
                  hom.victim_first_nme not like '%PO%') then '80'
             when substr(hom.homicide_no,1,1) in ('I') then ' '
             else CirCd.CIRCUMSTANCECODE end CCODE,

-- 3A. THE CASE STATEMENT ABOVE: UCR CODING FOR JUSTIFIABLE HOMICIDES DISTINGUISHES BETWEEN THOSE COMMITTED
--     BY POLICE VS. CITIZENS. THIS DISTINCTION IS NOT OVERTLY MADE IN THE HOMICIDE TABLE. SO THE CASE STATEMENT
--     ABOVE ATTEMPTS TO DISENTANGLE. 

        hom.death_cause_cd,
        hom.motive_descr,
        WEAPTABLE.HOM_HOMICIDE_ID,
        hom.injury_descr,
        WEAPTABLE.WEAPON_TYPE,
        WEAPTABLE.WEAPON_NAME,
        WEAPTABLE.HIERARCHY as WEAPONCD,
        dense_rank() over(partition by hom.rd_no order by hom.homicide_no) VICCNT,
        dense_rank() over(partition by hom.homicide_no order by hoff.id asc) OFFCNT,
        hom.rd_no,
        hom.homicide_no,
        hom.victim_last_nme,
        hom.victim_first_nme,
        case when to_char(hom.victim_age) = '1' then ' '    --****CHRIS does not capture age under 1.  ISP reporting requires a code of 'NB' for "birth to one week" and 'BB' for "newborn up to one year of age" (enter '1' if the victim is a year old).
             when to_char(hom.victim_age) between '02' and '98' then to_char(hom.victim_age)  
             when to_char(hom.victim_age) >= '99' then '99'
             else null end VICAGE,
        hom.victim_age,
        case when hom.victim_sex = 'M' then 'M'
             when hom.victim_sex = 'F' then 'F'
             else null end VICSEX,
        hom.victim_sex,
        case when hom.victim_race = 'API' then 'A'
             when hom.victim_race in ('BLK','WBH') then 'B'
             when hom.victim_race = 'I' then 'I'
             when hom.victim_race in ('WHI','WWH') then 'W'
             when hom.victim_race is null then ' '
             when hom.victim_race = 'U' then ' '
             else null end VICRACE,
        hom.victim_race,
        case when hom.victim_race in ('WBH', 'WWH') then 'H'
             when hom.victim_race in ('API', 'BLK', 'I','WHI') then 'N'
             when hom.victim_race is null then ' '
             when hom.victim_race = 'U' then ' '
             else null end VICETHNICITY,
        hoff.id HOFFID,
        case when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (to_char(hoff.age) between '01' and '98')) then to_char(hoff.age)
             when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (to_char(hoff.age) >= '99')) then '99'
             else '00' end OFFAGE,
        hoff.age,
        case when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.sex = 'M')) then 'M'
             when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.sex = 'F')) then 'F'
             else 'U' end OFFSEX,
        hoff.sex,
        case when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.race = 'API')) then 'A'
             when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.race = 'BLK')) then 'B'
             when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.race = 'I')) then 'I'
             when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.race in ('WHI','WBH','WWH'))) then 'W'
             else 'U' end OFFRACE,
        hoff.race,
        case when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.race in ('WBH','WWH'))) then 'H'
             when ((SITCODE not in ('B','F') and (hoff.last_nme is not null)) and (hoff.race in ('API','BLK','I','WHI'))) then 'N'
             else 'U' end OFFETHNICITY,

-- 3B. OFFENDER DEMOGRAPHIC CASE STATEMENTS: THE HOMICIDE TABLE HOUSES INFORMATION ON SUSPECTS 
--     (PERSONS NOT ARRESTED YET, BUT UNDER INVESTIGATION). BUT, FOR UCR WE ARE ONLY REPORTING 
--     OFFENDER INFORMATION FOR CLEARED CASES. SO, THE SCRIPT ABOVE DECLARES OFFENDER DEMOGRAPHICS
--     AS MISSING WHEN THE CASE IS NOT CLEARED. 

        hoff.last_nme,
        hoff.first_nme,
        case 
             when SITCODE in ('B', 'F') then 'UN'
             when hom.victim_rel_to_offender in ('CHILD') and hom.victim_sex = 'M' then 'SO'
             when hom.victim_rel_to_offender in ('CHILD') and hom.victim_sex = 'F' then 'DA'
             when hom.victim_rel_to_offender in ('LIFE PARTNER') and hom.victim_sex = 'M' then 'CH'
             when hom.victim_rel_to_offender in ('LIFE PARTNER') and hom.victim_sex = 'F' then 'CW'
             when hom.victim_rel_to_offender in ('BUSINESS PARTNER','CO-WORKER','DOCTOR',
                  'EX-ROOMMATES','LANDLADY','LANDLORD','MOTHER''S PARAMOUR','PATIENT',
                  'SOME ACQUAINTANCE','TENANT') then 'AQ'
             when hom.victim_rel_to_offender in ('BOYFRIEND','EX-BOYFRIEND') then 'BF'
             when hom.victim_rel_to_offender in ('BROTHER','HALF-BROTHER') then 'BR'
             when hom.victim_rel_to_offender in ('HUSBAND COMMON LAW') then 'CH'
             when hom.victim_rel_to_offender in ('WIFE COMMON LAW') then 'CW'
             when hom.victim_rel_to_offender in ('DAUGHTER') then 'DA'
             when hom.victim_rel_to_offender in ('EMPLOYEE') then 'EE'
             when hom.victim_rel_to_offender in ('EMPLOYER') then 'ER'
             when hom.victim_rel_to_offender in ('FATHER') then 'FA'
             when hom.victim_rel_to_offender in ('FRIENDS') then 'FR'
             when hom.victim_rel_to_offender in ('GIRLFRIEND','EX-GIRLFRIEND') then 'GF'
             when hom.victim_rel_to_offender in ('HUSBAND') then 'HU'
             when hom.victim_rel_to_offender in ('BROTHER IN LAW','FATHER-IN-LAW','MOTHER IN LAW',
                  'SISTER IN LAW','SON IN LAW') then 'IL'
             when hom.victim_rel_to_offender in ('MOTHER') then 'MO'
             when hom.victim_rel_to_offender in ('NEIGHBOR') then 'NE'
             when hom.victim_rel_to_offender in ('AUNT','COUSIN','FOSTER BROTHER',
                  'FOSTER DAUGHTER','FOSTER MOTHER','FOSTER SISTER','FOSTER SON',
                  'GRANDDAUGHTER','GRANDFATHER','GRANDMOTHER','GRANDSON','NEPHEW',
                  'NIECE','STEPBROTHER','UNCLE','WARD') then 'OF'
             when hom.victim_rel_to_offender in ('PARAMOUR''S CHILD','ROOMER','ROOMMATES') then 'OK'
             when hom.victim_rel_to_offender in ('STEPDAUGHTER') then 'SD'
             when hom.victim_rel_to_offender in ('STEPFATHER') then 'SF'
             when hom.victim_rel_to_offender in ('HALF-SISTER','SISTER') then 'SI'
             when hom.victim_rel_to_offender in ('STEPMOTHER') then 'SM'
             when hom.victim_rel_to_offender in ('SON') then 'SO'
             when hom.victim_rel_to_offender in ('STEPSON') then 'SS'
             when hom.victim_rel_to_offender in ('CUSTOMER','JANITOR','NONE','PROPRIETOR') then 'ST'
             when hom.victim_rel_to_offender in ('NOT ESTABLISHED') then 'UN'
             when hom.victim_rel_to_offender is null then 'UN'
             when hom.victim_rel_to_offender in ('WIFE') then 'WI'
             when hom.victim_rel_to_offender in ('EX-HUSBAND') then 'XH'
             when hom.victim_rel_to_offender in ('EX-WIFE') then 'XW'        
               else null end RELATIONCD

-- 3C. THE SCRIPT ABOVE TRANSLATES CPD RELATIONSHIP CODES INTO UCR RELATIONSHIP CODES. 

                  FROM CHRIS_DWH.hom_homicides_dw_redist hom
                    LEFT JOIN CHRIS_DWH.hom_offenders_dw hoff on hom.id = hoff.hom_homicide_id                   

-- 3. THE QUERY ABOVE IS THE DE FACTO MASTER QUERY, ASSEMBLING THE CORE INFORMATION IN THE FORM 
--    REQUIRED PER UCR GUIDELINES. 

                    LEFT JOIN 
                              (select *                                                                                 --This query selects the weapon in row number 1 for each RD.  So now we only have one weapon per homicide and the weapon that's selected is the most lethal weapon.
                              from (                                    
                                    select   WEAPID, HOMICIDE_NO, HOM_HOMICIDE_ID, WEAPON_TYPE, WEAPON_NAME, INJURY_DESCR, HIERARCHY,
                                             row_number() over (partition by HOM_HOMICIDE_ID order by HIERARCHY) RowNu     --This query orders weapons within each RD (orders them by listing the most fatal first).  It also numbers each weapon within an RD.
                                    from
                                             (
                                              select
                                              weap.hom_homicide_id, weap.id WEAPID, hom.homicide_no, hom.injury_descr, weap.weapon_type, weap.weapon_name,   --This subquery gives each weapon a hierarchical number.  This way if two different weapons are used in a homicide, the query above can select the most lethal weapon.
                                                  case
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_name = 'HANDGUN')) then '12'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_name = 'RIFLE')) then '13'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_name = 'SHOTGUN')) then '14'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'FIREARM' and weap.weapon_name = 'OTHER')) then '15'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'STABBING/CUTTING')) then '20'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'UNKNOWN' and hom.injury_descr = 'STABBED')) then '20'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'BLUDGEON/CLUB')) then '30'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'HANDS/FEET')) then '40'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'POISON')) then '50'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'BURN')) then '65'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'DRUG')) then '70'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_name = 'WATER (DROWNING)')) then '75'
                                                       when hom.injury_descr = 'STRANGULATION' then '80'
                                                       when hom.injury_descr = 'ASPHYXIATION' then '85'                                                      
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'VEHICLE')) then '90'  --we are categorizing vehicles used as weapons as OTHER which is ranked 17 on the ISP Weapon Codes list                                                 
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'LIGATURE')) then '90'                                                                                                             
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'OTHER')) then '90'
                                                       when ((hom.injury_descr not in ('STRANGULATION','ASPHYXIATION')) and (weap.weapon_type = 'UNKNOWN')) then '90'                                                       
                                                            else null end "HIERARCHY"

-- 4A. UCR WEAPON CATEGORIZATION INCLUDES "STRANGULATION" AND "ASPHYXIATION." BUT, THESE ARE NOT WEAPONS; 
--     THEY ARE METHODS. SO, CPD HAS THESE CATEGORIES IN AN INJURY DESCRIPTION FIELD, AS OPPOSED TO A WEAPON
--     CATEGORY FIELD. THE SCRIPT ABOVE FOLDS IN STRANGULATION AND ASPHXIATION, WHILE MAINTAINING MUTUAL EXCLUSIVITY 
--     IN THE CASE CATEGORIES. 

                                                                            from chris_dwh.hom_weapons_dw weap
                                                                            right join CHRIS_DWH.hom_homicides_dw_redist hom on hom.id = weap.hom_homicide_id
                                                                            where hom.injury_date between to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS') 
                                                                                                      And to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
                                                                            and hom.active_i = 'Y'                 
                                                                            and to_number(hom.curr_district) between 1 and 25
                                                                            order by hom.homicide_no, HIERARCHY ) WEAPONS --table that issues a "rank" to weapons. 
                                      )
                              where RowNu = '1'
                              order by HOMICIDE_NO  ) WEAPTABLE
                                          on hom.id = WEAPTABLE.HOM_HOMICIDE_ID     

-- 4. THE QUERY ABOVE PRODUCES SUPPLEMENTARY UCR MURDER WEAPON CATEGORIES. THE PRIMARY CHALLENGE IS THAT THE CPD 
--    SYSTEM MAKES IT POSSIBLE TO RECORD MULTIPLE WEAPONS. BUT, UCR REPORTING IS LIMITED TO A SINGLE WEAPON. SO, 
--    THE SCRIPT ABOVE IDENTIFIES A SINGLE, "MOST SERIOUS" WEAPON.  

                    LEFT JOIN (select distinct rd_no,                                                 --This query generates each RD# and the corresponding Situation Code.
                              case when cleared_I = 'Y' and (VICTOTAL = 1 and OFFTOTAL = 1) or          
                                        cleared_I = 'N' and (VICTOTAL = 1 and OFFTOTAL = 1)           --Added the substring because some justifed cases are coded as 'N' (cleared_I).  This causes the Sitation Code to be 'B' or 'F' for justified cases and they should not be 'B' or 'F'.
                                        and substr(homicide_no,1,1) in ('I','J') then 'A'             --Involuntary cases should also not be 'B' or 'F' since the offender is known.       
                                   when cleared_I = 'Y' and (VICTOTAL = 1 and OFFTOTAL > 1) or                                                                                 
                                        cleared_I = 'N' and (VICTOTAL = 1 and OFFTOTAL > 1) 
                                        and substr(homicide_no,1,1) in ('I','J') then 'C'               
                                   when cleared_I = 'Y' and (VICTOTAL > 1 and OFFTOTAL = 1) or                                                                           
                                        cleared_I = 'N' and (VICTOTAL > 1 and OFFTOTAL = 1) 
                                        and substr(homicide_no,1,1) in ('I','J') then 'D'                                                                                     
                                   when cleared_I = 'Y' and (VICTOTAL > 1 and OFFTOTAL > 1) or
                                        cleared_I = 'N' and (VICTOTAL > 1 and OFFTOTAL > 1) 
                                        and substr(homicide_no,1,1) in ('I','J') then 'E'
                                   when cleared_I = 'N' and (VICTOTAL = 1 and OFFTOTAL >= 1)            --If a case is not cleared then the appropriate Situation Code should be one that has the description containing "unknown" offender.  So if the case in 'N' in CLEARED_I and it's not an involuntary or justified case, then it should be 'B' or 'F'.   
                                        and substr(homicide_no,1,1) Not in ('I','J') then 'B'           --The Situation Code should not be 'B' or 'F' when the case is Justified or Involuntary.
                                   when cleared_I = 'N' and (VICTOTAL > 1 and OFFTOTAL >= 1) 
                                        and substr(homicide_no,1,1) Not in ('I','J') then 'F'
                                   else null end "SITCODE"
                            from (
                                 select distinct hom.rd_no, hom.homicide_no, hom.cleared_I,                                 --This subquery generates: RD#, each homicide #, whether the case is cleared or not, and the total number of victims and offender per RD#.
                                     count(distinct hom.homicide_no) over (partition by hom.rd_no) VICTOTAL,                --Total count of victims per RD#.
                                     (count(distinct hoff.id) over (partition by hom.rd_no)) / (count(distinct hom.homicide_no) over (partition by hom.rd_no)) OFFTOTAL  --Total count of offenders per RD#.                                   
                              from CHRIS_DWH.hom_homicides_dw_redist hom          
                                 left join CHRIS_DWH.hom_offenders_dw hoff on hom.id = hoff.hom_homicide_id
                              where hom.injury_date between to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS') 
                                                       And to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
                              and hom.active_i = 'Y' 
                              and to_number(hom.curr_district) between 1 and 25
                              order by hom.rd_no
                              )
                          ) SitCd on hom.rd_no = sitcd.rd_no

-- 5. THE QUERY ABOVE IDENTIFIES THE NUMBER OF VICTIMS AND OFFENDERS INVOLVED IN THE MURDER INCIDENT. THEN, 
--    IT ASSIGNS ONE OF THE UCR SITUATION CODES (CODES PREDICATED ON NUMBER OF VICTIMS / OFFENDERS). 

                    LEFT JOIN ( select hom.rd_no, hom.death_cause_cd, hom.victim_age,                                           --This query generates the RD#, DEATH_CAUSE_CD, and the corresponding Circumstance Code for the death_cause_cd.
                                       case                                                                      
                                          when hom.death_cause_cd in ('400', '405') then '17'
                                          when hom.death_cause_cd in ('840', '841') then '18'
                                          when hom.death_cause_cd in ('105') then '19'
                                          when hom.death_cause_cd in ('500', '925', '935') then '26'
                                          when hom.death_cause_cd in ('155') then '40'
                                          when hom.death_cause_cd in ('120') then '44'
                                          when hom.death_cause_cd in ('100', '110', '115', '135', '160') then '45'
                                          when hom.death_cause_cd in ('140', '700') and hom.victim_age > '17' then '46'
                                          when hom.death_cause_cd in ('140', '700') and hom.victim_age < '18' then '47'
                                          when hom.death_cause_cd in ('130', '150', '915', '920', '930', '940',
                                                                      '945', '950', '951', '955', '960', '965') then '60'
                                          when hom.death_cause_cd in ('600') then '99'
                                          when hom.death_cause_cd in ('300', '305') then '03'
                                          when hom.death_cause_cd in ('200') then '05'
                                          when hom.death_cause_cd in ('145', '905') then '06'
                                          when hom.death_cause_cd in ('900') then '09'
                                       else null end CIRCUMSTANCECODE
                                from CHRIS_DWH.hom_homicides_dw_redist hom
                                where hom.injury_date between to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS') 
                                                         And to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
                                and substr(hom.homicide_no,1,1) Not in ('I','J')
                                and to_number(hom.curr_district) between 1 and 25
                                order by hom.rd_no  ) CirCd on hom.rd_no = CirCd.rd_no

-- 6. THE QUERY ABOVE TRANSLATES CPD MURDER MOTIVE CODES TO UCR CIRCUMSTANCE CODES. 

        WHERE hom.injury_date between to_date('01/01/2016 00:00:00','MM/DD/YYYY HH24:MI:SS') 
                                  And to_date('12/31/2016 23:59:59','MM/DD/YYYY HH24:MI:SS')
        AND hom.active_i = 'Y'                          
        AND to_number(hom.curr_district) between 1 and 25
        ORDER BY hom.homicide_no, VICCNT, OFFCNT
) 

ORDER BY CASE#_TYPE2, VIC#, OFF#

) fulldata

ORDER BY CASE#_TYPE2, VIC#, OFF#

;

