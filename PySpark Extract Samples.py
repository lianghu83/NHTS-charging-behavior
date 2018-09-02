#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 29 10:30:01 2018

@author: Liang Hu

A pyspark code to extract 2017 NHTS personal vehicle samples.
"""

#find dir where Spark is installed
import findspark
findspark.init()

from pyspark.sql import SparkSession
from pyspark.sql import functions as F
import sys
from pyspark.sql.window import Window
from pyspark.sql import SQLContext

#initiate a Spark session
spark = SparkSession.builder.appName("NHTS").getOrCreate()

#read data as Spark dataframe
path = '/Users/LiangHu/Downloads/NHTS/'
trip = spark.read.csv(path+'trippub.csv', inferSchema=True, header=True)

trip.printSchema()
trip.head(3)
trip.show(3)
trip.describe()
print((trip.count(), len(trip.columns)))

#filter
pov = trip.filter((trip['DRVR_FLG']==1) & (trip['TRPTRANS'].isin({3,4,5,6})))

#create unique vehicle ID
pov = pov.withColumn('HOUSEID', pov['HOUSEID'].cast('long')) #convert integer to long
pov = pov.withColumn('HOUSE_VEH_ID', 100*pov['HOUSEID']+pov['VEHID'])
pov.select('HOUSE_VEH_ID').distinct().count()
pov.select('HOUSE_VHE_ID').head(10)

#summary trip distance
pov = pov.withColumn('VMT_MILE', pov['VMT_MILE'].cast('float')) 
pov.select('VMT_MILE').describe().show()
VMT_ub = pov.approxQuantile('VMT_MILE', probabilities=[0.999], relativeError=0)[0]

#create ORIGIN based on WHYFROM
ORIGIN = (F.when(pov['WHYFROM'].isin({1,2}), 'home')\
          .when(pov['WHYFROM'].isin({3,4}), 'work')\
          .when(pov['WHYFROM']>=5, 'public'))
pov = pov.withColumn('ORIGIN', ORIGIN)

#create DESTINATION based on WHYTRP1S
DESTINATION = (F.when(pov['WHYTRP1S']==1, 'home')\
               .when(pov['WHYTRP1S']==10, 'work')\
               .when(pov['WHYTRP1S']>=20, 'public'))
pov = pov.withColumn('DESTINATION', DESTINATION)

#remove missing values and errors, delete all affected HOUSE_VEH_ID
VMT_MILE_flag = (F.when((pov['VMT_MILE']<0) | (pov['VMT_MILE']>VMT_ub), 0)\
                 .otherwise(1))
pov = pov.withColumn('VMT_MILE_flag', VMT_MILE_flag)
STRTTIME_flag = (F.when(pov['STRTTIME']<0, 0).otherwise(1))
pov = pov.withColumn('STRTTIME_flag', STRTTIME_flag)
ENDTIME_flag = (F.when(pov['ENDTIME']<0, 0).otherwise(1))
pov = pov.withColumn('ENDTIME_flag', ENDTIME_flag)
WHYTRP1S_flag = (F.when(pov['WHYTRP1S']<0, 0).otherwise(1))
pov = pov.withColumn('WHYTRP1S_flag', WHYTRP1S_flag)
WHYFROM_flag = (F.when(pov['WHYFROM']<0, 0).otherwise(1))
pov = pov.withColumn('WHYFROM_flag', WHYFROM_flag)
pov = pov.withColumn('flag', pov['VMT_MILE_flag']*pov['STRTTIME_flag']*pov['ENDTIME_flag']*pov['WHYTRP1S_flag']*pov['WHYFROM_flag'])
temp = pov.select(['HOUSE_VEH_ID', 'flag'])
temp = temp.groupBy('HOUSE_VEH_ID').mean('flag')
temp = temp.withColumn('avg(flag)', temp['avg(flag)'].cast('integer'))
temp = temp.filter(temp['avg(flag)']<1)
Remove_HOUSE_VEH_ID = temp.select('HOUSE_VEH_ID')
Remove_HOUSE_VEH_ID.show(3)
Remove_HOUSE_VEH_ID = Remove_HOUSE_VEH_ID.rdd.flatMap(lambda x: x).collect()
pov = pov.filter(~pov['HOUSE_VEH_ID'].isin(Remove_HOUSE_VEH_ID))
drop_list = ['VMT_MILE_flag', 'STRTTIME_flag', 'ENDTIME_flag', 'WHYTRP1S_flag', 'WHYFROM_flag', 'flag']
pov = pov.select([col for col in pov.columns if col not in drop_list])
pov.printSchema()

#calculate new dwell time for vehicle = starttime - endtime
STRTTIME_new = (F.when(pov['STRTTIME']>=400, pov['STRTTIME']-400)\
                .otherwise(pov['STRTTIME']+2000))
pov = pov.withColumn('STRTTIME_new', STRTTIME_new)
ENDTIME_new = (F.when(pov['ENDTIME']>=400, pov['ENDTIME']-400)\
                .otherwise(pov['ENDTIME']+2000))
pov = pov.withColumn('ENDTIME_new', ENDTIME_new)
pov = pov.orderBy(['HOUSE_VEH_ID', 'STRTTIME_new'])
temp = pov.select('STRTTIME_new').rdd.flatMap(lambda x: x).collect()
temp.append(2359)
#add STRTTIME_new_next
#convert list to dataframe
STRTTIME_new_next = spark.createDataFrame([(l,) for l in temp], ['STRTTIME_new_next'])
STRTTIME_new_next.count()
STRTTIME_new_next = STRTTIME_new_next.withColumn('columnindex', F.monotonically_increasing_id()) #an issue with monotonically_increasing_id()
STRTTIME_new_next.orderBy('columnindex', ascending=False).head(1)
#change id
pov = pov.withColumn('columnindex', F.monotonically_increasing_id())
pov.count()
pov.head(3)
pov.orderBy('columnindex', ascending=False).head(1)
pov = pov.join(STRTTIME_new_next, pov.columnindex==STRTTIME_new_next.columnindex).drop('columnindex')
pov = pov.withColumn('DWELTIME_new', pov['STRTTIME_new_next']/100*60\
                     +pov['STRTTIME_new_next']%100\
                     -pov['ENDTIME_new']/100*60\
                     -pov['ENDTIME_new']%100)
  
#here should be a window function
"""
windowSpec = \
  Window\
    .partitionBy(pov['HOUSE_VEH_ID']).rangeBetween(-sys.maxsize, sys.maxsize)
    #.orderBy(df['revenue'].desc()) \
    
DWELTIME_new = (F.when(F.last(pov['HOUSE_VEH_ID']).over(windowSpec), pov['DWELTIME_new']).otherwise(1440-pov['DWELTIME_new'])) 
    
pov.withColumn('XX', DWELTIME_new).head(3)
"""

#remove error dwelltime
DWELTIME_new_flag = (F.when(pov['DWELTIME_new']<0, 0).otherwise(1))
pov = pov.withColumn('DWELTIME_new_flag', DWELTIME_new_flag)
temp = pov.select(['HOUSE_VEH_ID', 'DWELTIME_new_flag'])
temp = temp.groupBy('HOUSE_VEH_ID').mean('DWELTIME_new_flag')
temp = temp.withColumn('avg(DWELTIME_new_flag)', temp['avg(DWELTIME_new_flag)'].cast('integer'))
temp = temp.filter(temp['avg(DWELTIME_new_flag)']<1)
Remove_HOUSE_VEH_ID = temp.select('HOUSE_VEH_ID')
Remove_HOUSE_VEH_ID = Remove_HOUSE_VEH_ID.rdd.flatMap(lambda x: x).collect()
pov = pov.filter(~pov['HOUSE_VEH_ID'].isin(Remove_HOUSE_VEH_ID))
pov = pov.drop('DWELTIME_new_flag')
pov.printSchema()

#write
pov.write.csv(path+'POV_spark.csv')


