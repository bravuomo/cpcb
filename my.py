#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 14 22:26:00 2024

@author: ghoomketu
"""
from os import listdir 
import pandas as pd
import datetime as dt

myPath = '/home/ghoomketu/Tink'
myFiles = listdir(myPath)
myMonths = [4,5,6]
myHours = list(range(7,19))

# for i in myFiles:
#     print(i)

fields = ['Timestamp','Ozone (µg/m³)']

df = pd.read_csv(myFiles[0],usecols=fields)

df['Samay'] = pd.to_datetime(df['Timestamp'])
df['Mahina'] = df['Samay'].dt.month
df['Ghanta'] = df['Samay'].dt.hour
df['ppb_40'] = (df['Ozone (µg/m³)']/2 - 40)
# print(df)

pf = df[['Mahina','Ghanta','ppb_40']]


wo_Mahina = pf['Mahina'].isin(myMonths)
wo_Ghanta = pf['Ghanta'].isin(myHours)
wo_ppb_40 = pf['ppb_40']>0

cf = pf[wo_Mahina & wo_Ghanta & wo_ppb_40]
print(sum(cf['ppb_40']))