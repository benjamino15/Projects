#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep  5 11:32:25 2022

@author: benjamin
"""

import os
os.getcwd()  
os.chdir("/Users/benjamin/Python")

import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import statsmodels as sm
import statsmodels.formula.api as smf

#Question 1
df = pd.read_csv("airbag_data_V47.csv", header = None)

df = np.transpose(df)
df.columns = ['vehicle 1', 'vehicle 2', 'vehicle 3', 'vehicle 4', 'vehicle 5'
              ,'vehicle 6', 'vehicle 7', 'vehicle 8', 'vehicle 9', 'vehicle 10']


#Splitting the data
#Plot first 5 vehicles without accident 
plt.plot(df.iloc[:, 0:5], label = df.columns[0:5])
plt.legend(bbox_to_anchor = (1, 1))
plt.xlabel('Milliseconds')
plt.ylabel('Acceleration')
plt.suptitle('Airbag deployment test results for first 5 vehicles (No crash)')

#Specify color scheme used for second set of vehicles
color = ["black", "brown", "salmon", "seagreen", "skyblue"]

#Create plot for last 5 vehicles (with accident)
for i in range(5, 10):
    plt.plot(df.iloc[:, i], label = df.columns[5:10], color = color[i-5])
    
plt.legend(df.columns[5:10], bbox_to_anchor = (1, 1))
plt.xlabel('Milliseconds')
plt.ylabel('Acceleration')
plt.suptitle('Airbag deployment test results for last 5 vehicles (crashed)')



#Question 2

#define raw data as one lag of accelerometer data
df_raw = df.shift(-1)

#Create empty list
airbag = []
#For loop assigning 1s and 0s to the vehicles surpassing the threshold 
for i in range(10):
    if min(df_raw.iloc[:,i]) <= -1:
        airbag.append(1)
    else:
        airbag.append(0)  
        
#Define variable to calculate succes rate
def succes_rate(airbag):
    result = np.concatenate(((np.full(5, 1) - airbag[0:5]), airbag[5:10]))
    return (sum(result)/len(result))
        
succes_rate(airbag)
        

#Question 3

#Scaled version of data
df_scaled = 10*df_raw

#Plot not asked for, but good to visualize the data
plt.plot(df_scaled, label = df.columns)
plt.axhline( y = -1, linestyle = "--")
plt.legend(bbox_to_anchor = (1, 1))
plt.xlabel('Milliseconds')
plt.ylabel('$Signal_{it}$')
plt.suptitle('Airbag deployment test results for 10 vehicles')


#Determine success rate
airbag = []

for i in range(10):
    if min(df_scaled.iloc[:,i]) <= -1:
        airbag.append(1)
    else:
        airbag.append(0)  
        
succes_rate(airbag)


#Question 4

#Define functions for filtering the time series

def filterA (x, s):
    signalA = np.full((10001, 10),-0.0135) + 0.4*df_raw + 0.975*df_scaled
    return signalA 

def filterB (x, s):
    signalB = np.full((10001, 10),-0.0135) + 0.8*df_raw + 0.975*df_scaled
    return signalB 

def filterC (x, s):
    signalC = np.full((10001, 10),-0.0135) + 0.5*df_raw + 0.975*df_scaled
    return signalC

df_signalA = filterA( df_raw,  df_scaled)
df_signalB = filterB( df_raw,  df_scaled)
df_signalC = filterC( df_raw,  df_scaled)


#Create dummy data to add a customized legend later (ignore this)
  from matplotlib.lines import Line2D
  custom_lines = [Line2D([0], [0], color= "blue", lw=4),
                  Line2D([0], [0], color= "orange", lw=4),
                  Line2D([0], [0], color= "black", lw=4, linestyle = ":")]
    
#Plot each time series seperately for signal A
 fig, axes = plt.subplots(2,5, figsize = (15,7))
 for i in range(2):
     for j in range(5):
         axes[i,j].plot(df_signalA.iloc[:, 5*i+j], label = df.columns[5*i+j])
         axes[i,j].plot(df.iloc[:, 5*i+j], label = df.columns[5*i+j], linestyle = ":")
         axes[i,j].axhline( y = -1, linestyle = "--", color = "black")
         plt.setp(axes, ylim=(-2,2))
         plt.suptitle('Airbag deployment test results for 10 vehicles (Signal A)', size = 20)
         plt.legend(custom_lines, ['Signal', "Acceleration", 'Deployment threshold'], 
                    bbox_to_anchor = (-0.5, -0.2), ncol = 3) 
    
 

#Plot for signal B
 fig, axes = plt.subplots(2,5, figsize = (15,7))
 for i in range(2):
     for j in range(5):
         axes[i,j].plot(df_signalB.iloc[:, 5*i+j], label = df.columns[5*i+j])
         axes[i,j].plot(df.iloc[:, 5*i+j], label = df.columns[5*i+j], linestyle = ":")
         axes[i,j].axhline( y = -1, linestyle = "--", color = "black")
         plt.setp(axes, ylim=(-2,2))
         plt.suptitle('Airbag deployment test results for 10 vehicles (Signal B)', size = 20)
         plt.legend(custom_lines, ['Signal', "Acceleration", 'Deployment threshold'], 
                    bbox_to_anchor = (-0.5, -0.2), ncol = 3) 

#Plot for signal C

 fig, axes = plt.subplots(2,5, figsize = (15,7))
 for i in range(2):
     for j in range(5):
         axes[i,j].plot(df_signalC.iloc[:, 5*i+j], label = df.columns[5*i+j])
         axes[i,j].plot(df.iloc[:, 5*i+j], label = df.columns[5*i+j], linestyle = ":")
         axes[i,j].axhline( y = -1, linestyle = "--", color = "black")
         plt.setp(axes, ylim=(-2,2))
         plt.suptitle('Airbag deployment test results for 10 vehicles (Signal C)', size = 20)
         plt.legend(custom_lines, ['Signal', "Acceleration", 'Deployment threshold'], 
                    bbox_to_anchor = (-0.5, -0.2), ncol = 3) 

#Success rate filter A
airbag = []

for i in range(10):
    if min(df_signalA.iloc[:,i]) <= -1:
        airbag.append(1)
    else:
        airbag.append(0)  
        
succes_rate(airbag)

#Success rate filter B
airbag = []

for i in range(10):
    if min(df_signalB.iloc[:,i]) <= -1:
        airbag.append(1)
    else:
        airbag.append(0)  
        
succes_rate(airbag)

#Success rate filter C
airbag = []

for i in range(10):
    if min(df_signalC.iloc[:,i]) <= -1:
        airbag.append(1)
    else:
        airbag.append(0)  
        
succes_rate(airbag)


