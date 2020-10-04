# -*- coding: utf-8 -*-
"""
Created on Mon Sep 28 17:31:19 2020

@author: user
"""

from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
from time import sleep

driver = webdriver.Chrome("C:/Users/user/Downloads/chromedriver_win32 (1)/chromedriver.exe")
frames=[]


for y in range(2008,2019):

    driver.get("https://www.iplt20.com/matches/results/men/"+ str(y))
    sleep(5)
    content = driver.page_source
    soup = BeautifulSoup(content)    

    teams = []
    scores = []    

    for a in soup.find_all('div', attrs = {'class':'result__teams'}):
        for b in a.find_all('p', attrs = {'class':'result__team-name'}):
            teams.append(b.text)
        for c in a.find_all('strong'):
            scores.append(c.text)
            
    result=[]            
    for a in soup.find_all('p', attrs = {'class':'result__outcome u-hide-phablet'}):
        if "abandoned" in (a.text.lower()).split() :
            result.append(1)
        elif ("no" and "result") in (a.text.lower()).split() :
            result.append(1)
        else:
            result.append(0)
        
        
    indices = [i for i, x in enumerate(result) if x == 1 ]

    result = [x for x in result if x!=1]
    
    n = int(len(teams)/2) #No of matches
    t = (np.array(teams).reshape(n,2)).tolist()

    for p in sorted(indices, reverse = True):  
        del t[p]
   
    if y == 2015:
        del scores[10:12] #This has been done becuase year 2015 contains out of pattern result
        
    
    t = np.array(t)    
    n = int(len(scores)/2) #No of unabandoned matches

    
    s = np.array(scores).reshape(n,2)
    
    z = np.concatenate((t,s), axis = 1)
    
    x = pd.DataFrame(z, columns = ["T1", "T2", "score_1", "score_2"])
    x["year"] = y
    
    frames.append(x)
    
data = pd.concat(frames, ignore_index = True)
data.to_csv("IPL_Matches(2008-18).csv")

