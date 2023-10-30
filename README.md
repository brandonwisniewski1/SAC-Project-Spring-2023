# SAC-Project-Spring-2023
This repo contains the code used to make my Spring Project in the Spring of 2023. In this project, I focused on Lamar Jackson and why (at the time) he deserved the lucrative contract he was asking for.


In this file I used the following packages: 

library(tidyverse)
library(readr)
library(ggplot2)
library(rvest)
library(XML)
library(reshape2)
library(gt)
library(ggimage)

My primary goal was establishing the relationship between annual salary (AAV) and production, which I quantified using yards/attempt, completion percentage, and EPA per play from the nflfastR package. I went further in my evaluation by distinguishing each contract between sticker amount and guaranteed money, an important feature of NFL contracts that sets it apart from other sports. All contract-related data I obtained from spottrac.com. 
