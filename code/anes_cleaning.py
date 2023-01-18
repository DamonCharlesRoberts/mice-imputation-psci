# Title: ANES Cleaning for Random Forest MICE 

# Notes:
    #* Description: Python script for cleaning the 2020 ANES to be used for imputation
    #* Updated: 2022-03-25
    #* Updated by: dcr

# Setup
    #* Load relevant functions
import pandas as pd
import numpy as np

    #* Load 2020 ANES dataset
anesO = pd.read_csv('data/anes_timeseries_2020.csv', low_memory = False)


# Grab a bunch of columns that have some non-administrative value to them
anesPost = anes[['V202054x', 'V202065x', 'V202068x', 'V202105x', 'V202106x', 'V202107x', 'V202108x', 'V202116', 'V202117', 'V202118', 'V202119', 'V202122', 'V202123', 'V202138y', 'V202139y1', 'V202140y1', 'V202141y1', 'V202142y1', 'V202143', 'V202144', 'V202145', 'V202146', 'V202147', 'V202148', 'V202149', 'V202150', 'V202156', 'V202157', 'V202158', 'V202159', 'V202160', 'V202161', 'V202162', 'V202163', 'V202164', 'V202165', 'V202166', 'V202167', 'V202168', 'V202169', 'V202170', 'V202171', 'V202172', 'V202173', 'V202174', 'V202175', 'V202176', 'V202177', 'V202178', 'V202179', 'V202180', 'V202181', 'V202182', 'V202183', 'V202184', 'V202185', 'V202186', 'V202187', 'V202203x', 'V202212', 'V202213', 'V202214', 'V202215', 'V202216', 'V202219', 'V202220', 'V202221', 'V202222', 'V202223', 'V202224', 'V202225', 'V202231x', 'V202232', 'V202233', 'V202236x', 'V202242x', 'V202252x', 'V202255x', 'V202259x', 'V202260', 'V202261', 'V202262', 'V202263', 'V202264', 'V202265', 'V202266', 'V202267', 'V202268', 'V202269', 'V202270', 'V202273x', 'V202276x', 'V202279x', 'V202282x', 'V202286x', 'V202290x', 'V202292', 'V202300', 'V202301', 'V202302', 'V202303', 'V202304', 'V202305', 'V202308x', 'V202310', 'V202311', 'V202312', 'V202320x', 'V202321', 'V202325', 'V202328x', 'V202331x', 'V202336x', 'V202341x', 'V202344x', 'V202347x', 'V202350x', 'V202351', 'V202352', 'V202355', 'V202357', 'V202361x', 'V202364x', 'V202367x', 'V202370x', 'V202373x', 'V202376x', 'V202377', 'V202380x', 'V202383x', 'V202387x', 'V202390x', 'V202451', 'V202457', 'V202468x', 'V202490x', 'V202493x', 'V201507x', 'V201511x', 'V201533x', 'V201549x', 'V201600']]


# Get rid of all negative values - some type of NA
anesPost = anesPost._get_numeric_data()
anesPost[anesPost < 0] = np.nan

# Save as csv
anesPost.to_csv('data/anes_2020_clean.csv')