import pandas as pd
import numpy as np
import xlwings as xw
import os

def to_df(range):
  return range.options(pd.DataFrame, expand='table', index=False).value

def paste(range, value, down):
  
  if isinstance(value, pd.DataFrame) :
    value = value.values 
  else :
    value = np.array(value)
    
  if down:
    value = value.reshape(-1, 1)

  range.options(np.ndarray).value = value
