import torch
#import torch.nn as nn
#import pandas as pd
import numpy as np
#import matplotlib.pyplot as plt
import os
#import sys
import random
#import functools
#import operator


def seed_everything(seed=1):
    seed = int(seed)
    random.seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.backends.cudnn.deterministic = True
      
