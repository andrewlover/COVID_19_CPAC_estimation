#mcandrew

import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

if __name__ == "__main__":


    meanTrajectory = pd.read_csv('./analysisData/meanPrediction.csv')
    meanTrajectory.columns= ['index','est']
    
    sampledTrajectories = pd.read_csv('./analysisData/sampledTrajectories.csv')

    _80CI = pd.read_csv('./analysisData/_80%CIS.csv')
    _80CI = pd.pivot_table(_80CI, columns=['Unnamed: 0'], values=["V{:d}".format(x) for x in np.arange(1,16+1)])
    _80CI.index = [int(x[1:]) for x in _80CI.index]
    _80CI = _80CI.sort_index()

    fig,ax = plt.subplots()

    days = np.arange(1,16+1,1)
    
    ax.plot(days, meanTrajectory.est, 'k-')
    ax.plot(days, _80CI['10%'],'k-')
    ax.plot(days, _80CI['90%'],'k-')
    ax.fill_between(days, _80CI['10%'], _80CI['90%'], color = 'b', alpha=0.10  )

    ax.set_ylabel('Total National COVI19 infections',fontsize=10)
    
    ax.set_xlim(1,16)
    ax.set_yticklabels(["{:d}".format(int(y/10**4)) + r"$\times 10^{4}$" for y in ax.get_yticks()])

    ax.set_xticks( [1] + list(np.arange(4,16+2,2)) )
    ax.set_xticklabels( ["Mar. 1"] +["Mar. {:d}".format(x) for x in np.arange(4,16+2,2)], fontsize=8 )
    
    ax.tick_params(direction='in',size=2)
    
    fig.set_tight_layout(True)
    plt.savefig('./figs/estimatedTrajectory.pdf')
    plt.close()
