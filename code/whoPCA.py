import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn import preprocessing
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler

who = pd.read_csv("BIO465_Vaccine/data/WHO-SIMPLE.csv")

#apply any filters worth analyzing
filter = who["Year"] >= 2000
filter2 = who["Country Code"] == "USA"
who = who[filter]
#who = who[filter2]

#Select which factors you would like to analyze
#who = who[["Country_Year", "MDG_0000000001", "PCV3", "ROTAC", "WHS4_100", "WHS4_117", "WHS4_129", "WHS4_543", "WHS4_544", "WHS8_110", "MCV2", "NUTRITION_564", "WHS4_128", "LBW_NUMBER", "LBW_PREVALENCE", "NUTRITION_HA_2", "NUTRITION_WA_2", "NUTRITION_WH2", "NUTRITION_WH_2", "WHOSIS_000005", "WHOSIS_000006", "MDG_0000000026", "WHS9_95", "WHS_PBR", "WSH_2", "WSH_3", "WSH_SANITATION_SAFELY_MANAGED", "M_Est_smk_curr", "M_Est_smk_daily", "TOBACCO_0000000192", "GHED_CHEGDP_SHA2011", "WHS9_85"]]
who = who[["Country_Year", "MDG_0000000001", "LBW_PREVALENCE",  "GHED_CHEGDP_SHA2011", "GDP"]]
who = who.set_index("Country_Year")
#Drop rows with NA values
who.dropna(axis=0,how="any",inplace=True)


#Conduct PCA
imr_column = who.loc[:,"MDG_0000000001"]
imr = imr_column.values

category = pd.qcut(who.MDG_0000000001,q=4, labels=["low imr", "moderately low imr", "moderately high imr", "high imr"])
length = len(who.columns)
who.insert(length, "IMR Category", category)
#Remove continous variables for imr
del who["MDG_0000000001"]

features = ["LBW_PREVALENCE",  "GHED_CHEGDP_SHA2011", "GDP"]
# Separating out the features
x = who.loc[:, features].values
# Separating out the target
y = who.loc[:,['IMR Category']].values
# Standardizing the features
x = StandardScaler().fit_transform(x)

#switch index to allow for join with PCs 1 and 2
who['rows'] = np.arange(len(who))
who = who.set_index('rows')

pca = PCA(n_components=2)
principalComponents = pca.fit_transform(x)
principalDf = pd.DataFrame(data = principalComponents, columns = ['principal component 1', 'principal component 2'])
finalDf = pd.concat([principalDf, who[['IMR Category']]], axis = 1)

#print(finalDf)

fig = plt.figure(figsize = (8,8))
ax = fig.add_subplot(1,1,1)
ax.set_xlabel('Principal Component 1', fontsize = 15)
ax.set_ylabel('Principal Component 2', fontsize = 15)
ax.set_title('2 component PCA', fontsize = 20)
targets = ['high imr', 'moderately high imr', 'moderately low imr', 'low imr']
colors = ['r', 'g', 'b', 'c']
for target, color in zip(targets,colors):
    indicesToKeep = finalDf['IMR Category'] == target
    ax.scatter(finalDf.loc[indicesToKeep, 'principal component 1']
               , finalDf.loc[indicesToKeep, 'principal component 2']
               , c = color
               , s = 50)
ax.legend(targets)
ax.grid()

#This ratio tells you the amount of variance explained by PC1 and PC2
print(pca.explained_variance_ratio_)
plt.show()


