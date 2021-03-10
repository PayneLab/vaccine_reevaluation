import pandas as pd


who1 = pd.read_csv("BIO465_Vaccine/data/WHO-CHILD-HEALTH-1.csv")
pd.set_option('display.max_columns', None)


filter = who1["YEAR"] == 2018
who1 = who1[filter]
who_subset = who1[['GHO','YEAR','COUNTRY', 'Numeric']]
countries = sorted(list(set(who_subset['COUNTRY'])))
codes = sorted(list(set(who_subset['GHO'])))
index = pd.Index(countries, name='COUNTRY')

who1b = pd.DataFrame(index=index, columns=codes)

for i in countries:
    for j in codes:
        country = i
        code = j
        filter = who_subset['COUNTRY'] == country
        temp = who_subset[filter]
        factors = temp['GHO'].tolist()
        temp.index = factors
        if j not in factors:
            continue
        else:
            who1b.at[country,code] = temp.at[code, 'Numeric']

print(who1b)
