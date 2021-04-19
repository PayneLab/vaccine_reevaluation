import pandas as pd

who1 = pd.read_csv("BIO465_Vaccine/data/WHO-CHILD-HEALTH-1.csv")
who2 = pd.read_csv("BIO465_Vaccine/data/WHO-CHILD-HEALTH-2a.csv")
who3 = pd.read_csv("BIO465_Vaccine/data/WHO-CHILD-HEALTH-3.csv")
who4 = pd.read_csv("BIO465_Vaccine/data/WHO-CHILD-HEALTH-4.csv")
whoIMR = pd.read_csv("BIO465_Vaccine/data/WHO-IMR-2017-2019.csv")
pd.set_option('display.max_columns', None)

who_subset = who1[['GHO','YEAR','COUNTRY', 'Numeric']]
countries = sorted(list(set(who_subset['COUNTRY'])))
years = sorted(list(set(who_subset['YEAR'])))
codes = sorted(list(set(who_subset['GHO'])))

country_year = []

codes.insert(0, "YEAR")
codes.insert(0, "COUNTRY")

for i in countries:
    for j in years:
        country_year.append(i+"_"+str(j))

who1b = pd.DataFrame(index=country_year, columns=codes)

for i in country_year:
    for j in codes:
        country = i[:3]
        year = int(i[4:])
        code = j
        temp = who_subset[(who_subset["COUNTRY"]==country) & (who_subset["YEAR"]==year)]
        factors = temp['GHO'].tolist()
        temp.index = factors
        if j not in factors:
            continue
        else:
            who1b.at[i,"COUNTRY"] = country
            who1b.at[i,"YEAR"] = year
            who1b.at[i,j] = temp.at[code,"Numeric"]

print(who1b)




#TESTING FOR OPTIMIZATION BELOW
#who_subset_cp = who_subset.copy()

#who_subset['Country_Year'] = who_subset_cp['COUNTRY'].str.cat(who_subset_cp['YEAR'].astype(str), sep="_")
#country_year = pd.Index(who_subset["Country_Year"], name="Country_Year")

# who_test = who_subset.copy()[['Country_Year', 'GHO', 'Numeric']]
# who_test.set_index('Country_Year')
# filter = who_test['Country_Year'] == 'AFG_2015'
# print(who_test[filter].head())
#{"GHO":"Numeric"}
#print(who_test.head())
#print(who_test.index)