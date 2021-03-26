import pandas as pd
from sklearn import linear_model, preprocessing
import statsmodels.api as sm


def split_cols(df, col, delim=":"):
    split = col.split(delim)

    df[split] = df[col].str.split(delim, expand=True)
    df.drop(col, axis=1, inplace=True)
    df['Country'] = df['Country'].str.strip()

    return df

def MLR(df, response_var, explanatory_vars):
    '''
    Parameters:
        df: DataFrame containing the full dataset including response_var and explanatory_vars
        response_var: (String) the name of the column containing the response variable for MLR
        explanatory_vars: (list/iterable of strings) contains the names of the features to explore with MLR

    Returns:
        regr: a LinearRegression model from sklearn that is a fit model to the data provided in df.
            It contains useful information as member variables that may be worth exploring
        model: a statsmodel Multiple Linear regression model fit to the data provided in df.
            It contains much of the same information as sklearn does, but in a more "R-centric" format
            that can be more statistically intuitive
        predictions: a statsmodel object based on 'model' that shows what the model would predict the
            response_var to be in any given row and can be used to predict the response_var on new data that
            may not contain the response_var
    '''
    x_and_y_cols = explanatory_vars.copy()
    x_and_y_cols.append('MDG_0000000001')
    df_no_nan = df[x_and_y_cols].copy().dropna()
    X = df_no_nan[explanatory_vars]  # Our multiple variables
    Y = df_no_nan['MDG_0000000001']

    regr = linear_model.LinearRegression()
    regr.fit(X, Y)

    print("MLR Results using Sci-kit Learn:")
    print('Intercept: \n', regr.intercept_)
    print('Coefficients: \n', regr.coef_)
    print()

    # with statsmodels
    X = sm.add_constant(X)  # adding a constant

    model = sm.OLS(Y, X).fit()
    predictions = model.predict(X)

    print(model.summary())

    return regr, model, predictions


def normalize(who_usa):
    who_max_scaled = who_usa.copy()
    for column in who_max_scaled.columns:
        who_max_scaled[column] = who_max_scaled[column]  / who_max_scaled[column].abs().max()
    return who_max_scaled


def makeFigure3():
    who = pd.read_csv("~/Desktop/WHO-SIMPLE.csv")
    who = split_cols(who, 'Country_Year', delim='_')
    who['Year'] = who['Year'].astype(int)
    who_relevant = who[who['Year'] >= 2000]
    who_usa = who_relevant[who_relevant['Country'] == 'USA']
    del who_usa['Country']
    del who_usa['Country Code']
    who_usa = normalize(who_usa)
    explanatory_vars = ['WHS4_100', 'WHS4_117', 'WHS4_129', 'WHS4_544', 'WHS8_110',
                        'MCV2', 'LBW_NUMBER', 'LBW_PREVALENCE', 'MDG_0000000026',
                        'WSH_SANITATION_SAFELY_MANAGED', 'GHED_CHEGDP_SHA2011', 'GDP']
    response_var = 'MDG_0000000001'

    MLR(who_usa, response_var, explanatory_vars)


if __name__ == '__main__':
    makeFigure3()


