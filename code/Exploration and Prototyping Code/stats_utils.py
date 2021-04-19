import pandas as pd
from sklearn import linear_model
import statsmodels.api as sm

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
    X = df_no_nan[explanatory_vars] # Our multiple variables
    Y = df_no_nan['MDG_0000000001']
    
    regr = linear_model.LinearRegression()
    regr.fit(X, Y)

    print("MLR Results using Sci-kit Learn:")
    print('Intercept: \n', regr.intercept_)
    print('Coefficients: \n', regr.coef_)
    print()

    # with statsmodels
    X = sm.add_constant(X) # adding a constant

    model = sm.OLS(Y, X).fit()
    predictions = model.predict(X) 

    print(model.summary())
    
    return regr, model, predictions