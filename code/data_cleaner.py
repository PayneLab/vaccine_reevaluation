def split_cols(df, col, delim=":", drop=True):
    split = col.split(delim)
    
    df[split] = df[col].str.split(delim, expand=True)
    df[split[0]] = df[split[0]].str.strip()
    df[split[1]] = df[split[1]].str.strip()
    
    if drop:
        df.drop(col, axis=1, inplace=True)
    
    return df

def clean_data(df, to_remove, to_split):
    relevant_cols = []
    new_df = df.copy()
    
    #Split desired columns
    print("Columns split:\n")
    
    for col in to_split:
        print("Original Column:", col, '\n')
        new_df = split_cols(new_df, col)
        to_remove.append(col)
        relevant_cols.append(col.split(":")[0])
        relevant_cols.append(col.split(":")[1])
    
    print("Relevant columns:", relevant_cols, '\n')
    
    new_df.drop(columns=to_remove)
    print("Columns removed:", to_remove, '\n')
    
    return new_df[relevant_cols]
