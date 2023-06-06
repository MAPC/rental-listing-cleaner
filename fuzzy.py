import pandas as pd
import numpy as np
from sklearn.cluster import KMeans
from numpy import cos, sin, arcsin, sqrt
from math import radians
from fuzzywuzzy import fuzz 
import time

def haversine(lat1,lon1,lat2,lon2):
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * arcsin(sqrt(a)) 
    km = 6367 * c
    return km

def fuzzdist(a, l):
    return [1.0 - fuzz.token_sort_ratio(a, b)/100.0 for b in l]

# def fuzzdist(a, b):
    # return 1.0 - fuzz.token_sort_ratio(a, b)/100.0

def fuzz_token_set_ratio(df, df_num):
    index_i,index_j,ratio_greater_85,title_i,title_j=[],[],[],[],[]
    diff_list=[]
    harv_list=[]
    df_len = len(df_num)

    for i in range(df_len):
        for j in range(i+1,df_len):
                if df_num[i][0]==df_num[j][0]:
                    ratio4=fuzz.token_sort_ratio(df_num[i][1],df_num[j][1])
                    if ratio4 > 85:
                        index_i.append(i)
                        index_j.append(j)
                        ratio_greater_85.append(ratio4)
                        diff=df.at[i,'ask']-df.at[j,'ask']
                        diff_list.append(diff)
                        lat1,lon1,lat2,lon2=df.at[i,'latitude'],df.at[i,'longitude'],df.at[j,'latitude'],df.at[j,'longitude']
                        harv_list.append(haversine(lat1,lon1,lat2,lon2))
                        title_i.append(df.at[i,'title'])
                        title_j.append(df.at[j,'title'])
                    #if ratio4 ==100 :
                        #print ("index "+str(i)+" VS "+str(j)+" Similarity Ratio: "+str(ratio4))
                        #print( df_num[i][1]+" **** "+ df_num[j][1])
                       # print ("different price is: "+ str  (diff))
                        #with open('same_output.txt', 'w') as out:
                               #out.write(cap.stdout)
                        #df['Same_Info'][i]=df['Same_Info'][i]+" "+str(j)+" difference:"+str(diff)+" "
    df_2=pd.DataFrame({'Index_i': index_i,
        'lndex_j': index_j,
        'Title_Similarity_Ratio': ratio_greater_85,
        'Price_Difference':diff_list,
        'Location_Haversine_Distance':harv_list,
        'Title_i':title_i,
        "Title_j":title_j,})
    return df_2
                        
def get_fuzzy_dup_indices(listing):
    # Read rental listing csv into a dataframe
    # df=pd.read_csv("./unique_listings.csv")
    df = listing

    # Do clustering based on lat/long (location)
    X=df.loc[:,['latitude','longitude']]
    id_n=200
    kmeans = KMeans(n_clusters=id_n, random_state=0).fit(X)
    id_label=kmeans.labels_

    centers = kmeans.cluster_centers_

    # Assign "location group" based on clusters
    df['loc_grp']=id_label

    # Strip dataframe to just those columns that we need, then convert to numpy
    df_clean=df[['id','loc_grp','title']]
    df_num=df_clean[['loc_grp','title']].to_numpy()

    df['Duplicate_Info']=' '
    df['Same_Info']=' '

    df_2 = fuzz_token_set_ratio(df, df_num)

    # print("df_2.head(10) from python:")
    # print(df_2.head(10))
    return df_2

