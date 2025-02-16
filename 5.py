# -*- coding: utf-8 -*-
"""
Created on Tue Dec 19 14:22:49 2023

@author: HP
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from math import radians, cos, sin, asin, sqrt
#from datetime import * 没用到
import seaborn as sns  # 基于matplotlib的库，可以画图
from sklearn.cluster import KMeans  # kmeans聚类
from sklearn.preprocessing import StandardScaler  # 实现数据标准化
#from sklearn.metrics import silhouette_score
#from sklearn.metrics import calinski_harabasz_score
from scipy.spatial.distance import cdist  # 用于计算两个输入集合之间的距离。
# 读取数据
#df = pd.read_excel(r"E:\Program Files\TDA\task5\obike_1.xlsx",index_col=False)
df = pd.read_excel(r'C:\Users\HP\OneDrive - tongji.edu.cn\作业\大二上\交通运输数据技术\task5\obike_1.xlsx',index_col = False)
print('预处理前的数据量为：',len(df))

# 利用球面余弦定理和Haversin定理来计算地球上任意两点之间的最短距离
def haversine(lon1, lat1, lon2, lat2):
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])  # 把经纬度转化为弧度制不然没法算
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2*asin(sqrt(a))
    r = 6371  # 地球半径6371km
    return c*r
df['distance']=df.apply(lambda row: haversine(row['olgt'],row['olat'],row['dlgt'],row['dlat']), axis=1)
# 删除速度异常数据(阈值为25)
def get_hour(a):
    h = a.hour + a.minute/60 + a.second/3600
    return h
df['time']=df.apply(lambda row: get_hour(row['time']), axis=1)
df['speed']=df['distance']/df['time']
new_df=df[df['speed']<=25]
print('预处理后的数据量为：',len(new_df))
# 提取O特征，并可视化
df_d=new_df[['olgt','olat']]
df_d5=new_df.copy()
df_d7=new_df.copy()
df_d9=new_df.copy()
sns.set(style='white', rc={'figure.figsize':(10,8)})
sns.jointplot(x='olgt', y='olat', data=new_df, kind='hist',color='mediumorchid')
plt.show()

# (1)通过欧氏距离的相似度来聚类
X = np.array(df_d[['olgt','olat']])
n_clusters = 3 # 簇的个数为3
np.random.seed(0) # 随机选择初始的质心
centers = X[np.random.choice(range(len(X)), n_clusters, replace = False)]
while True:
    distances = np.linalg.norm(X[:,np.newaxis] - centers, axis = -1)  # 计算欧几里得距离
    labels = np.argmin(distances, axis=1)  # 离谁近就归哪一类
    new_centers = np.array([X[labels == i].mean(axis=0) for i in range(n_clusters)])
    if np.all(centers == new_centers):
        break
    centers = new_centers.copy()
df_d['cluster3'] = labels
sns.set(style = 'white', rc = {'figure.figsize':(10,8)})
sns.scatterplot(x='olgt', y='olat', hue='cluster3', data=df_d, palette='muted')
plt.title('Eculid K-means 3 Cluster', fontsize=30)
plt.show()

#(2)通过Manhattan距离来进行聚类
X = np.array(df_d[['olgt','olat']])
n_clusters = 3  # 簇的个数为3
np.random.seed(0)
centers = X[np.random.choice(range(len(X)), n_clusters, replace = False)]
while True:
    distances = np.sum(np.abs(X[:,np.newaxis] - centers), axis = -1)  # abs,不难发现是曼哈顿距离
    labels = np.argmin(distances, axis=1)
    new_centers = np.array([X[labels == i].mean(axis=0) for i in range(n_clusters)])
    if np.all(centers == new_centers):
        break
    centers = new_centers.copy()
df_d['cluster3'] = labels
sns.set(style = 'white', rc = {'figure.figsize':(10,8)})
sns.scatterplot(x='olgt', y='olat', hue='cluster3', data=df_d, palette='muted')
plt.title('Manhatton K-means 3 Cluster', fontsize=30)
plt.show()  # 真的，R语言一直报错说只有经纬度的一个新数据集的维度不对，python一下就画出来了、（感慨
 
# 结果评估
# (1)用轮廓系数评价聚类效果(自己编写的)
def my_silhouette_score(X, labels):
    n_samples, _ = X.shape
    n_labels = len(np.unique(labels))
    a = np.zeros(n_samples)
    b = np.zeros(n_samples)
    for i in range(n_samples):
        a[i] = np.mean(cdist(X[labels == labels[i]], [X[i]]))   #计算a[i]
    for i in range(n_samples):  # 这里算的是b[i]
        b[i] = np.min([np.mean(cdist(X[labels == k], [X[i]])) for k in np.unique(labels) if k != labels[i]])    
        score = np.mean((b - a) / np.maximum(a, b))
    return score

k_list1 = []
silhouette_index = []
for i in range (2,9):
    kmeans = KMeans(n_clusters = i)
    clusters = kmeans.fit_predict(df_d)
    effect = df_d[['olgt', 'olat']].values
    effect = StandardScaler().fit_transform(effect)  # 这部分是要获取聚类的结果
    clusters = kmeans.fit_predict(df_d)
    silhouette_avg = my_silhouette_score(effect, clusters)
    k_list1.append(i)
    silhouette_index.append(silhouette_avg)
plt.plot(k_list1,silhouette_index)
plt.title('silhouette index change(2=<k<=8)')
plt.show()
#print("The average silhouette_score for 3 is :", silhouette_avg3)

# (2)用Calinski-Harabaz指数来评价(自己编写的)  
def my_calinski_harabasz(X, labels):
    n_samples, _ = X.shape
    n_labels = len(np.unique(labels))
    extra_cov, intra_cov = 0., 0.
    mean = np.mean(X, axis=0)
    for k in range(n_labels):
        cluster_k = X[labels == k]
        mean_k = np.mean(cluster_k, axis=0)
        extra_cov += len(cluster_k) * np.sum((mean_k - mean)**2)  #类间协方差
        intra_cov += np.sum((cluster_k - mean_k)**2)  # 类内协方差
    score = (1. if intra_cov == 0. else extra_cov * (n_samples - n_labels) /(intra_cov * (n_labels - 1.)))      # 计算Calinski-Harabasz系数
    return score

k_list2 = []
ch_index = []
for i in range(2,9):
    kmeans = KMeans(n_clusters = i)
    clusters = kmeans.fit_predict(df_d)
    effect = df_d[['olgt', 'olat']].values
    effect = StandardScaler().fit_transform(effect)
    clusters = kmeans.fit_predict(df_d)
    calinski_harabasz_avg = my_calinski_harabasz(effect, clusters)
    k_list2.append(i)
    ch_index.append(calinski_harabasz_avg)
plt.plot(k_list2,ch_index)  # 系数的变化范围和趋势
plt.title('calinski harabasz index change(2<=k<=8)')
plt.show()
#print("The average Calinski-Harabasz score for 3 is :", calinski_harabasz_avg3)


# 以下是根据calinski harabasz指数的变化情况，绘制的k = 5时候的情况
# (1)通过欧氏距离的相似度来聚类
X = np.array(df_d[['olgt','olat']])
n_clusters = 5 # 簇的个数为3
np.random.seed(0) # 随机选择初始的质心
centers = X[np.random.choice(range(len(X)), n_clusters, replace = False)]
while True:
    distances = np.linalg.norm(X[:,np.newaxis] - centers, axis = -1)  # 计算欧几里得距离
    labels = np.argmin(distances, axis=1)  # 离谁近就归哪一类
    new_centers = np.array([X[labels == i].mean(axis=0) for i in range(n_clusters)])
    if np.all(centers == new_centers):
        break
    centers = new_centers.copy()
df_d['cluster5'] = labels
sns.set(style = 'white', rc = {'figure.figsize':(10,8)})
sns.scatterplot(x='olgt', y='olat', hue='cluster5', data=df_d, palette='muted')
plt.title('Eculid K-means 5 Cluster', fontsize=30)
plt.show()

#(2)通过Manhattan距离来进行聚类
X = np.array(df_d[['olgt','olat']])
n_clusters = 5  # 簇的个数为3
np.random.seed(0)
centers = X[np.random.choice(range(len(X)), n_clusters, replace = False)]
while True:
    distances = np.sum(np.abs(X[:,np.newaxis] - centers), axis = -1)  # abs,不难发现是曼哈顿距离
    labels = np.argmin(distances, axis=1)
    new_centers = np.array([X[labels == i].mean(axis=0) for i in range(n_clusters)])
    if np.all(centers == new_centers):
        break
    centers = new_centers.copy()
df_d['cluster5'] = labels
sns.set(style = 'white', rc = {'figure.figsize':(10,8)})
sns.scatterplot(x='olgt', y='olat', hue='cluster5', data=df_d, palette='muted')
plt.title('Manhatton K-means 5 Cluster', fontsize=30)
plt.show()

   