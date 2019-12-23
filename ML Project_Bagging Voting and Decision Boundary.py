#!/usr/bin/env python
# coding: utf-8

# Common imports
import numpy as np
import pandas as pd
pd.set_option('display.max_columns', 500)
pd.set_option('display.max_rows', 500)
np.set_printoptions(threshold=500)

import csv

#ret = pd.read_csv('Ret_5_day_eps.csv', sep=",", header=0)
ret = pd.read_csv('F:/UCLA/2019 Spring/431-2  Data Analytics and Machine Learning/ML final project/topic2/wohewozuihoudejuejiang.csv', sep=",", header=0)
ret.head()

ret = ret.drop(ret.columns[0:1], axis=1)


ret.dropna(inplace=True)


data_dummy = pd.get_dummies(ret['fqtr'], prefix = 'Q')


data = pd.concat([ret, data_dummy], axis=1)


data['logME'] = np.log(data['LaggedME'])


data['posret'] = np.where(data['Ret60DayAfter']>0,1,0)
data['posretprev'] = np.where(data['Ret5DayBefore']>0,1,0)

data['PR'] = (np.log(99*10**9) - data['logME'])/(np.log(99*10**9) - np.log(10**6))

from sklearn.linear_model import LogisticRegression


insample = data[data['fyearq'] <= 2015]
outsample = data[data['fyearq'] >= 2016]



from sklearn.linear_model import LinearRegression



x_train = insample[['ifPositiveDrift_Lag','Q_1','Q_2','Q_3','Q_4','Past30dayVol','PastMktVol3Month','Past10dayVol','Ret5DayBefore','PR','Ret1DayBefore','Past200SK','Past200Kur']]
#x_train = insample[['Q_1','Q_2','Q_3','Q_4']]

y_train = insample['posret']


from sklearn.metrics import r2_score

x_test = outsample[['ifPositiveDrift_Lag','Q_1','Q_2','Q_3','Q_4','Past30dayVol','PastMktVol3Month','Past10dayVol','Ret5DayBefore','PR','Ret1DayBefore','Past200SK','Past200Kur']]
#x_test = outsample[['Q_1','Q_2','Q_3','Q_4']]
y_test = outsample['posret']




from sklearn.metrics import f1_score

from sklearn.metrics import accuracy_score

from sklearn.metrics import classification_report


from sklearn.model_selection import cross_val_score


### voting


# In[93]:

from sklearn.ensemble import VotingClassifier

log_clf = LogisticRegression(random_state=42)
rnd_clf = RandomForestClassifier(random_state=42)

voting_clf = VotingClassifier(
    estimators=[('lr', log_clf), ('rf', rnd_clf)],
    voting='hard')

voting_clf2 = VotingClassifier(
    estimators=[('lr', log_clf), ('rf', rnd_clf)],
    voting='soft')

voting_clf.fit(x_train, y_train)

for clf in (log_clf, rnd_clf, voting_clf,voting_clf2):
    clf.fit(x_train, y_train)
    y_pred = clf.predict(x_test)
    print(clf.__class__.__name__, accuracy_score(y_test, y_pred))
    print(clf.__class__.__name__, f1_score(y_test, y_pred))


# In[96]:

# Bagging
# Get some classifiers to evaluate


from sklearn.model_selection import cross_val_score
from sklearn.ensemble import BaggingClassifier, ExtraTreesClassifier, RandomForestClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import cross_val_score

X = data[['ifPositiveDrift_Lag','Q_1','Q_2','Q_3','Q_4','Past30dayVol','PastMktVol3Month',\
          'Past10dayVol','Ret5DayBefore','PR','Ret1DayBefore','Past200SK','Past200Kur']]

y = data['posret']

seed = 42
# Create classifiers
rf = RandomForestClassifier()
et = ExtraTreesClassifier()
knn = KNeighborsClassifier()
lg = LogisticRegression()
clf_array = [rf, et, knn, lg]

for clf in clf_array:
    vanilla_scores = cross_val_score(clf, x_train, y_train, cv=10, n_jobs=-1)
    clf.fit(x_train, y_train)
    y_pred = clf.predict(x_test)
    fscore=f1_score(y_test,y_pred)
    bagging_clf = BaggingClassifier(clf, max_samples=0.4, max_features=10, random_state=seed)
    bagging_scores = cross_val_score(bagging_clf, x_train, y_train, cv=10, n_jobs=-1)
    bagging_clf.fit(x_train,y_train)
    y_pred2 = bagging_clf.predict(x_test)
    fscore2=f1_score(y_test,y_pred2)
    
    print ("Mean: {1:.3f}, std: (+/-) {2:.3f} [{0}]".format(clf.__class__.__name__,\
           vanilla_scores.mean(), vanilla_scores.std()))
    print ("Mean: {1:.3f}, std: (+/-) {2:.3f} [Bagging {0}]" .format(clf.__class__.__name__,\
           bagging_scores.mean(), bagging_scores.std()))
    print ("f1_score: {1:.3f} [{0}]".format(clf.__class__.__name__, fscore))
    print ("f1_score: {1:.3f} [Bagging {0}]\n".format(clf.__class__.__name__, fscore2))

# In[ ]:
    
# hard/soft voting
    
import warnings
warnings.filterwarnings("ignore")    

from sklearn.ensemble import VotingClassifier
eclf = VotingClassifier(estimators=[('Random Forests', rf), ('Extra Trees', et), \
                                    ('KNeighbors', knn),('Logistic Classifier', lg)], voting='hard')
eclf2 = VotingClassifier(estimators=[('Random Forests', rf), ('Extra Trees', et), \
                                     ('KNeighbors', knn), ('Logistic Classifier', lg)], voting='soft')
for clf, label in zip([eclf,eclf2], ['Voting Hard','Voting Soft']):
    scores = cross_val_score(clf, x_train, y_train, cv=10, scoring='accuracy')
    clf.fit(x_train,y_train)
    y_pred = clf.predict(x_test)
    fscore=f1_score(y_test,y_pred)
    
    print("Mean: {1:.3f}, std: (+/-) {2:.3f} [{0}]" .format(label,scores.mean(), scores.std()))
    print ("f1_score: {1:.3f} [{0}]\n".format(label, fscore))

# Decision Boundaries

import matplotlib.pyplot as plt
from mlxtend.plotting import plot_decision_regions
import matplotlib.gridspec as gridspec
import itertools

gs = gridspec.GridSpec(3, 3)
fig = plt.figure(figsize=(14, 12))
labels = ['Random Forest', 'Extra Trees', 'KNN', 'Logistic', 'Voting Hard','Voting Soft']
for clf, lab, grd in zip([rf, et, knn, lg, eclf,eclf2], labels, \
                         itertools.product([0, 1, 2], repeat = 2)):
    clf.fit(x_train[['Ret5DayBefore', 'Past200SK']], y_train)
    ax = plt.subplot(gs[grd[0], grd[1]])
    fig = plot_decision_regions(X=np.array(x_train[['Ret5DayBefore', 'Past200SK']]), \
                                y=np.array(y_train), clf=clf)
    plt.title(lab)





