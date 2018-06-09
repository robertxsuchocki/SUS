#!/usr/bin/env python
import os
import sys
import pandas as pd
from xgboost.sklearn import XGBRegressor

DIR = '/media/robert/My Passport/DM2018/'
TRAIN_FILE = '/home/robert/Code/SUS/zad3/data/train.txt'
TAG_DIR = '/home/robert/Code/SUS/zad3/data/tags/'
CSV_DIR = '/media/robert/My Passport/DM2018/csv/'

def predict_tags(tag):
    training_dataset = pd.read_csv(CSV_DIR + 'train_' + tag)
    training_dataset = pd.get_dummies(training_dataset)

    test_dataset = pd.read_csv(CSV_DIR + 'test_' + tag)
    test_dataset = pd.get_dummies(test_dataset)

    X_train = training_dataset.drop(['outcome'], axis=1)
    y_train = training_dataset['outcome']

    X_test = test_dataset

    xgbcl = XGBRegressor()
    xgbcl.fit(X_train, y_train)

    return(list(xgbcl.predict(X_test)))

with open(TRAIN_FILE, 'r') as train:
    text = train.read().replace('\r\n', ',\r\n')

files = os.listdir(TAG_DIR)
length = len(files)
predicts = {}
for i in range(100000):
    predicts[i] = list()
for i, file in enumerate(files):
    tag = file[:-4]
    result = predict_tags(file)
    with open(TRAIN_FILE, 'r') as train:
        count = text.count(tag + ',') + 1
    limit = sorted(result)[-count]
    if limit < 0.0001:
        indexes = []
    else:
        indexes = [j for j, val in enumerate(result) if val > limit]
    for index in indexes:
        predicts[index].append(tag)
    percent = (i + 1) * 100 / length
    status = str(i + 1) + '/' + str(length) + ' - ' + str(percent) + '%\r'
    sys.stdout.write('%s\r' % status)
    sys.stdout.flush()
sys.stdout.write('\n')
sys.stdout.flush()

with open(DIR + 'result.txt', 'w') as file:
    for i in range(100000):
        file.write(','.join(predicts[i]) + '\n')
