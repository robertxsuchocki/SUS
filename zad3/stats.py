#!/usr/bin/env python
from collections import Counter
import re
import os
import sys

TEST_DIR = '/home/robert/Code/SUS/zad3/data/test/'
TRAIN_DIR = '/home/robert/Code/SUS/zad3/data/train/'

TAG_DIR = '/home/robert/Code/SUS/zad3/data/tags/'

CSV_DIR = '/media/robert/My Passport/DM2018/csv/'

def gen_ranks(words, lines):
    ranks = []
    for word in words:
        for line in lines:
            if word == line[1]:
                ranks.append(line[0])
                break
        else:
            ranks.append('0')
    return ranks


files = os.listdir(TAG_DIR)
length = len(files)
for i, file_name in enumerate(files):
    with open(TAG_DIR + file_name, 'r') as file:
        words = [line[:-1].split(',')[1] for line in file.readlines()[:50]]
        with open(CSV_DIR + 'train_' + file_name, 'w') as csv:
            csv.write(','.join(words) + ',outcome\n')
            for j in range(1, 100001):
                with open(TRAIN_DIR + str(j) + '.txt', 'r') as train:
                    lines_train = train.readlines()
                    lines = [line[:-1].split(',') for line in lines_train[1:]]
                    ranks = gen_ranks(words, lines)
                    outcome = str(int(file_name[:-4] in lines_train[0][:-1].split(',')))
                    csv.write(','.join(ranks + [outcome]) + '\n')
        with open(CSV_DIR + 'test_' + file_name, 'w') as csv:
            csv.write(','.join(words) + '\n')
            for j in range(1, 100001):
                with open(TEST_DIR + str(j) + '.txt', 'r') as test:
                    lines = [line[:-1].split(',') for line in test.readlines()]
                    ranks = gen_ranks(words, lines)
                    csv.write(','.join(ranks) + '\n')
    percent = (i + 1) * 100 / length
    status = str(i + 1) + '/' + str(length) + ' - ' + str(percent) + '%\r'
    sys.stdout.write('%s\r' % status)
    sys.stdout.flush()
sys.stdout.write('\n')
sys.stdout.flush()
