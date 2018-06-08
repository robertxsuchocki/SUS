#!/usr/bin/env python
from collections import Counter
import re
import os
import sys

TEST_DIR = './DM2018_test/'
TRAIN_DIR = './DM2018_train/'

TAG_DIR = './DM2018_tags/'

tag_counters = {}
files = os.listdir(TRAIN_DIR)
length = len(files)
for i, file in enumerate(files):
    with open(TRAIN_DIR + file, 'r') as file:
        lines = [line[:-1] for line in file.readlines()]
        tags = lines[0].split(',')
        for tag in tags:
            for line in lines[1:]:
                count = int(line.split(',')[0])
                word = line.split(',')[1]
                if tag not in tag_counters:
                    tag_counters[tag] = Counter()
                tag_counters[tag].update({word: count})
    percent = (i + 1) * 100 / length
    status = str(i + 1) + '/' + str(length) + ' - ' + str(percent) + '%\r'
    sys.stdout.write('%s\r' % status)
    sys.stdout.flush()
sys.stdout.write('\n')
sys.stdout.flush()

length = len(tag_counters)
for tag in tag_counters:
    with open(TAG_DIR + tag + '.txt', 'w') as file:
        counter = Counter(tag_counters[tag])
        for key, value in counter.most_common():
            file.write(str(value) + ',' + str(key) + '\n')
