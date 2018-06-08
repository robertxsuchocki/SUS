#!/usr/bin/env python
from collections import Counter
import re
import os
import sys

TEST_DIR = './DM2018_test/'
TRAIN_DIR = './DM2018_train/'

DIRS = [TEST_DIR, TRAIN_DIR]

word_list = []
for dir in DIRS:
    for file in os.listdir(dir):
        with open(dir + file, 'r') as file:
            lines = [line[:-1] for line in file.readlines()]
            index = lines[0]
            tags = lines[1:-1]
            words = lines[-1].split()
            word_list.extend(words)

counter = Counter(word_list)
print(counter)
