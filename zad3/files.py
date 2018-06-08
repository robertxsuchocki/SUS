#!/usr/bin/env python
from collections import Counter
import re
import os
import sys

IGNORED = ['the', 'and', 'a', 'that', 'I', 'it', 'not', 'he', 'as', 'you', 'this', 'but', 'his', 'they', 'her', 'she', 'or', 'an', 'will', 'my', 'would', 'there', 'their', 'to', 'of', 'in', 'for', 'on', 'with', 'at', 'by', 'from', 'up', 'about', 'into', 'over', 'after', 'be', 'have', 'do', 's', 'is', 'we', 'are', 'can', 'which', 'our', 'has', 'its', 'was', 'were']

TEST = {
    'file' : 'DM2018_test_docs.txt',
    'folder' : './DM2018_test/',
    'tags': False
}

TRAIN = {
    'file' : 'DM2018_training_docs_and_labels.txt',
    'folder' : './DM2018_train/',
    'tags': True
}

options = {
    'test': TEST,
    'train': TRAIN
}

def prep_word(word):
    word = word.strip()
    if word.istitle():
        word = word.lower()
    return word

def prep_text(input):
    words = re.sub('[^\w]', ' ', re.sub('<[^>]*>', '', input)).split()
    words = map(prep_word, words)
    words = [word for word in words if word not in IGNORED]
    return words

def create_files(dict):
    with open(dict['file'], 'r') as files:
        lines = files.readlines()
        length = len(lines)
        for i, line in enumerate(lines):
            splitted = line.split('\t')
            with open(dict['folder'] + splitted[0], 'w') as file:
                if dict['tags']:
                    file.write(str(i) + ',')
                    file.write(','.join(splitted[2].split(',')[:-1]) + '\n')
                else:
                    file.write(str(i) + '\n')
                counter = Counter(prep_text(splitted[1]))
                for key, value in counter.most_common():
                    file.write(str(value) + ',' + str(key) + '\n')
            percent = (i + 1) * 100 / length
            status = str(i + 1) + '/' + str(length) + ' - ' + str(percent) + '%\r'
            sys.stdout.write('%s\r' % status)
            sys.stdout.flush()
    sys.stdout.write('\n')
    sys.stdout.flush()

if len(sys.argv) == 2 and sys.argv[1] in options:
    create_files(options[sys.argv[1]])
else:
    create_files(TEST)
    create_files(TRAIN)
