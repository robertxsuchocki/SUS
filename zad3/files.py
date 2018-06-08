#!/usr/bin/env python
import en
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
    if en.is_noun(word):
        word = en.noun.singular(word)
    if en.is_verb(word):
        word = en.verb.infinitive(word)
    return word

def prep_text(input):
    words = re.sub('[^\w]', ' ', re.sub('<[^>]*>', '', input)).split()
    words = map(prep_word, words)
    words = [word for word in words if word not in IGNORED]
    return ' '.join(words)

def create_files(dict):
    with open(dict['file'], 'r') as files:
        lines = files.readlines()
        length = len(lines)
        for i, line in enumerate(lines):
            splitted = line.split('\t')
            with open(dict['folder'] + splitted[0], 'w') as file:
                file.write(str(i) + '\n')
                if dict['tags']:
                    for tag in splitted[2].split(',')[:-1]:
                        file.write(tag + '\n')
                file.write(prep_text(splitted[1]) + '\n')
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
