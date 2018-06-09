#!/usr/bin/env python
import re
import os
import sys

CSV_DIR = '/media/robert/My Passport/DM2018/csv/'
SHORT_DIR = '/media/robert/My Passport/DM2018/csv_short/'


files = os.listdir(CSV_DIR)
length = len(files)
for i, file_name in enumerate(files):
    with open(CSV_DIR + file_name, 'r') as csv:
        csv_lines = csv.readlines()
        with open(SHORT_DIR + file_name, 'w') as short:
            columns = "1,2,3,4,5\n" if len(csv_lines[0].split(',')) == 50 else "1,2,3,4,5,outcome\n"
            short.write(columns)
            for line in csv_lines[1:]:
                numbers = [int(num) for num in line.split(',')]
                sums = [sum(numbers[j*10:(j+1)*10]) for j in range(0,((len(numbers)-1)/10)+1)]
                short.write(','.join(map(str, sums)) + '\n')
    percent = (i + 1) * 100 / length
    status = str(i + 1) + '/' + str(length) + ' - ' + str(percent) + '%\r'
    sys.stdout.write('%s\r' % status)
    sys.stdout.flush()
sys.stdout.write('\n')
sys.stdout.flush()
