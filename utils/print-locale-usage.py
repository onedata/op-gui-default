#!/usr/bin/env python
import re, sys, os

RE_JS = re.compile(r'.*\.js$')
RE_HBS = re.compile(r'.*\.hbs$')
RE_GET_I18N = re.compile(r"get\(.i18n.\)\.t\(.([a-zA-Z\.]*).\)", re.I | re.M)
RE_HBS_T = re.compile(r"[\{\(][\{\(]t\s.([a-zA-Z\.]*).[\}\)][\}\)]", re.I | re.M)

all_usage = []

def ffind(re_file, re_t):
    for folder, subfolders, files in os.walk(os.getcwd() + '/../src/app'):
        for fname in filter(lambda f: re_file.match(f), files):
            filePath = os.path.join(os.path.abspath(folder), fname)
            # print filePath
            with open(filePath) as f:
                all_usage.extend(re_t.findall(f.read()))

# ffind(RE_JS, RE_GET_I18N)
ffind(RE_HBS, RE_HBS_T)

import json
locales = json.loads(open('flatted.txt').read())['keys']

# print locales
# all_usage = map(unicode, all_usage)
# print all_usage

status = 0

for u in all_usage:
    if u not in locales:
        print u, 'not found'
        status = 1

sys.exit(status)
