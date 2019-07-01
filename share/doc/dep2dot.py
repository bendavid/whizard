#!/usr/bin/env python
# Creates dot from Makefile.depend files
import os
import argparse
import re

dot_head = '''
digraph G {
'''
dot_foot = ''' }
'''

parser = argparse.ArgumentParser(
    description='Build dependency graphs',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('files', help='The name(s) of the Makefile.depend(s)',
                    nargs='+')
args = parser.parse_args()


def transform(strg):
    strg = strg.replace('-', '_')
    strg = strg.replace('.lo:', ' ->')
    strg = strg.replace('.lo', '')
    strg = strg.replace('../', '')
    return strg


def transform_external(strg, this_dir):
    strg = re.sub(r".* ->", this_dir.replace('-', '_') + ' ->', strg)
    strg = re.sub(r"/\w*", '', strg)
    return strg


def deps_of_file(filename):
    this_dir = os.path.basename(os.path.dirname(filename))
    dot = open(filename, "r")
    dep_strgs = []
    for line in dot.readlines():
        if len(args.files) == 1:
            if "_ut" not in line and "/" not in line:
                dep_strgs.append(line)
        else:
            if "_ut" not in line and "/" in line:
                dep_strgs.append(line)
    dep_strgs = list(set(dep_strgs))
    if len(dep_strgs) == 0:
        dep_strgs.append('"No internal dependencies"')
    dep_strgs = map(transform, dep_strgs)
    if len(args.files) > 1:
        dep_strgs = map(lambda x: transform_external(x, this_dir), dep_strgs)
        dep_strgs = list(set(dep_strgs))
    return dep_strgs


results = []
for filename in args.files:
    results += (deps_of_file(filename))

dot_strg = dot_head + ''.join(results) + dot_foot
print dot_strg
