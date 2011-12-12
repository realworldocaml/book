#!/usr/bin/env python

import glob, os, re

chapter_re = re.compile(r'<(chapter|appendix|preface)\s+id="([^"]+)">')
filename_re = re.compile(r'<\?dbhtml filename="([^"]+)"\?>')
title_re = re.compile(r'<title>(.*)</title>')

chapters = (sorted(glob.glob('../en/ch*.xml')) +
            sorted(glob.glob('../en/app*.xml')))

fp = open('index-read.html.in', 'w')

print >> fp, '''<!-- -*- html -*- -->
{% extends "boilerplate.html" %}
{% block bodycontent %}
<div class="navheader"><h1 class="booktitle">Mercurial: The Definitive Guide<div class="authors">by Bryan O'Sullivan</div></h1></div>
<div class="book"><ul class="booktoc">'''

ch = 0
app = 0
ab = 0
for c in chapters:
    filename = None
    title = None
    chapid = None
    chaptype = None
    for line in open(c):
        m = chapter_re.search(line)
        if m:
            chaptype, chapid = m.groups()
        m = filename_re.search(line)
        if m:
            filename = m.group(1)
        m = title_re.search(line)
        if m:
            title = m.group(1)
        if filename and title and chapid:
            if chaptype == 'appendix':
                num = chr(ord('A') + app)
                app += 1
            else:
                num = ch
                ch += 1
            ab += 1
            date = os.popen('hg log -l1 --template "{date|isodate}" ' + c).read().split(None, 1)[0]
            args = {
                'ab': "ab"[ab % 2],
                'date': date,
                'chapid': chapid,
                'num': num,
                'filename': filename,
                'title': title,
                }
            print >> fp, '<li class="zebra_%(ab)s"><span class="chapinfo">%(date)s<a href="/feeds/comments/%(chapid)s/"><img src="/support/figs/rss.png"/></a></span>%(num)s. <a href="%(filename)s">%(title)s</a></li>' % args
            break

print >> fp, '''</ul></div>
{% endblock %}'''

fp.close()
