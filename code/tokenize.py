#!/usr/bin/env python
## python map for word counts

# Import Modules
import sys
import re
import json
import codecs
# from collections import Counter # not in py<2.7

# all non alphanumeric
symbols = re.compile(r'(\W+)', re.U)
# pure numeric
numeric = re.compile(r'(?<=\s)(\d+|\w\d+|\d+\w)(?=\s)', re.I|re.U)
# stop words
swrd = re.compile(r'(?<=\s)(d|re|m|ve|s|n|to|a|the|an|and|or|in|at|with|for|are|is|the|if|of|at|but|and|or)(?=\s)', re.I|re.U)
# suffix strip
suffix = re.compile(r'(?<=\w)(s|ings*|ives*|ly|led*|i*ed|i*es|ers*)(?=\s)')
# separators (any whitespace)
seps = re.compile(r'\s+')

# cleaner (order matters)
def clean(text): 
    text = u' ' +  text.lower() + u' '
    text = symbols.sub(r' \1 ', text)
    text = numeric.sub(' ', text)
    text = swrd.sub(' ', text)
    text = suffix.sub('', text)
    text = seps.sub(' ', text)
    return text


vout = codecs.open("data/yelp_rev_attr.txt", 'w', encoding='utf-8')
xout = codecs.open("data/yelp_rev_text.txt", 'w', encoding='utf-8')
fin = open("data/yelp_training_set/yelp_training_set_review.json", 'r')
i = 0

for line in fin:
    d = json.loads(line)
    i += 1
    try:
        vout.write(u"{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}\t{7}\t{8}\n".format(
            i,
            d['review_id'],
            d['user_id'],
            d['business_id'],
            d['date'],
            d['stars'],
            d['votes']['funny'],
            d['votes']['useful'],
            d['votes']['cool']))
        txt = clean(d['text'])
        tkns = [ w for w in txt.split() if len(w) > 2 ] # only > 2 letter words
        counter = tkns.count # minor speedup
        tkncnt = dict((w,counter(w)) for w in set(tkns))

        for w in tkncnt:
            try:
                xout.write(u"{0}\t{1}\t{2}\n".format(i,w,tkncnt[w]))
            except UnicodeEncodeError, e:
                print 'error in write'
                sys.stderr.write(str(e))
                pass

        print i

    except:
        e = sys.exc_info()[0]
        sys.stderr.write("review reader error: %s\n"%str(e))


fin.close()
vout.close()
xout.close()


vout = codecs.open("data/yelp_usr.txt", 'w', encoding='utf-8')
fin = open("data/yelp_training_set/yelp_training_set_user.json", 'r')
for line in fin:
    d = json.loads(line)
    try:
        vout.write(u"{0}\t{1}\t{2}\t{3}\t{4}\t{5}\n".format(
            d['user_id'],
            d['average_stars'],
            d['review_count'],
            d['votes']['funny'],
            d['votes']['useful'],
            d['votes']['cool']))
        print d['name']
    except:
        e = sys.exc_info()[0]
        sys.stderr.write("user reader error: %s\n"%str(e))

vout.close()
fin.close()


vout = codecs.open("data/yelp_biz_attr.txt", 'w', encoding='utf-8')
xout = codecs.open("data/yelp_biz_cats.txt", 'w', encoding='utf-8')
fin = open("data/yelp_training_set/yelp_training_set_business.json", 'r')
i = 0

for line in fin:
    d = json.loads(line)
    i+=1
    try:
        vout.write(u"{0}\t{1}\t{2}\t{3}\t{4}\t{5}\n".format(
            i,
            d['business_id'],
            d['city'],
            d['state'],
            d['stars'],
            d['review_count']))

        for c in d['categories']:
            xout.write(u"{0}\t{1}\n".format(i,c))
        print d['city']
    except:
        e = sys.exc_info()[0]
        sys.stderr.write("user reader error: %s\n"%str(e))


vout.close()
xout.close()
fin.close()
