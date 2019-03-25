import json
import re
import os
import html

years = list(range(2009,2019))

for yr in years:

    ifn = 'condensed_' + str(yr) + '.json'
    ofn = str(yr) + '.tweets'

    if os.path.isfile(ofn):
        os.remove(ofn)

    with open(ifn) as f:
        data = json.load(f)
        f.close()
    
    data = [ elem for elem in data if not elem['is_retweet'] ]
    
    rx = re.compile('http[^\s]*')
    
    outfile = open(ofn,'a') 
    for elem in data:
        res = rx.sub(' ', elem['text']).strip()
        output = html.unescape(res)
        outfile.write(output)
        outfile.write('\n')
    
    outfile.close()
    
