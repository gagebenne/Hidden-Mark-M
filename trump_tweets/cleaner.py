import json
import re

years = list(range(2009,2019))

for yr in years:

    ifn = 'condensed_' + str(yr) + '.json'
    with open(ifn) as f:
        data = json.load(f)
        f.close()
    
    data = [ elem for elem in data if not elem['is_retweet'] ]
    
    rx = re.compile('http[^\s]*')
    
    ofn = str(yr) + '.tweets'
    outfile = open(ofn,'a') 
    for elem in data:
        res = rx.sub(' ', elem['text']).strip()
        outfile.write(res)
        outfile.write('\n')
    
    outfile.close()
    
