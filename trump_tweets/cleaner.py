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
    
    url_rm = re.compile('http[^\s]*')
    period_spacer = re.compile('\.')
    quote_rm = re.compile('\"')
    
    outfile = open(ofn,'a') 
    for elem in data:
        temp = url_rm.sub(' ', elem['text']).strip()
        temp = period_spacer.sub(' .', temp).strip()
        temp = quote_rm.sub('', temp).strip()

        output = html.unescape(temp)
        outfile.write(output)
        outfile.write('\n')
    
    outfile.close()
    
