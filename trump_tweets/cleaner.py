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
    apostrophe_standard = re.compile('’')
    dots_standard = re.compile('(\.\.\.+| … )')
    dot_at_stripper = re.compile('\.@')
    USA_stripper = re.compile('U\.S\.A\.')
    US_stripper = re.compile('U\.S\.')
    pm_stripper = re.compile('P\.M\.')
    am_stripper = re.compile('A\.M\.')
    dc_stripper = re.compile('D\.C\.')
    period_spacer = re.compile('\.')
    exclamation_spacer = re.compile('!')
    question_spacer = re.compile('\?')
    quote_rm = re.compile('\"|“|”')
    colon_spacer = re.compile(':')


    outfile = open(ofn,'a')
    for elem in data:
        temp = url_rm.sub(' ', elem['text']).strip()
        temp = apostrophe_standard.sub('\'', temp).strip()
        temp = dots_standard.sub(' … ', temp).strip()
        temp = dot_at_stripper.sub('@', temp).strip()
        temp = USA_stripper.sub(' USA ', temp).strip()
        temp = US_stripper.sub(' US ', temp).strip()
        temp = pm_stripper.sub(' PM ', temp).strip()
        temp = am_stripper.sub(' AM ', temp).strip()
        temp = dc_stripper.sub(' DC ', temp).strip()
        temp = period_spacer.sub(' . ', temp).strip()
        temp = exclamation_spacer.sub(' ! ', temp).strip()
        temp = question_spacer.sub(' ? ', temp).strip()
        temp = colon_spacer.sub(' : ', temp).strip()
        temp = quote_rm.sub('', temp).strip()

        output = html.unescape(temp)
        outfile.write(output)
        outfile.write('\n')

    outfile.close()
