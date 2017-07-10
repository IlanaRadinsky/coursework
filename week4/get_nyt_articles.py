#
# file: get_nyt_articles.py
#
# description: fetches article urls from the NYTimes API
#
# usage: get_nyt_articles.py <api_key> <section_name> <num_articles>
#
# requirements: a NYTimes API key
#   available at https://developer.nytimes.com/signup
#

import requests
import json
import sys
import time

ARTICLE_SEARCH_URL = 'https://api.nytimes.com/svc/search/v2/articlesearch.json'

if __name__=='__main__':
   if len(sys.argv) != 4:
      sys.stderr.write('usage: %s <api_key> <section_name> <num_articles>\n' % sys.argv[0])
      sys.exit(1)

   api_key = sys.argv[1]
   section_name = sys.argv[2]
   num_articles = int(sys.argv[3])

   with open("articles_" + section_name + ".tsv", "w") as articles:
         articles.write("section_name\tweb_url\tpub_date\tsnippet\n")

   count = 0
   page = 0

   while count<num_articles:
      params = {
         'api-key': api_key,
         'fq': "section_name:\"{}\"".format(section_name),
         'page': page
      }

      r = requests.get(ARTICLE_SEARCH_URL, params)
      data = r.json()
      #data = json.loads(r.content)

      for doc in data['response']['docs']:
           #print(doc['web_url'])
         if count<num_articles:
            snippet = str(doc['snippet']).replace('\n', ' ')
            with open("articles_" + section_name + ".tsv", "a") as articles:
               articles.write(str(doc['section_name'])+"\t"+str(doc['web_url'])+"\t"+ str(doc['pub_date'])+"\t"+snippet+"\n")
            count += 1

      page += 1
      time.sleep(2)