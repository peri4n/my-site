---
layout: post
title: "Retrieve all tickers of the S&P 500 index"
author: Fabian Bull
categories: [python, finance]
---

Recently, I got interested into quantitive finance. To get started with analyzing financial data, I wanted historical data of the [S&P 500 Index][sp500].
As this index contains 500 companies, it would be quite cumbersome to maintain all tickers by hand, I hacked a small script to retrieve all tickers using Wikipedia.

# Scraping Wikipedia for the S&P 500

The wikipedia page of the [S&P 500][sp500] contains a list of all tickers currently in the index. After having a look at the underlying HTML code, I came up with the following 
python script:

~~~~ {.python .numberLines}
import bs4 as bs
import requests

resp = requests.get('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
soup = bs.BeautifulSoup(resp.text, 'lxml')
table = soup.find('table', { 'class': 'wikitable sortable'})
for row in table.findAll('tr')[1:]:
    ticker = row.findAll('td')[0].text
    print(ticker)
~~~~

Which retrieves the first *table* and prints the text content of the first cell of every row, except the first one (Because is only contains the table header). 
If you like to have this information inside python you can append the tickers to a list and do some further analysis.

# Additional notes: The DAX 30

Just in case you have problems adopting this script to other pages, here is a script which scrapes the [DAX 30 index][dax30].

~~~~ {.python .numberLines}
import bs4 as bs
import requests

resp = requests.get('https://en.wikipedia.org/wiki/DAX')
soup = bs.BeautifulSoup(resp.text, 'lxml')
table = soup.find('table', { 'class': 'wikitable sortable'})
for row in table.findAll('tr')[1:]:
    ticker = row.findAll('td')[3].text
    print(ticker)
~~~~

This page has the tickers written on the fourth column.

[dax30]: https://en.wikipedia.org/wiki/DAX
[sp500]: https://en.wikipedia.org/wiki/S%26P_500_Index
