# Scraper



```python
# setup library imports
import io, time, json
import requests
from pathlib import Path
from bs4 import BeautifulSoup
from testing.testing import test
```

## Library Documentation

* Standard Library: 
    * [io](https://docs.python.org/2/library/io.html)
    * [time](https://docs.python.org/2/library/time.html)
    * [json](https://docs.python.org/2/library/json.html)

* Third Party
    * [requests](http://docs.python-requests.org/en/master/)
    * [Beautiful Soup (version 4)](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
    * [yelp-fusion](https://www.yelp.com/developers/documentation/v3/get_started)

**Note:** You may come across a `yelp-python` library online. The library is deprecated and incompatible with the current Yelp API, so do not use the library.

## Introduction

Welcome to the homework on web scraping. While many people might view working with data (including scraping, parsing, storing, etc.) a necessary evil to get to the "fun" stuff (i.e. modeling), I think that if presented in the right way this munging can be quite empowering. Imagine you never had to worry or ask those _what if_ questions about data existing or being accessible... but that you can get it yourself!

By the end of this exercise hopefully you should look at the wonderful world wide web without fear, comforted by the fact that anything you can see with your human eyes, a computer can see with its computer eyes...
 
### Objectives

But more concretely, this homework will teach you (and test you on):

* HTTP Requests (and lifecycle)
* RESTful APIs
    * Authentication (OAuth)
    * Pagination
    * Rate limiting
* JSON vs. HTML (and how to parse each)
* HTML traversal (CSS selectors)

## Working with APIs

Since everyone loves food (presumably), the ultimate end goal of this homework will be to acquire the data to answer some questions and hypotheses about the restaurant scene in Pittsburgh (which we will get to later). We will download __both__ the metadata on restaurants in Pittsburgh from the Yelp API and with this metadata, retrieve the comments/reviews and ratings from users on restaurants.

But first things first, let's do the "hello world" of making web requests with Python to get a sense for how to programmatically access web pages: an (unauthenticated) HTTP GET to download a web page.

---

## Q0: Basic HTTP Requests

Fill in the funtion to use `requests` to download and return the raw HTML content of the URL passed in as an argument. As an example try the following NYT article (on Facebook's algorithmic news feed): [http://www.nytimes.com/2016/08/28/magazine/inside-facebooks-totally-insane-unintentionally-gigantic-hyperpartisan-political-media-machine.html](http://www.nytimes.com/2016/08/28/magazine/inside-facebooks-totally-insane-unintentionally-gigantic-hyperpartisan-political-media-machine.html)

> Your function should return a tuple of: (`<status_code>`, `<raw_html>`)

```python
>>> facebook_article = retrieve_html('http://www.nytimes.com/2016/08/28/magazine/inside-facebooks-totally-insane-unintentionally-gigantic-hyperpartisan-political-media-machine.html')
>>> print(facebook_article)
(200, u'<!DOCTYPE html>\n<!--[if (gt IE 9)|!(IE)]> <!--> <html lang="en" class="no-js section-magazine...')
```

```python
def retrieve_html_test(retrieve_html):
    status_code, text = retrieve_html("http://www.example.com")
    test.equal(status_code, 200)
    test.true("This domain is established to be used for illustrative examples in documents." in text)
    # Note that the text hash may change depending on the remote server. Feel free to change the test.

@test
def retrieve_html(url):
    """
    Return the raw HTML at the specified URL.

    Args:
        url (string): 

    Returns:
        status_code (integer):
        raw_html (string): the raw HTML content of the response, properly encoded according to the HTTP headers.
    """
    
    return 0, ""

```

---

Now while this example might have been fun, we haven't yet done anything more than we could with a web browser. To really see the power of programmatically making web requests we will need to interact with a API. For the rest of this homework we will be working with the [Yelp API](https://www.yelp.com/developers/documentation/v3/get_started) and Yelp data (for an extensive data dump see their [Academic Dataset Challenge](https://www.yelp.com/dataset_challenge)). The reasons for using the Yelp API are 3 fold:

1. Incredibly rich dataset that combines:
    * entity data (users and businesses)
    * preferences (i.e. ratings)
    * geographic data (business location and check-ins)
    * temporal data
    * text in the form of reviews
    * and even images.
2. Well [documented API](https://www.yelp.com/developers/documentation/v3/get_started) with thorough examples.
3. Extensive data coverage so that you can find data that you know personally (from your home town/city or account). This will help with understanding and interpreting your results.

## Authentication

To access the Yelp API however we will need to go through a few more steps than we did with the first NYT example. Most large web scale companies use a combination of authentication and rate limiting to control access to their data to ensure that everyone using it abides. The first step (even before we make any request) is to setup a Yelp account if you do not have one and get API credentials.

## Yelp API Access

1. Create a Yelp account (if you do not have one already)
2. [Generate API keys](https://www.yelp.com/developers/v3/manage_app) (if you haven't already). You will only need the API Key (not the Client ID or Client Secret) -- more on that later.


Now that we have our accounts setup we can start making requests! There are various authentication schemes that APIs use, listed here in relative order of complexity:

* No authentication
* [HTTP basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
* Cookie based user login
* OAuth (v1.0 & v2.0, see this [post](http://stackoverflow.com/questions/4113934/how-is-oauth-2-different-from-oauth-1) explaining the differences)
* API keys
* Custom Authentication

For the NYT example, since it is a publicly visible page we did not need to authenticate. HTTP basic authentication isn't too common for consumer sites/applications that have the concept of user accounts (like Facebook, LinkedIn, Twitter, etc.) but is simple to setup quickly and you often encounter it on with individual password protected pages/sites. I'm sure you have seen this before somewhere:

![http-basic](http://i.stack.imgur.com/QnUZW.png)

Cookie based user login is what the majority of services use when you login with a browser (i.e. username and password). Once you sign in to a service like Facebook, the response stores a cookie in your browser to remember that you have logged in (HTTP is stateless). Each subsequent request to the same domain (i.e. any page on `facebook.com`) also sends the cookie that contains the authentication information to remind Facebook's servers that you have already logged in.

Many REST APIs however use OAuth (authentication using tokens) which can be thought of a programmatic way to "login" _another_ user. Using tokens, a user (or application) only needs to send the login credentials once in the initial authentication and as a response from the server gets a special signed token. This signed token is then sent in future requests to the server (in place of the user credentials).

A similar concept common used by many APIs is to assign API Keys to each client that needs access to server resources. The client must then pass the API Key along with _every_ request it makes to the API to authenticate. This is because the server is typically relatively stateless and does not maintain a session between subsequent calls from the same client. Most APIs (including Yelp) allow you to pass the API Key via a special HTTP Header: `Authorization: Bearer <API_KEY>`. Check out the [docs](https://www.yelp.com/developers/documentation/v3/authentication) for more information.

Yelp used to use OAuth tokens but has now switched to API Keys. **For the sake of backwards compatibility Yelp still provides a Client ID and Secret for OAuth, but you will not need those for this assignment.** 

---


## Q1: Authenticated HTTP Request with the Yelp API

First, store your Yelp credentials in a local file which you can read in to authenticate with the API. This file can be any format/structure since you will fill in the function stub below.


**KEEP THE API KEY FILE PRIVATE AND OUT OF VERSION CONTROL**

Using the Yelp API, fill in the following function stub to make an authenticated request to the [search](https://www.yelp.com/developers/documentation/v3/business_search) endpoint.

> As a test, search for businesses in Pittsburgh. You should find ~13400 total depending on when you search (but this will actually differ from the number of actual Business objects returned... more on this in the next section)

When writing the python request, you'll need to pass in a custom header as well as a parameter. Here are some examples for [response headers](http://docs.python-requests.org/en/master/user/quickstart/#response-headers) and [passing parameters in urls](http://docs.python-requests.org/en/master/user/quickstart/#passing-parameters-in-urls)

```python
>>> api_key = read_api_key('api_key.txt')
>>> num_records, data = yelp_search(api_key, 'Pittsburgh')
>>> print(num_records)
2900
>>> print(list(map(lambda x: x['name'], data)))
['Gaucho Parrilla Argentina', 'Randyland', 'Redhawk Coffee', 'Phipps Conservatory and Botanical Gardens', 'La Gourmandine Bakery & Pastry Shop', ...]
```

```python
def read_api_key(filepath="api_key.txt"):
    """
    Read the Yelp API Key from file.
    
    Args:
        filepath (string): File containing API Key
    Returns:
        api_key (string): The API Key
    """
    
    # Feel free to modify this function if you are storing the API Key differently
    return Path(filepath).read_text().strip()
```

```python
def yelp_search_test(yelp_search):
    total, business = yelp_search(read_api_key(), "Pittsburgh")
    test.true(abs(total - 2600) < 60)
    expected_keys = ['id', 'name', 'phone', 'review_count']
    if len(business):
        test.true(all(k in set(business[0].keys()) for k in expected_keys))

@test
def yelp_search(api_key, query):
    """
    Make an authenticated request to the Yelp API.

    Args:
        query (string): Search term

    Returns:
        total (integer): total number of businesses on Yelp corresponding to the query
        businesses (list): list of dicts representing each business
    """
    
    return 0, []

```

---

Now that we have completed the "hello world" of working with the Yelp API, we are ready to really fly! The rest of the exercise will have a bit less direction since there are a variety of ways to retrieve the requested information but you should have all the component knowledge at this point to work with the API. Yelp being a fairly general platform actually has many more business than just restaurants, but by using the flexibility of the API we can ask it to only return the restaurants.

## Parameterization and Pagination

And before we can get any reviews on restaurants, we need to actually get the metadata on ALL of the restaurants in Pittsburgh. Notice above that while Yelp told us that there are ~13400, the response contained far fewer actual `Business` objects. This is due to pagination and is a safeguard against returning __TOO__ much data in a single request (what would happen if there were 100,000 restaurants?) and can be used in conjuction with _rate limiting_ as well as a way to throttle and protect access to Yelp data.

If an API has 1,000,000 records, but only returns 10 records per page and limits you to 5 requests per second... how long will it take to acquire ALL of the records contained in the API?

One of the ways that APIs are an improvement over plain web scraping is the ability to make __parameterized__ requests. Just like the Python functions you have been writing have arguments (or parameters) that allow you to customize its behavior/actions (an output) without having to rewrite the function entirely, we can parameterize the queries we make to the Yelp API to filter the results it returns.

---

## Q2: Aquire all of the restaurants in Pittsburgh (on Yelp)

Again using the [API documentation](https://www.yelp.com/developers/documentation/v3/business_search) for the `search` endpoint, fill in the following function to retrieve all of the _Restaurants_ (using categories) for a given query. Again you should use your `read_api_key()` function outside of the `all_restaurants()` stub to read the API Key used for the requests. You will need to account for __pagination__ and __[rate limiting](https://www.yelp.com/developers/faq)__ to:

1. Retrieve all of the Business objects (# of business objects should equal `total` in the response). Paginate by querying 20 restaurants each request.
2. Pause slightly (at least 200 milliseconds) between subsequent requests so as to not overwhelm the API (and get blocked).  

As always with API access, make sure you follow all of the [API's policies](https://www.yelp.com/developers/api_terms) and use the API responsibly and respectfully.

**DO NOT MAKE TOO MANY REQUESTS TOO QUICKLY OR YOUR KEY MAY BE BLOCKED**

Again, you can test your function with an individual neighborhod in Pittsburgh (I recommend Polish Hill). Pittsburgh itself has a lot of restaurants... meaning it will take a lot of time to download them all.

```python
>>> data = all_restaurants(api_key, 'Polish Hill, Pittsburgh')
>>> print(len(data))
315
>>> print([x['name'] for x in data])
['Lili Cafe', 'Morcilla', 'Umami', 'Piccolo Forno', "Alfred's Deli & Market", ...]
```

```python
def all_restaurants(api_key, query):
    """
    Retrieve ALL the restaurants on Yelp for a given query.

    Args:
        query (string): Search term

    Returns:
        results (list): list of dicts representing each business
    """

    return []
```

---

Now that we have the metadata on all of the restaurants in Pittsburgh (or at least the ones listed on Yelp), we can retrieve the reviews and ratings. The Yelp API gives us aggregate information on ratings but it doesn't give us the review text or individual users' ratings for a restaurant. For that we need to turn to web scraping, but to find out what pages to scrape we first need to parse our JSON from the API to extract the URLs of the restaurants.

In general, it is a best practice to seperate the act of __downloading__ data and __parsing__ data. This ensures that your data processing pipeline is modular and extensible (and autogradable ;). This decoupling also solves the problem of expensive downloading but cheap parsing (in terms of computation and time).

---

## Q 2.5: Parse the API Responses and Extract the URLs

Because we want to seperate the __downloading__ from the __parsing__, fill in the following function to parse the URLs pointing to the restaurants on `yelp.com`. As input your function should expect a string of [properly formatted JSON](http://www.json.org/) (which is similar to __BUT__ not the same as a Python dictionary) and as output should return a Python list of strings. The input JSON will be structured as follows (same as the [sample](https://www.yelp.com/developers/documentation/v3/business_search) on the Yelp API page):

```python
json_src = """{
  "total": 8228,
  "businesses": [
    {
      "rating": 4,
      "price": "$",
      "phone": "+14152520800",
      "id": "four-barrel-coffee-san-francisco",
      "is_closed": false,
      "categories": [
        {
          "alias": "coffee",
          "title": "Coffee & Tea"
        }
      ],
      "review_count": 1738,
      "name": "Four Barrel Coffee",
      "url": "https://www.yelp.com/biz/four-barrel-coffee-san-francisco",
      "coordinates": {
        "latitude": 37.7670169511878,
        "longitude": -122.42184275
      },
      "image_url": "http://s3-media2.fl.yelpcdn.com/bphoto/MmgtASP3l_t4tPCL1iAsCg/o.jpg",
      "location": {
        "city": "San Francisco",
        "country": "US",
        "address2": "",
        "address3": "",
        "state": "CA",
        "address1": "375 Valencia St",
        "zip_code": "94103"
      },
      "distance": 1604.23,
      "transactions": ["pickup", "delivery"]
    }
  ],
  "region": {
    "center": {
      "latitude": 37.767413217936834,
      "longitude": -122.42820739746094
    }
  }
}
"""
```

```python
def parse_api_response_test(parse_api_response):
    test.equal(parse_api_response(json_src), ['https://www.yelp.com/biz/four-barrel-coffee-san-francisco'])

@test
def parse_api_response(data):
    """
    Parse Yelp API results to extract restaurant URLs.
    
    Args:
        data (string): String of properly formatted JSON.

    Returns:
        (list): list of URLs as strings from the input JSON.
    """
    
    return []
```

---

As we can see, JSON is quite trivial to parse (which is not the case with HTML as we will see in a second) and work with programmatically. This is why it is one of the most ubiquitous data serialization formats (especially for RESTful APIs) and a huge benefit of working with a well defined API if one exists. But APIs do not always exists or provide the data we might need, and as a last resort we can always scrape web pages...

## Working with Web Pages (and HTML)

Think of APIs as similar to accessing a application's database itself (something you can interactively query and receive structured data back). But the results are usually in a somewhat raw form with no formatting or visual representation (like the results from a database query). This is a benefit _AND_ a drawback depending on the end use case. For data science and _programatic_ analysis this raw form is quite ideal, but for an end user requesting information from a _graphical interface_ (like a web browser) this is very far from ideal since it takes some cognitive overhead to interpret the raw information. And vice versa, if we have HTML it is quite easy for a human to visually interpret it, but to try to perform some type of programmatic analysis we first need to parse the HTML into a more structured form.

As a general rule of thumb, if the data you need can be accessed or retrieved in a structured form (either from a bulk download or API) prefer that first. But if the data you want (and need) is not as in our case we need to resort to alternative (messier) means.

Going back to the "hello world" example of question 1 with the NYT, we will do something similar to retrieve the HTML of the Yelp site itself (rather than going through the API) programmatically as text. 

---

## Q3: Parse a Yelp restaurant Page

Using `BeautifulSoup`, parse the HTML of a single Yelp restaurant page to extract the reviews in a structured form as well as the total number of pages. Fill in following function stubs to parse a single page of reviews and return:
* the reviews as a structured Python dictionary
* the total number of pages of reviews.

For each review be sure to structure your Python dictionary as follows (to be graded correctly). The order of the keys doesn't matter, only the keys and the data type of the values:

```python
{
    'author': 'Aaron W.' # str
    'rating': 4.0        # float
    'date': '2019-01-03' # str, yyyy-mm-dd
    'description': "Wonderful!" # str
}
```

Return reviews in the order that they are present on the page.

There can be issues with Beautiful Soup using various parsers, for maximum conpatibility (and fewest errors) initialize the library with the default (and Python standard library parser): `BeautifulSoup(markup, "html.parser")`. You may notice that the HTML is automatically generated. Yelp uses a modern web application technology called [React](https://reactjs.org/), which generates the markup from Javascript code. This is a common hazard of scraping data from HTML.

Here's a hint for this problem: [`aria`](https://www.w3.org/WAI/PF/aria-1.1/terms) is a web standard that supports text-to-speech, and `aria-*` properties are often used to tag elements that have some semantic meaning.  

```python
# You do not need to use regular expressions in this solution. This is only for testing.
import re

def reviews_check(reviews):
    type_check = lambda field, typ: all(field in r and typ(r[field]) for r in reviews)
    test.true(type_check("author", lambda r: isinstance(r, str)))
    test.true(type_check("rating", lambda r: isinstance(r, float)))

    datecheck = re.compile("^\d{4}-\d{2}-\d{2}$")
    test.true(type_check("date", lambda r: datecheck.match(r)))
    test.true(type_check("description", lambda r: isinstance(r, str)))

def parse_page_test(parse_page):
    reviews, num_pages = parse_page(retrieve_html("https://www.yelp.com/biz/the-porch-at-schenley-pittsburgh")[1])
    reviews_check(reviews)
    test.equal(len(reviews), 20)
    test.equal(num_pages, 32)

@test
def parse_page(html):
    """
    Parse the reviews on a single page of a restaurant.
    
    Args:
        html (string): String of HTML corresponding to a Yelp restaurant

    Returns:
        tuple(list, string): a tuple of two elements
            first element: list of dictionaries corresponding to the extracted review information
            second element: URL for the next page of reviews (or None if it is the last page)
    """
    
    return [], None

```

---

## Q 3.5: Extract all of the Yelp reviews for a Single Restaurant

So now that we have parsed a single page, and figured out a method to go from one page to the next we are ready to combine these two techniques and actually crawl through web pages! 

Using `requests`, programmatically retrieve __ALL__ of the reviews for a __single__ restaurant (provided as a parameter). Just like the API was paginated, the HTML paginates its reviews (it would be a very long web page to show 300 reviews on a single page) and to get all the reviews you will need to parse and traverse the HTML. As input your function will receive a URL corresponding to a Yelp restaurant. As output return a list of dictionaries (structured the same as question 3 containing the relevant information from the reviews.

Return reviews in the order that they are present on the page.

You will need to get the number of pages on the first request and generate the URL for subsequent pages automatically. Use the Yelp website to see how the URL changes for subsequent pages.

```python
def extract_reviews_test(extract_reviews):
    reviews = extract_reviews("https://www.yelp.com/biz/larry-and-carols-pizza-pittsburgh")
    test.equal(len(reviews), 47) # This may change!
    reviews_check(reviews)

@test
def extract_reviews(url):
    """
    Retrieve ALL of the reviews for a single restaurant on Yelp.

    Parameters:
        url (string): Yelp URL corresponding to the restaurant of interest.

    Returns:
        reviews (list): list of dictionaries containing extracted review information
    """
    return []

```
