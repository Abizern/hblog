---
title: Getting the address of a shortened URL
tags: programming
---

Many times you will see a shortened url but you aren't sure what it points
at. Here's a little tip that I picked up from
[Tom Morris](http://tommorris.org).

Pop open your terminal, or console, or whatever your command-line application is
and type the following:

``` bash
curl --head shorturl
```

This pops up a short amount of useful information about the target url. For
example the response for the Google short URL for this site shows this;

``` bash
~ % curl --head http://goo.gl/oQx8
HTTP/1.1 301 Moved Permanently
Content-Type: text/html; charset=UTF-8
Expires: Sat, 16 Oct 2010 21:01:51 GMT
Date: Sat, 16 Oct 2010 21:01:51 GMT
Cache-Control: private, max-age=86400
Location: http://abizern.org/
X-Content-Type-Options: nosniff
X-Frame-Options: SAMEORIGIN
X-XSS-Protection: 1; mode=block
Server: GSE
Transfer-Encoding: chunked
```

And you can see that the Location shows that you aren't going to get
Rick-rolled.
