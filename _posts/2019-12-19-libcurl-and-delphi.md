---
layout: post
title: "libcurl and Delphi"
date: 2019-12-19 12:00:00 +0000
last_modified_at: 2019-12-20 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [libcurl, openssl, Mikhail Merkuryev, Arnaud Bouchez, Christophe Espern, Jeffrey Pohlmeyer]
permalink: post/libcurl-and-delphi
published: true
image: /assets/blog/curl/curl4delphi-small.png
description: Using libcurl in Delphi applications with curl4delphi
---

[**libcurl**](https://curl.haxx.se/libcurl/) is a hugely popular tool, available on most platforms and supports many internet protocols. I've mainly used it for HTTP GET/POST tasks but there's built-in support for FTP, IMAP, POP3, SMTP, Gopher, RTMP, Telnet and more. It can handle proxies, cookies and various authentication options. In short, it's a deep toolbox that is extremely handy to use on a command line and is widely regarded as one of the most popular system tools of all time.

libcurl has a C API which is free and opensource. On their [**bindings page**](https://curl.haxx.se/libcurl/bindings.html), they currently list three options for Delphi developers:

-   [**OP-Curl**](https://web.archive.org/web/20020610214926/http://www.tekool.com/opcurl/) - Object-Pascal Wrapper for Curl by **Christophe Espern**. This is a very dated wrapper targeted at FreePascal.
    
-   [**CurlPas**](https://web.archive.org/web/20030804091414/houston.quik.com/jkp/curlpas/) - An ancient free-pascal library by **Jeffrey Pohlmeyer**. There is a newer version of [**CurlPas on SourceForge**](https://sourceforge.net/projects/curlpas/) with code dating back to 2005 and is apparently still getting traffic as there has been 11 downloads this week. There is a [**libcurl post**](https://forum.lazarus.freepascal.org/index.php/topic,21932.msg128853.html#msg128853) on the Lazarus forums about it being available in Free Pascal but not working for the Windows platform. There's also a newer [**GitHub mirror**](https://github.com/Soldat/CurlPas) which includes some fixes from 2013. (Another [**repo**](https://github.com/Ruzzz/CurlPas) is available on GitHub which is just a clone of the SourceForge code from 2005.)
    
-   [**curl4delphi**](https://github.com/Mercury13/curl4delphi) - A more recent port by **Mikhail Merkuryev**, with the last commit on November 3, 2017. It's available on GitHub under a MIT license.
    

![curl in Delphi](/assets/blog/curl/curl4delphi.png)

## **curl4delphi**

Let's get a test project using the latest project up and running. The first thing to do is to grab a copy of the curl4delphi repo, either download it directly from GitHub with your browser or fire up your favorite Git client and clone: [**https://github.com/Mercury13/curl4delphi.git**](https://github.com/Mercury13/curl4delphi.git)

You'll also want to grab the latest version of **curl for Windows**, which is currently version 7.67.0. [**Download curl**](https://curl.haxx.se/windows/) [](https://curl.haxx.se/windows/)and you'll use the included **libcurl.dll** or **libcurl-x64.dll** file found in the bin folder, depending if you are using 32 or 64-bit projects. Also copy the **curl-ca-bundle.crt** file or grab the very latest [**cacert.pem file from Mozilla**](https://curl.haxx.se/docs/caextract.html) which is also available on curl's website. (_These are the same cert files, just with different names._)

This download will also get you the latest curl.exe to play with. Note that on my updated Windows 10 machine, the included curl.exe (found in windows\\system32 and also in windows\\sysWOW64) is an older version 7.55.1 which dates back to August 9, 2017. It's seemingly great that Microsoft started including curl in Windows 10 as of insider build 17063/update 1803, but you would think they would keep the tools provided in their System folders up to date. Just be aware of which version of curl.exe you are using (to be sure, run: _**curl.exe --version**_)

Next, you'll need the OpenSSL binaries which are available on the same download page. Again, download the 32-bit or 64-bit version based on your requirements. You'll need the **libcrypto-1\_1.dll** and **libssl1-1\_1.dll** files for 32-bit or the **libcrypto-1\_1-x64.dll** and **libssl-1\_1-x64.dll** files found in the root folder of the downloaded zip file.

Either put these required dependencies into a folder that is in your system PATH, or put them in the same folder as your application's executable. Also note further dependencies are available on the download page including [**brotli**](https://www.brotli.org/), [**libssh2**](https://www.libssh2.org/), [**nghttp2**](https://www.nghttp2.org/), and [**zlib**](https://www.zlib.net/). (ZLib support is built-in and is not required as a separate DLL.)

For now, the DLLs for libcurl, libssl, and libcrypto should be all that we need to fire up a sample project and download something from the web.

Start your Delphi (_version XE2 or later_) and load one of the easy sample projects included by curl4delphi such as: **Examples\\Easy\_Http\\Readme\\Readme.dproj**

When you build this project (note that the Windows 64-bit platform is selected by default) you will get the HTML source downloaded from [http://www.example.com](http://www.example.com/) (If you get an error, ensure the dependencies are in the output folder.)

You can also load another example project from **Examples\\Misc\\Version\\Version.dproj** and see the capabilities of your curl library. It should appear something like:

![curl library capabilities](/assets/blog/curl/curl4delphi-version.png)

For something a bit more interesting, try the HTTPS demo provided in **Examples\\Raw\_Http\\Https\\Https.dproj**. You'll need the previously mentioned cacert.pem or curl-ca-bundle.crt files for this to work. This is the original code from the HTTP demo:
````pascal
 curl := curl_easy_init;
  if curl <> nil then begin
    curl_easy_setopt(curl, CURLOPT_URL, 'https://ukr.net/');
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, true);
    curl_easy_setopt(curl, CURLOPT_CAINFO, 'cacert.pem');
````

To use the cert bundle provided within the curl download, change the source line to reflect **curl-ca-bundle.crt** and perhaps change the example URL being used, and you should have a working https example download:
````pascal
  curl := curl_easy_init;
  if curl <> nil then begin
    curl_easy_setopt(curl, CURLOPT_URL, 'https://www.google.com/');
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, true);
    curl_easy_setopt(curl, CURLOPT_CAINFO, 'curl-ca-bundle.crt');
````

In a few minutes, with little effort, you have access for a wide variety of curl actions directly from your Delphi applications - including full TLS support. Thank you, **Mikhail!**

For further reading, curl has a page dedicated to [**SSL Certificate Verification**](https://curl.haxx.se/docs/sslcerts.html) to review. You can also review a blog entry covering [**SNI support**](https://major.io/2012/02/07/using-openssls-s_client-command-with-web-servers-using-server-name-indication-sni/) when using the _\-servername_ argument with OpenSSL to generate a server's PEM file.

If you are debugging connectivity and you want to skip the certificate validation routines, you can set a curl option during testing - see the following working example using this same HTTPS example project:
````pascal
curl := curl_easy_init;
  if curl <> nil then begin
    curl_easy_setopt(curl, CURLOPT_URL, 'https://www.google.com/');
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, true);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0);  //debug only!
````

### **License review**

When dealing with third party components you should always review the license files. libcurl has a pretty broad [**MIT-style license**](https://curl.haxx.se/docs/copyright.html), making it available for use in your commercial projects. If you have [**pointy-haired bosses**](https://en.wikipedia.org/wiki/Pointy-haired_Boss) reviewing the decision, you can provide them with a partial list of [**companies using curl in commercial projects**](https://curl.haxx.se/docs/companies.html). The list is quite impressive - including Apple, Google, Facebook, Samsung, Cisco and many others.

As stated in his GitHub repo, the license for curl4delphi is MIT for the source and Public Domain for the examples so you shouldn't have a problem there either. While at GitHub, check out the [**curl4delphi wiki**](https://github.com/Mercury13/curl4delphi/wiki) which includes a [**FAQ**](https://github.com/Mercury13/curl4delphi/wiki/FAQ) and notes on [**curl issues with Unicode on Windows**](https://github.com/Mercury13/curl4delphi/wiki/Unicode-support).

### **Summary**

curl4delphi is seemingly a little-known wrapper for libcurl as it currently has only 23 stars on GitHub. It offers unique Unicode support and grants your Delphi XE2+ applications access to a host of functionality from the famous libcurl library. Given the liberal MIT license, you are free to use this functionality in your commercial applications. Clone it from GitHub today - and while you are there, click the Star button to let Mikhail and others know of your support!

----

_Update:_ **Arnaud Bouchez** stopped by the blog and commented about libcurl support in [**mORMot**](https://synopse.info/fossil/wiki?name=SQLite3+Framework). I ran some quick tests it and it's fairly easy to get started. I added a [**demo project on GitHub**](https://github.com/ideasawakened/Sandbox_mORMot) to show three different HTTP clients in mORMot, including a libcurl version. mORMot is a large project and there are a lot of hidden gems included. I'll probably add some more demos to that new GitHub repo as I discover more.