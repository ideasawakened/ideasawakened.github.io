---
layout: post
title: "Product Review: Pascal Expert"
date: 2019-11-18 12:00:00 +0000
last_modified_at: 2019-11-18 12:00:00 +0000
categories: [delphi programming, product review]
tags: [Delphi, Pascal Expert, Peganza, Static Code Analysis, Roman Yankovsky]
permalink: post/product-review-pascal-expert
published: true
image: /assets/blog/Peganza/Pascal-Expert/PEX_Menu.jpg
description: Product review of Pascal Expert, a Delphi IDE plug-in featuring static source code analysis.
---

The Delphi and Pascal developer community has historically had many active third-party tools and components. The current perception held by many is that the majority of these solutions have been neglected or abandoned. However, after you start looking, you'll discover that there is a vibrant third-party community still operating today. As part of a new blog series, we'll look at some of these solutions with Product Review posts such as this.

We'll start with a tool from one of the stalwarts of the community, [**Peganza**](https://www.peganza.com/). We'll likely look at their venerable [**Pascal Analyzer**](https://www.peganza.com/products_pal.html) product in a future post, but we'll start today with its younger cousin [**Pascal Expert**](https://www.peganza.com/products_pex.html).

### **What is Pascal Expert?**

Pascal Expert is an IDE plug-in for Delphi which performs **static code analysis** on the current project, also known as a "white-box" testing tool. Pascal Expert was first released back in October 2015.

Analyzing source within the IDE is triggered via a click of a Tools menu item, a toolbar button, or via a hotkey, to analyze the current project, the current module, or to perform a Quick Analysis of the current module. Below is the Tools menu item, and corresponding toolbar menu icons:

![Pascal Analyzer - Menu](/assets/blog/Peganza/Pascal-Expert/PEX_Menu.jpg)

The source is analyzed and up to four types of warnings are presented in the output window, similar to the warnings presented during compilation.

Currently, there are **142 different warnings** that can be revealed by Pascal Expert. Surprisingly, this is just a small subset of the Pascal Analyzer product which has many more reporting options available. Small projects are analyzed in a few seconds, and extremely large projects with many hundreds of thousands of lines of source can be fully scanned in well under a minute. (Your mileage may vary based on your hardware in use. It is highly recommended to be using NVMe drives for development machines.)

![](/assets/blog/Peganza/Pascal-Expert/PEX_Warnings.png)

Some may see some alerts as "noise" and these can be disabled in the options menu. (For example, if you rarely change your visual components from their numerically named default values such as Edit1, you will want to disable that warning.) Every alert in the output window can be selected and the F1 help key used to bring up context-sensitive help on the particular warning. You can also double-click the warning to open the source location in the editor.

![](/assets/blog/Peganza/Pascal-Expert/PEX_ContextSensitiveHelp.png)

### **Alerts**

The first type of warnings are Alerts. There are currently 87 alerts provided with Pascal Expert, with 78 of them activated by default. The few that are not active are "possibly never set" type warnings which you can enable as needed. By default, these appear in Red in the output window. (Each type of alert can have its output window messages customized for font type, style, size, color, and background color.)

![](/assets/blog/Peganza/Pascal-Expert/PEX_Alerts1.png)

### **Reductions**

The next alert is Reductions. There are 24 provided with 20 active by default. (Again a few "possibly set" style warnings are disabled to reduce noise.) The full list of Reductions is displayed below. You'll see warnings such as "**Identifiers never used**" and these are displayed for you to possibly simplify your code base. This particular warning is displayed by the compiler if warnings are enabled. However, there are more advanced warnings available such as "**Fields only used in single method**" which could alert you to change the class field to a local variable.

![](/assets/blog/Peganza/Pascal-Expert/PEX_Reductions.png)

### **Optimizations**

This type of alert informs you of possible code changes which could yield better performance. There are 9 types of optimization alerts provided. The full list is displayed below, starting with an often-missed simple performance improvement: "**Missing 'const' for unmodified string parameter**."

![](/assets/blog/Peganza/Pascal-Expert/PEX_Optimizations.png)

### **Conventions**

The last type of alert is Conventions, with 22 common alerts to help standardize your code base by enforcing strict naming conventions, such as the Delphi standard practice of "**Interface types that do not start with an 'I'**" or leaving the default names for visual components such as "Edit1"

![](/assets/blog/Peganza/Pascal-Expert/PEX_Conventions.png)

### **Configuration**

You can configure keyboard short-cuts to execute Pascal Expert instead of using the menu or toolbar buttons. These options are available in the Options menu. **Note** that the product analyzes source code files, and not the in-memory editor buffer so you need to save changes before analyzing the source.

![](/assets/blog/Peganza/Pascal-Expert/PEX_GeneralConfig.png)

You can also configure the tool to exclude source from analysis. By default, the Delphi base folder is excluded. For fun, you can remove this exclusion and see all the warnings in the Delphi source code - but it may be disappointing to see the results!

You can also easily mark specific items in code to be excluded. You can either use compiler directives to exclude blocks of code, or use a simple source code comment for single line exceptions, such as _//PALOFF_

![](/assets/blog/Peganza/Pascal-Expert/PEX_ReportingConfig.png)

### **Automation**

Runtime automation is only available with their Pascal Analyzer product. Pascal Expert is for manual use within the IDE only.

### **Support**

Email support is available and has proven on multiple occurrences to be timely with responses received under one business day, some within one hour. They also offer a [**FAQ Page**](https://peganza.com/faq.html) covering some common issues. Their [**blog**](https://peganza.com/blog.html) is frequently updated as well.

### **Alternatives**

-   Perhaps the very first white-box plug-in for Delphi was [**FixInsight**](https://tmssoftware.com/site/fixinsight.asp). This was originally written by **Roman Yankovsky** and introduced in early 2015. It was picked up by **TMS Software** in [June 2016](http://sourceoddity.com/fi_tms.html). We'll look at this product in a future post, but a quick review reveals that it offers only a small subset of warnings that Pascal Expert provides. It does seem to be popular so perhaps there are features that we'll find after a deeper dive. (Automation is available in the FixInsight Pro license.)
    
-   The most in-depth alternative to Pascal Expert is available from Peganza: [**Pascal Analyzer**](https://www.peganza.com/products_pal.html). This product has been around since 2001 and has been continuously updated since. We've purchased a license for this product and we'll definitely do a deeper dive in a future blog post. It offers both stand-alone reporting and command line automation tools.
    
-   A free version of Pascal Analyzer is also available, called **Pascal Analyzer Lite**. It was released back in [**mid-2017**](https://www.peganza.com/introducing-pascal-analyzer-lite.html) and is a feature-limited version of Pascal Analyzer.
    
-   For years, the main competitor to Pascal Analyzer was [**CodeHealer**](http://www.socksoftware.com/codehealer.php). Unfortunately, this product appears abandoned.
    

### **Technical Details**

-   **Compatibility**: All versions of Delphi from 2007 to current are supported.
    
-   **Trial version available**: a feature-limited 30-day trial version is available via their [**download page**](https://www.peganza.com/download.html).
    
-   [**Online documentation**](https://www.peganza.com/PEXHelp/index.html) is available for review before purchase.
    
-   **License**: the software is licensed per-developer and can be installed up to four computers. The license is device-locked and can be simply uninstalled to migrate the license to another machine. (Offline registration is also available via an activation request file.)
    
-   **Initial cost**: if you want to purchase Pascal Expert alone, the cost is currently $89 per user, with volume licensing down to $66/user which includes support for the first year.
    
-   **Annual maintenance**: you can renew support for $39/year/user down to $29/year/user for volume purchases.
    
-   **Cost saving bundling option**: if you are interested in Pascal Analyzer (or their other product, Pascal Browser) then it makes sense to purchase a bundle of products for significant savings. See their [**orders page**](https://peganza.com/orders.html) for details.
    

### **Summary**

Every developer should be actively utilizing static source analysis tools during the development process and Pascal Expert is one of the few available for Delphi. It is simply a **must-have tool** and the cost is negligible for the value it provides. If you don't have a copy, you should download their demo today!