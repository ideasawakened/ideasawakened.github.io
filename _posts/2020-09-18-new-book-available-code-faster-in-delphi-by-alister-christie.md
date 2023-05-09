---
layout: post
title: "New book available: Code Faster In Delphi by Alister Christie"
date: 2020-09-18 12:00:00 +0000
last_modified_at: 2020-09-20 12:00:00 +0000
categories: [Delphi Programming, Review]
tags: [Alister Christie]
permalink: post/new-book-available-code-faster-in-delphi-by-alister-christie
published: true
image: /assets/blog/Books/Code-Faster-In-Delphi-small.png
description: Short review on Alister Chrisite's new book "Code Faster in Delphi"
---
It's great to see books still being published targeting Delphi developers. The latest one is from a well known Delphi developer, **Alister Christie** and it's called [**Code Faster In Delphi**](https://leanpub.com/codefasterindelphi), which is available from as an e-book from [**LeanPub**](https://leanpub.com/). (Available as PDF for reading on a computer, EPUB for reading on phones and tables, and MOBI for reading on a Kindle. It should be available in hardcopy/print form soon.)

![Code Faster in Delphi book cover](/assets/blog/Books/Code-Faster-In-Delphi-Book-Cover.png)

Alister may be most well-known for his popular video series, [**LearnDelphi.tv**.](https://www.learndelphi.tv/) His latest in the series, [**Delphi Video #157**](https://youtu.be/pnTcwlJug3E) from September 15th 2020, covers some [**keyboard shortcuts**](http://docwiki.embarcadero.com/RADStudio/en/Default_Keyboard_Shortcuts) usable in the Delphi IDE. In this four+ minute video, he covers three types of shortcuts:

-   Select identifier, then expand: CTRL+W
    
-   Smart Surround Keys: (, \[ and {
    
-   Shift cursor within selection: Ctrl+Q+B and Ctrl+Q+K
    

I had forgotten about the Smart Surround Keys and will attempt to re-integrate that into future coding sessions. Simply highly the text and use the left version of a bracket to surround the text with the particular bracket pair. You can use parenthesis, square brackets or curly brackets. This functionality can be toggled on and off in the IDE configuration: Tools->Options->User Interface->Editor->Key Mappings.

Alister uses the video as a small intro into his **Code Faster in Delphi** book. He may release further videos in the future that highlight other content from his book. The table of contents of his book is reproduced from LeanPub and is listed at the bottom of the blog post.

**Update:** Alister released another video demonstrating some further tips, this time focusing on different types of selection modes. See [**Delphi Video #158**](https://youtu.be/7YgS4chHM2k) from September 20th, 2020 on YouTube which covers:

-   Block Selection Mode is temporarily enabled by holding the Alt-key while dragging the mouse.
    
-   **Ctrl+O+C** to enable Block Selection Mode
    
-   **Ctrl+O+L** to enable Line Selection Mode
    
-   **Ctrl+O+K** to revert back to Normal Selection Mode
    

As you can see from the table of contents, the book covers a wide variety of ways to actually code faster in the Delphi IDE. There's enough information in there that everyone will likely learn, or re-learn, something to help them be more productive. In his next book, **Code Better In Delphi**, Alister plans to improve on your Delphi skills. This book is simply a practical list of ways to produce code more quickly.

The first chapter is on Touch Typing. For fun, I used one of the sites he referenced in his book and tested my typing speed to see where I'm at these days. When I was young, I was an extremely fast typist as I transcribed hand-written vouchers all day while working for the government as an accounting technician. They basically gave me a large pile of paperwork every morning and my work-day was complete when the pile was finished so I was highly motivated to type faster. Eventually, I spent afternoons helping to build reports in dBase III for fun. Since that was over 30 years ago, I'm happy to see that I'm still able to get 99wpm from [**TypingTest.com**](https://www.typingtest.com/). I will note that if you are typing 100wpm for any extended length of time in the Delphi IDE, then something is wrong. The main goal would be to become a touch-typist so that the keyboard doesn't get in the way of your thoughts. I agree with Alister on his mechanical keyboard preference as I originally learned to type on a manual typewriter. I like to feel, and hear, each click! (Although the people around you typicall do not particularly enjoy the constant keyboard noise.) A quick note for the old-timers that may not be aware - you can still get a new "Model M" keyboard online at: [**Unicomp**](https://www.pckeyboard.com/page/category/UKBD).

The next chapter covers the Delphi IDE and starts with a list of Keyboard Shortcuts. I spent some time looking over the list of Refactoring keys as I don't use them all. A quite handy one that I don't use today is **Ctrl+Shift+D**. After reading the chapter, I tried this shortcut out and will likely use this a lot in the future. Try this: start a new VCL application, drop a button on the form, double-click the button, and in the event type **fTesting := 'Hello'** Put the cursor on the fTesting variable and then use this **Ctrl+Shift+D** shortcut and it will popup a populated Add New Field dialog box. Simply click the OK button and you will end up with a new field automatically defined in the private section of the form's class. I have no idea why I haven't been using this shortcut in the past, but the **Code Faster in Delphi** book just helped me do just that! I need to test out a few more keyboard shortcuts listed in this book. It's a bit embarrassing that I've missed some of these...

The next chapter covers the Form Designer. Another gem which Alister uncovered for me is the **Add Component** right click option. Drop a panel on a form and right click the panel and select Add Component and you have the ability to drop a Label, Button, Edit, Panel, ToolBar, or StatusBar to the panel. I wonder how many years ago that menu item was added to the Form Designer? (I also wonder how unobservant I have been on other IDE improvements...) Again, this book is directly helping me to **Code Faster in Delphi**.

There's a chapter on Customising the IDE which covers custom layouts. Note that the latest version, [**10.4 Sydney Update 1**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.1.0), has many fixes related to the IDE and custom layouts for High DPI and dual-monitor situations. If you have had issues with custom layouts in the past and perhaps may have given up on this IDE feature, it's really time to try again starting with a review of this chapter. Setting up custom layouts for Code Editing, Form Design, and Debugging really helps to keep your focus on the current task.

I won't cover all the chapters as you really should buy the book to get all the details. To be honest, I purchased the book simply because I purchase just about every new Delphi book that ever hits the market (because there are so few.) Since I've been a long-time Delphi developer, I didn't think I would get much out of this particular book as I assumed that it was targeted mainly at Delphi noobs. I was wrong. It's a nice reference that most anyone can rely on to improve their skills. It would be a great reference for new Delphi developers, and it's also a good refresher for the old timers. I'm glad that I purchased this book and I look forward to Alister's next book, **Code Better in Delphi**.

Alister's website has many hours of content related to Delphi programming, much of it is [**freely available**](https://www.learndelphi.tv/free-videos) to watch on YouTube. While you are on his website, also checkout his [**paid content**](https://www.learndelphi.tv/products) which has bundles available which cover specific topics such as VCL Application development, XML in Delphi, TClientDataSet, Generic Collections and others. If needed, Alister is also available for [**training and consultancy**](https://www.learndelphi.tv/training).

## **Code Faster in Delphi**

### Table of Contents

**Copyright 1**

**Dedication 1**

**Table of Contents 2**

**Foreword 6**

**Preface 7**

**Acknowledgements 8**

**Introduction 9**

**Conventions Used in this Book 9**

**Scope 10**

**Code Samples 10**

**Code Faster by Typing Faster 11**

**Touch Typing 11**

Getting Started with Touch Typing 13

**Know Thy Keyboard Shortcuts 14**

**The Delphi Code Editor 16**

**Keyboard Shortcuts 16**

**CodeInsight 26**

**Code Templates 27**

**MultiPaste 30**

**The Editor Toolbar 31**

**IDE Insight 33**

**Structure View 33**

**The Class Explorer 35**

**Code History 36**

**Macros 38**

**Surround 39**

**SyncEdit 40**

**The Delphi Form Designer 41**

**Keyboard Shortcuts 41**

**Quick Edits 43**

**Quick Actions 44**

**Add Control and Add Component 45**

**Object Inspector 45**

**Structure View 46**

**The Component Palette 48**

**Editing the Form’s Source 50**

**Editing the Clipboard 52**

**Aligning Controls 52**

Position 53

Alignment (and Size) 53

VCL Guidelines 56

Windows Magnifier 56

**Customising the IDE 57**

**IDE Layout 57**

Unpinning and Undocking 57

Desktop Speedsettings 60

**Changing the ToolBar and ToolButtons 62**

**Welcome to the Dark Side 63**

**Write Your Own IDE Plugin 65**

Further Learning 69

**Language Features 70**

**Interfaces 70**

Further Learning 72

**Generics 73**

Generic Collections 75

**Anonymous Methods 76**

Variable Capture 78

**Anonymous Threads 79**

Further Learning 80

**Inline Variables and Type Inferencing 80**

**Know the RTL 82**

**Measuring Time 82**

**Generic Collections 83**

TDictionary 85

Further Learning 88

**Parallel Programming 88**

No Parallel Example 89

Background Thread Example 91

Multiple Tasks Example 92

Parallel For Example 94

Further Learning 97

**Regular Expressions 97**

IP Address Validation 97

IsMatch 98

Match 99

Matches 100

Replace 101

Summary 102

Further Learning 102

**Enhanced RTTI 102**

Reading Properties 103

Writing Properties 104

Further Learning 106

**FireDAC 106**

TFDConnection 106

Adding a TFDQuery 109

But There’s More 110

Further Learning 111

**Tools and Plugins 112**

**Third-Party Tools 112**

**cnWizards / cnPack 112**

Structural Highlighting 112

Tab Order 115

Component Prefix Wizard 117

**ModelMaker Code Explorer 119**

Live Documentation 120

Class Browser 121

Tip of the Day 123

The MMX Toolbar 123

**Navigator 125**

**Bookmarks 127**

**CodeSite 129**

Further Learning 130

**GExperts 130**

Clipboard History 131

File Favorites 131

AutoCorrect 132

Backup Project 133

**Other Non-Delphi Specific Tools 135**

**Third-Party Libraries 135**

**Metaprogramming 136**

Case study - BDE Replacement. 136

**Find and Replace 137**

In the IDE 137

Turbo GREP 137

**Delphi AST 140**

**DFM Parser 142**

**reFind 143**

**Mida Converter 145**

**cnWizards Property Corrector 146**

**GExperts Replace Components 147**

**Your Physical Environment 150**

**Hardware 150**

Keyboard 150

Mouse 150

Computer 150

Screens 151

Chair 151

Desk 151

**Other Considerations 152**

Environmental 152

Interruptions 152

Multitasking 153

**Sharpening the Saw 154**

**Where to go when you are Stuck 154**

Google is Your Friend 154

Asking Questions 155

Stack Overflow 155

**Recommended Reading 155**

**Social Networks 156**

Facebook 156

LinkedIn 156

Twitter 156

Meetup 156

YouTube 157

StackOverflow 157

Delphi-PRAXiS 157

**Becoming Known as an Expert 157**

**What’s Improved Productivity Worth 158**

As an Employer 158

As an Employee 158

Self Employed 159

Diminishing returns on investment 159

Further Learning 159

**Final Words and Conclusion 160**