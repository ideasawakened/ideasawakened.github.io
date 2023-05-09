---
layout: post
title: "Simple SVG images in Delphi applications"
date: 2019-11-19 12:00:00 +0000
last_modified_at: 2020-08-25 12:00:00 +0000
categories: [Delphi Programming, Components]
tags: [SVG, Open Source]
permalink: post/simple-svg-images-in-delphi-applications
published: true
image: /assets/blog/SVG/TPath_Delphi.png
description: Use Scalable Vector Graphics in your Delphi applications.
---
**Scalable Vector Graphics** (SVG) are becoming more popular as an alternative to providing custom sized icons for the various target devices in use. In the old days it certainly felt much easier. Of course, it depends on your definition _old days_ as it could mean 80 column terminals, 640x480, 800x600, 1024x768 resolutions or a myriad of other choices. Today we have a vast array of devices that need to be considered for application development. They can range from small handhelds with 360x640 resolution to 8k monitors at 7680x4320 - along with aspect ratios varying from 16:9, 16:10, 3:2... In short, when you ask "what is the most common screen resolution to target", be prepared for at least three different answers. This blog post is not going to wade deep into those waters. Instead we will concentrate on a simple solution for utilizing SVG graphics in your Delphi FMX-based applications.

![Delphi logo as SVG](/assets/blog/SVG/TPath_Delphi.png)

**Why SVG?**

SVG rendering provides for no loss of image quality, regardless of size and resolution. In addition, the image files are typically much smaller than conventional raster images. In addition, recently all modern browsers have added support for the SVG file format (with varying degrees of capabilities of course.)

Instead of relying on grids of pixels, SVGs relay on paths, points, curves and angles. These are defined in textual form using XML compatible format.

### **A simple example**

Delphi includes a TPath component for use with Firemonkey based applications. This TPath component can easily handle basic SVG images. Note that earlier versions of FMX had major problems with SVG rendering and has been greatly improved over the years.

What you'll need to provide is the Path text from a source SVG file. The web is full of sites which offer icons of various sizes and formats, and many support SVG. You simply need to drop a TPath component onto an FMX form and set its Data property and you'll end up with a scalable vector graphic for use at any size. For example, use this text for the chevron image below:

````plaintext
M5.59000015258789,7.40999984741211 L7,6 L13,12 L7,18 L5.59000015258789,16.5900001525879 L10.1700000762939,12 L5.59000015258789,7.40999984741211 M11.5900001525879,7.40999984741211 L13,6 L19,12 L13,18 L11.5900001525879,16.5900001525879 L16.1700000762939,12 L11.5900001525879,7.40999984741211 Z
````

Delphi even shows you a preview of the graphic when editing the Path data property.

![TPath of Chevron image](/assets/blog/SVG/TPath_Chevron.png)

Combine this TPath component along with a TRectangle and a TSpeedButton and you can generate infinitely-scalable buttons with images. (Remember to set the HitTest property to False on the TPath component.)

You can define more complex paths within this Path data property. Try this Path data to generate the first more-complex image on this page:

````plaintext
M8,15.5 C3.86499977111816,15.5 0.5,12.1350002288818 0.5,8 C0.5,3.86499977111816 3.86500000953674,0.5 8,0.5 C12.1350002288818,0.5 15.5,3.86500000953674 15.5,8 C15.5,12.1350002288818 12.1350002288818,15.5 8,15.5 Z M8,1 C11.8599996566772,1 15,4.14000034332275 15,8 C15,11.8599996566772 11.8599996566772,15 8,15 C4.14000034332275,15 1,11.8599996566772 1,8 C1,4.14000034332275 4.1399998664856,1 8,1 M8,0 C3.5789999961853,0 0,3.5789999961853 0,8 C0,12.4209995269775 3.5789999961853,16 8,16 C12.4209995269775,16 16,12.4209995269775 16,8 C16,3.57900047302246 12.4209995269775,0 8,0 L8,0 Z M12.8800001144409,3 L9.17000007629395,5.96999979019165 C8.86999988555908,5.80000019073486 8.53999996185303,5.67000007629395 8.19999980926514,5.59000015258789 L11,2 L12.8800001144409,3 Z M9.5,2 L7.55999994277954,5.51000022888184 C7.48000001907349,5.48999977111816 7.3899998664856,5.48999977111816 7.30999994277954,5.48999977111816 C7.11999988555908,5.48999977111816 6.94000005722046,5.5 6.76000022888184,5.53999996185303 L7.75,2 Z M6.61999988555908,2.11999988555908 L6.05999994277954,5.69999980926514 C5.80000019073486,5.78999996185303 5.55000019073486,5.90999984741211 5.32000017166138,6.05999994277954 L5,2.84999990463257 Z M4.80000019073486,6.44000005722046 C4.57999992370605,6.63000011444092 4.3899998664856,6.84000015258789 4.21999979019165,7.07000017166138 L3,4.75 L4,3.5 Z M3.85999989509583,7.69000005722046 L3.85999989509583,7.69999980926514 C3.75999999046326,7.8899998664856 3.69000005722046,8.09000015258789 3.63000011444092,8.30000019073486 L2,6.98000001907349 L2.45000004768372,5.86999988555908 Z M3.5,9.28999996185303 C3.5,9.43000030517578 3.50999999046326,9.5600004196167 3.51999998092651,9.69999980926514 L2,9 L2,8.11999988555908 L3.51999998092651,8.98999977111816 C3.5,9.09000015258789 3.5,9.1899995803833 3.5,9.28999996185303 Z M3.75,10.6199998855591 L2.59999990463257,10.6199998855591 L2.24000000953674,10 L3.59000015258789,10.1000003814697 C3.63000011444092,10.2799997329712 3.6800000667572,10.4499998092651 3.75,10.6199998855591 Z M4.26999998092651,11.5699996948242 L3.5,11.8800001144409 L3,11.3400001525879 L4.01999998092651,11.1900005340576 C4.09000015258789,11.3199996948242 4.17999982833862,11.4499998092651 4.26999998092651,11.5699996948242 Z M4.78700017929077,12.125 L8.64599990844727,12.125 L6.89400005340576,9.07100009918213 L10.4790000915527,7.18000030517578 L10.4460000991821,7.13000011444092 C9.73600006103516,6.10300016403198 8.56700038909912,5.49000024795532 7.3100004196167,5.49000024795532 C5.20900011062622,5.48999977111816 3.5,7.19199991226196 3.5,9.28499984741211 C3.5,10.3780002593994 3.96700000762939,11.4029998779297 4.78700017929077,12.125 Z M8.36499977111816,11.625 L4.98299980163574,11.625 C4.35500001907349,11.0069999694824 4,10.1709995269775 4,9.28499984741211 C4,7.46799993515015 5.4850001335144,5.98999977111816 7.30999994277954,5.98999977111816 C8.23900032043457,5.98999977111816 9.11199951171875,6.375 9.73099994659424,7.03599977493286 L9.99600028991699,7.43499994277954 L10.9219999313354,6.94099998474121 L10.9019994735718,6.90999984741211 C10.1300001144409,5.75299978256226 8.8100004196167,4.98999977111816 7.30999994277954,4.98999977111816 C4.92999982833862,4.98999977111816 3,6.91300010681152 3,9.28499984741211 C3,10.6350002288818 3.6269998550415,11.8369998931885 4.60500001907349,12.625 L8.91499996185303,12.625 L8.36499977111816,11.625 Z M11.625,6 L6,9 L6.375,10 L12,9 L11.625,6 Z M8.11900043487549,11.1780004501343 C7.96700000762939,11.1409997940063 7.35699987411499,11 6.78999996185303,11 C6.05000019073486,11 5.56599998474121,11.1339998245239 5.24300003051758,11.3039999008179 C5.06199979782104,11.3990001678467 4.92799997329712,11.5590000152588 4.84200000762939,11.7449998855591 C4.59000015258789,12.2919998168945 4,13.5 4,13.5 C4,13.5 4.94999980926514,13 5.90000009536743,13 C7.32499980926514,13 8.27499961853027,13.5 8.27499961853027,13.5 L8.79799938201904,12.4119997024536 Z M6.625,9.5 L9.10000038146973,14 C9.10000038146973,14 10.246000289917,13.8760004043579 11,12.7560005187988 C11,12.7560005187988 10.246000289917,11.2640008926392 9.61800003051758,10.8910007476807 C9.27700042724609,10.6890001296997 8.57400035858154,10.3330001831055 8.36200046539307,10.1000003814697 C8.02299976348877,9.72599983215332 7.5460000038147,9.5 7.03399991989136,9.5 Z 
````
### **Alternatives**

For more in-depth SVG handling, there are some third-party components to consider, including:

-   [**DelphiSVG**](https://www.bverhue.nl/delphisvg/) - VCL, first released in 2015
    
-   [**RiverSoftAVG SVG Component Library**](http://riversoftavg.com/svg.htm) - VCL and FMX, first released 2013.11.17 by **Thomas Grubb**.
    
-   [**SVGIconImageList**](https://github.com/EtheaDev/SVGIconImageList) - An extended ImageList for Delphi (VCL+FMX) to simplify use of SVG Icons (with resize, opacity, grayscale...)
    
-   [**SVGImage**](https://github.com/ekot1/DelphiSVG) - VCL and FMX, first released in 2007 by **Martin Walter**.
    
-   [**SVGLite**](https://github.com/lamdalili/SVGLite) - VCL, leverages code from [**Graphics32**](https://github.com/graphics32/graphics32), new library with first GitHub commit 2019.10.19 suggested by [**Edwin Yip**](https://github.com/edwinyzh) (edwinyzh)
    
-   [**SVGMagic**](https://svgmagic.io/) - VCL, first released 2019.02.06
    
-   [**TMS VCL UI Pack**](https://tmssoftware.com/site/tmsvcluipack.asp) - VCL. Announced 2019.11.28 v10.1.0.0 in a [**blog post**](https://tmssoftware.com/site/blog.asp?post=605) SVG support was added.
    
-   [**TMS FNC Core**](https://www.tmssoftware.com/site/tmsfnccore.asp) - VCL, FMX, LCL, Web. Announced 2019.11.26 in a [**blog post**](https://www.tmssoftware.com/site/blog.asp?post=602).
    
-   [**TSVG**](https://sivv.com/ape) - FMX, first beta released 2012.01.21.
    
-   [**WPTools**](http://www.wpcubed.com/pdf/products/wptools/) - VCL word processor component that includes an embedded light weight SVG rendering engine as of 2019.10.22.
    
-   Update 2020.08.25: See Praxis [**message post**](https://en.delphipraxis.net/topic/3360-looking-for-svg-support-in-delphi/) on new developments
    

### **Summary**

Simple SVG based graphics/buttons are easy to use in your Firemonkey applications right out of the box without any third-party controls. If you have SVGs which are too complex for the built-in FireMonkey controls to handle, or you need SVG support in VCL applications, look into the growing number of component library alternatives.

On a related note, should you use FMX or VCL? See a blog post from [**Delphi.Org**](http://delphi.org/2016/10/firemonkey-vs-vcl/) dating back to late 2016 which is still relevant today. Both platforms are stronger and more feature rich today than when the blog post was released. And while you are there, check out his blog post on a more advanced usage of TPath including [**animation**](http://delphi.org/2019/04/animated-path-graphics-of-grace-hopper/).