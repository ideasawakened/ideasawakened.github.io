---
layout: post
title: "Double-dabble and conversions from base-10 and base-2 number systems"
date: 2020-09-28 12:00:00 +0000
last_modified_at: 2020-09-28 12:00:00 +0000
categories: [Delphi Programming]
tags: [Algorithms, Brian Thomson]
permalink: post/double-dabble-and-conversions-from-base-10-and-base-2-number-systems
published: true
image: /assets/blog/Algorithms/Base-Conversions/Convert-base2-to-base10-square.png
description: Convert to and from base-2 and base-10 numbers systems.  A quick base-2 binary conversion to base-10 via the double-dabble method is discussed.
---
Many of you probably use this trick all the time, but I haven't heard of this magic **double-dabble** short cut before today. I wanted to quickly write it down so that I don't forget it.

Base-10 is obviously the number system most often used in everyday life and it is completely engrained into your head so much so that you typically no longer break a number like **582** into its components: (5x100) + (8x10) + (2x1) Or, using the powers of 10 listed as a sum of weights: (5x10²) + (8x10¹) + (2x10⁰) **In the base-10 number system, we use the ten digits of 0 to 9 and each position is a power of ten, starting at 0**.

Base-2 is the number system that you use all the time while programming, and it's just another number system like base-10, but **in the base-2 number system, we use the two digits of 0 and 1 and each position is a power of two, starting at 0**. The decimal number 582 in base-2 is represented as 1001000110 and you can break it down via a sum of weights as: (1x2⁹) + (0x2⁸) + (0x2⁷) + (1x2⁶) + (0x2⁵) + (0x2⁴) + (0x2³) + (1x2²) + (1x2¹) + (0x2⁰) Due to **positional notation**, the Least Significant Bit (LSB) is the furthest on the right and the Most Significant Bit (MSB) is the largest value and is listed the furthest to the left. (This comes into play when sharing data between systems and there is a decision needed on which bit is listed first in the stream.) The LSB, also known as the low-order bit, can also be used to quickly determine if the number is Even or Odd.

**Note:** while working with different number systems, you should annotate the base using a subscript like: (582)₁₀ or (1001000110)₂ to avoid ambiguity when dealing with a sequence of digits like 1101 as it could be (1101)₁₀ or (1101)₂ or any other number base.

To quickly convert a base-10 number to base-2, you can use the **Repeated Division-by-2 method** by progressively dividing the number by 2 and then writing the remainder after each division, leaving the binary representation when read in reverse order. Let's convert (582)₁₀ using this method in the image below:

![Convert from base-10 to base-2](/assets/blog/Algorithms/Base-Conversions/Convert-From-Base10-To-Base2.png)

To manually convert from binary to decimal, I have been using the sum of weights method by calculating the powers of 2 for each digit's position. Thankfully, I stumbled across a quicker conversion method today. The **Double-Dabble method** works from the left to the right and Doubles the digit and then Adds the next digit, repeating until you reach the end. This simple method is demonstrated in the image below:

![Convert from base-2 to base-10](/assets/blog/Algorithms/Base-Conversions/Convert-base2-to-base10.png)

This quick double-dabble method will come in handy...if I remember it!

So now there is an easy way to convert to and from whole numbers, but what about fractional binary numbers like 0.24? There is a system for that as well and it uses multiplication instead of division as demonstrated in the image below.

![Convert fractional base10 to base2](/assets/blog/Algorithms/Base-Conversions/Convert-Fractional-base10-to-base2.png)

To convert a mixed number (a whole integer plus fractional number like 582.24) simply perform the two steps separately and combine the results together such as: (1001000110.00111101)₂ (**UPDATE**: corrected typo, previously listed incorrectly at 1001000110.0011101 based on post comment from **Brian Thomson**. He also reiterated that you need to continue this operation until desired accuracy achieved. My example of 8 fractional digits of 1001000110.00111101 converts to 582.23828125. This should be extended to something similar to his suggestion of 12 digits to the right of the binary separator: 1001000110.001111010111 which converts to a closer value of 582.239990234375. If you extend this to 23 digits to the right of the separator: 1001000110.00111101011100001010001 the number gets even closer: 582.23999989032745361328. This is a very good example of why a "simple" fractional number like 0.24 may not be as precise as you might assume.)

One final piece of the puzzle is missing - is there a shortcut method like double-dabble to convert fractional base-2 numbers back to base-10? Let me know!