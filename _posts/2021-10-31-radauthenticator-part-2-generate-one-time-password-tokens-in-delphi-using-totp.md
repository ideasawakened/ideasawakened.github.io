---
layout: post
title: "RADAuthenticator Part 2 - Generate one time password tokens in Delphi using TOTP"
date: 2021-10-23 12:00:00 +0000
last_modified_at: 2021-10-23 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Algorithms,RAD Programmer, rad-authenticator, Unit Testing]
permalink: post/radauthenticator-part-2-generate-one-time-password-tokens-in-delphi-using-totp
published: true
image: /assets/blog/Delphi-Programming/OTP-Calculation.png
description: "Generate Google Authenticator compatible (TOTP) one -time passwords in Delphi"
---
In [**part one**](https://www.ideasawakened.com/post/base32-encoding-in-delphi-for-google-authenticator-replacement-app-part-one-in-series) of this blog post series on an upcoming multi-platform RADAuthenticator Delphi app, we went over base32 encoding which is used for managing the secret key in Google Authenticator compatible one-time password apps (typically emedded within a QR Code link during setup.) In this blog post we will cover the heart of the process and that is to generate one-time use, dynamic time-based password tokens used by many applications for multi-factor authentication (MFA).

The **Time-Based One-Time Password Algorithm** (TOTP) is covered in [**RFC-6238**](https://datatracker.ietf.org/doc/html/rfc6238) which relies on the HMAC-Based One-Time Password Algorithm (HOTP) as defined in [**RFC-4226**](https://datatracker.ietf.org/doc/html/rfc4226).

![Example of calculating OTP](/assets/blog/Delphi-Programming/One-Time-Password-Generation-Example.png)

## TOTP Generation in Delphi

**From the RFC**

````
The output of the HMAC-SHA-1 calculation is truncated to obtain user-friendly values:

      HOTP(K,C) = Truncate(HMAC-SHA-1(K,C))

where Truncate represents the function that can convert an HMAC-SHA-1 value into an HOTP value.  K and C represent the shared secret and counter value. TOTP is the time-based variant of this algorithm, where a value T, derived from a time reference and a time step, replaces the counter C in the HOTP computation.
````

The counter value used for TOTP is an ever-increasing time value passed to the HOTP password generator. Since _the prover and verifier must always match input values_ this time value is expressed in UTC which bypasses any time-zone issues. The standard is to express this value in [**Unix time format**](https://en.wikipedia.org/wiki/Unix_time) and encoded as the number of seconds since midnight at the start of January 1, 1970. (This means that both sides need to have fairly accurate time, with the default verfication method allowing for 30 seconds of drift.)

Let us first look at the interface section of TOTP which is pretty minimal. There is an overloaded **GeneratePassword** function call which requires the Secret Key to be passed in and it returns a 6-digit result by default. (The routine allows you specify a 6, 7, or 8 digit response as needed.) There is also a protected function for retrieving the current timestamp to be used as a Counter Value for the HOTP generation.

````pascal
  TTOTP = class(THOTP)
  private const
    TimeStepWindow = 30; // 30 is recommended value. "The prover and verifier MUST use the same time-step"
  protected
    class function GetCurrentUnixTimestamp():Int64;
  public
    /// <summary> TOTP: Time-Based One-Time Password Algorithm (most commonly used by Google Authenticaor)</summary>
    class function GeneratePassword(const pBase32EncodedSecretKey:string; const pOutputLength:TOTPLength = TOTPLength.SixDigits):string; overload;
  end;
````

The implementation section of TOTP is also short and easy to follow. We are simply forwarding the GeneratePassword call to the underlying THOTP routines with the GetCurrentUnixTimeStamp value as the Counter Value parameter.

````pascal
class function TTOTP.GetCurrentUnixTimestamp():Int64;
begin
  Result := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)) div TTOTP.TimeStepWindow;
end;


// https://datatracker.ietf.org/doc/html/rfc6238
class function TTOTP.GeneratePassword(const pBase32EncodedSecretKey:string; const pOutputLength:TOTPLength = TOTPLength.SixDigits):string;
begin
  Result := THOTP.GeneratePassword(pBase32EncodedSecretKey, GetCurrentUnixTimestamp, pOutputLength);
end;
````

## HOTP Generation in Delphi

**From the RFC**

````
We can describe the operations in 3 distinct steps:

Step 1: Generate an HMAC-SHA-1 value Let HS = HMAC-SHA-1(K,C)  
Step 2: Generate a 4-byte string (Dynamic Truncation)
Step 3: Compute an HOTP value

The reason for masking the most significant bit of P is to avoid
confusion about signed vs. unsigned modulo computations.  Different
processors perform these operations differently, and masking out the
signed bit removes all ambiguity.

The following code example describes the extraction of a dynamic
binary code given that hmac_result is a byte array with the HMAC-
SHA-1 result:
     int offset   =  hmac_result[19] & 0xf ;
     int bin_code = (hmac_result[offset]  & 0x7f) << 24
        | (hmac_result[offset+1] & 0xff) << 16
        | (hmac_result[offset+2] & 0xff) <<  8
        | (hmac_result[offset+3] & 0xff) ;                          


````

The bulk of the code is in the HOTP class and we can see its interface section below. We define a custom Exception class which is thrown if the secret key is not long enough (which is defined as 'must be' 128 bits in the RFC) and we also allow for three different output lengths as specified in the RFC. There are two overloaded public **GeneratePassword** method calls taking the Secret Key and Counter Value as required inputs. There are a few private variables for use later in the code (**ModTable** allows for trimming of the Hash result based on output length desired, **FormatTable** is used for padding the string result based on desired length and a **RFCMinimumKeyLengthBytes** to specify the minimum length of the secret key.)

If you follow the industry standard convention of storing your Secret Keys as base32 encoded UTF-8 strings, use the first overloaded method and pass in the encoded password. Otherwise, pass the plain text Secret Key as an array of bytes using the second overloaded GeneratePassword method (keep in mind the rule that _the prover and verifier must always match input values_ so be careful that client and server use the same string encoding.)

````pascal
type

  EOTPException = class(Exception);


  // "Password generated must be at least 6, but can be 7 or 8" digits in length (simply changes the MOD operation) (9-digit addition suggested in errata: https://www.rfc-editor.org/errata/eid2400)
  TOTPLength = (SixDigits, SevenDigits, EightDigits);


  THOTP = class
  private const
    ModTable: array [0 .. 2] of integer = (1000000, 10000000, 100000000); // 6,7,8 zeros matching OTP Length
    FormatTable: array [0 .. 2] of string = ('%.6d', '%.7d', '%.8d'); // 6,7,8 string length (padded left with zeros)
    RFCMinimumKeyLengthBytes = 16; // length of shared secret MUST be 128 bits (16 bytes)
  public
    /// <summary> HOTP: HMAC-Based One-Time Password Algorithm</summary>
    class function GeneratePassword(const pBase32EncodedSecretKey:string; const pCounterValue:Int64; const pOutputLength:TOTPLength = TOTPLength.SixDigits):string; overload;
    class function GeneratePassword(const pPlainTextSecretKey:TBytes; const pCounterValue:Int64; const pOutputLength:TOTPLength = TOTPLength.SixDigits):string; overload;
  end;
````

As you can see, the first overloaded method simply converts the base32 encoded Secret Key string into an array of bytes and calls the main GeneratePassword method.

We need to calculate the [**HMAC**](https://datatracker.ietf.org/doc/html/rfc2104) of the Counter Value and Secret Key (SHA-1 is the most common variant used today and other hash types may be added in the future.) Four bytes of this 160-bit digest value is used as the OTP result and the RFC dictates how to extract that data. (We define an offset based on the last byte of the hash digest and then extract 4-bytes from the hash starting at that offset value.) We then truncate value to the length requested (via a [**mod**](https://docwiki.embarcadero.com/RADStudio/en/Expressions_(Delphi)) operation) and left-pad the integer result with zeros.

````pascal
class function THOTP.GeneratePassword(const pBase32EncodedSecretKey:string; const pCounterValue:Int64; const pOutputLength:TOTPLength = TOTPLength.SixDigits):string;
var
  vEncodedKey:TBytes;
  vDecodedKey:TBytes;
begin
  vEncodedKey := TEncoding.UTF8.GetBytes(pBase32EncodedSecretKey); // assume secret was stored as UTF8  (prover and verifier must match)
  vDecodedKey := TBase32.Decode(vEncodedKey);

  Result := GeneratePassword(vDecodedKey, pCounterValue, pOutputLength);
end;


// https://datatracker.ietf.org/doc/html/rfc4226
class function THOTP.GeneratePassword(const pPlainTextSecretKey:TBytes; const pCounterValue:Int64; const pOutputLength:TOTPLength = TOTPLength.SixDigits):string;
var
  vData:TBytes;
  vHMAC:TBytes;
  vOffset:integer;
  vBinCode:integer;
  vPinNumber:integer;
begin
  if Length(pPlainTextSecretKey) < RFCMinimumKeyLengthBytes then
  begin
    // RFC minimum length required  (Note: did not see this limitation in other implementations)
    raise EOTPException.CreateRes(@sOTPKeyLengthTooShort);
  end;
  vData := ReverseByteArray(ConvertToByteArray(pCounterValue)); // RFC reference implmentation reversed order of CounterValue (movingFactor) bytes
  vHMAC := THashSHA1.GetHMACAsBytes(vData, pPlainTextSecretKey); // SHA1 = 20 byte digest

  // rfc notes: extract a 4-byte dynamic binary integer code from the HMAC result
  vOffset := vHMAC[19] and $0F; // extract a random number 0 to 15 (from the value of the very last byte of the hash digest AND 0000-1111)

  // 4 bytes extracted starting at this random offset (first bit intentionally zero'ed to avoid compatibility problems with signed vs unsigned MOD operations)
  vBinCode := ((vHMAC[vOffset] and $7F) shl 24) // byte at offset AND 0111-1111 moved to first 8 bits of result
    or (vHMAC[vOffset + 1] shl 16) or (vHMAC[vOffset + 2] shl 8) or vHMAC[vOffset + 3];

  // trim 31-bit unsigned value to 6 to 8 digits in length
  vPinNumber := vBinCode mod THOTP.ModTable[Ord(pOutputLength)];

  // Format the 6 to 8 digit OTP result by padding left with zeros as needed
  Result := Format(FormatTable[Ord(pOutputLength)], [vPinNumber]);
end;
````

## OTP Unit Tests

The RFCs provides test vectors to validate our custom implementation which can easily be implemented in two DUnit tests as shown below:

````pascal
(*
  https://datatracker.ietf.org/doc/html/rfc4226
  Appendix D

 The following test data uses the ASCII string
   "12345678901234567890" for the secret:

   Secret = 0x3132333435363738393031323334353637383930

   Table 1 details for each count, the intermediate HMAC value.

   Count    Hexadecimal HMAC-SHA-1(secret, count)
   0        cc93cf18508d94934c64b65d8ba7667fb7cde4b0
   1        75a48a19d4cbe100644e8ac1397eea747a2d33ab
   2        0bacb7fa082fef30782211938bc1c5e70416ff44
   3        66c28227d03a2d5529262ff016a1e6ef76557ece
   4        a904c900a64b35909874b33e61c5938a8e15ed1c
   5        a37e783d7b7233c083d4f62926c7a25f238d0316
   6        bc9cd28561042c83f219324d3c607256c03272ae
   7        a4fb960c0bc06e1eabb804e5b397cdc4b45596fa
   8        1b3c89f65e6c9e883012052823443f048b4332db
   9        1637409809a679dc698207310c8c7fc07290d9e5

   Table 2 details for each count the truncated values (both in
   hexadecimal and decimal) and then the HOTP value.

                     Truncated
   Count    Hexadecimal    Decimal        HOTP
   0        4c93cf18       1284755224     755224
   1        41397eea       1094287082     287082
   2         82fef30        137359152     359152
   3        66ef7655       1726969429     969429
   4        61c5938a       1640338314     338314
   5        33c083d4        868254676     254676
   6        7256c032       1918287922     287922
   7         4e5b397         82162583     162583
   8        2823443f        673399871     399871
   9        2679dc69        645520489     520489
*)
procedure THOTPTest.TestRFCVectors;
const
  SECRET_PLAINTEXT_BYTES:TBytes = [49, 50, 51, 52, 53, 54, 55, 56, 57, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 48]; //'12345678901234567890'
  SECRET_BASE32_STRING = 'GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ'; // TBase32.Encode('12345678901234567890');
  EXPECTED_VALUES: array [0 .. 9] of string = ('755224', '287082', '359152', '969429', '338314', '254676', '287922', '162583', '399871', '520489');
var
  i:integer;
begin
  for i := low(EXPECTED_VALUES) to high(EXPECTED_VALUES) do
  begin
    CheckEquals(EXPECTED_VALUES[i], THOTP.GeneratePassword(SECRET_PLAINTEXT_BYTES, i));
    CheckEquals(EXPECTED_VALUES[i], THOTP.GeneratePassword(SECRET_BASE32_STRING, i));
  end;
end;
````

````pascal
(*
 https://datatracker.ietf.org/doc/html/rfc6238
 Appendix B

 The test token shared secret uses the ASCII string value
   "12345678901234567890".  With Time Step X = 30, and the Unix epoch as
   the initial value to count time steps, where T0 = 0, the TOTP
   algorithm will display the following values for specified modes and
   timestamps.

  +-------------+--------------+------------------+----------+--------+
  |  Time (sec) |   UTC Time   | Value of T (hex) |   TOTP   |  Mode  |
  +-------------+--------------+------------------+----------+--------+
  |      59     |  1970-01-01  | 0000000000000001 | 94287082 |  SHA1  |
  |             |   00:00:59   |                  |          |        |
  |      59     |  1970-01-01  | 0000000000000001 | 46119246 | SHA256 |
  |             |   00:00:59   |                  |          |        |
  |      59     |  1970-01-01  | 0000000000000001 | 90693936 | SHA512 |
  |             |   00:00:59   |                  |          |        |
  |  1111111109 |  2005-03-18  | 00000000023523EC | 07081804 |  SHA1  |
  |             |   01:58:29   |                  |          |        |
  |  1111111109 |  2005-03-18  | 00000000023523EC | 68084774 | SHA256 |
  |             |   01:58:29   |                  |          |        |
  |  1111111109 |  2005-03-18  | 00000000023523EC | 25091201 | SHA512 |
  |             |   01:58:29   |                  |          |        |
  |  1111111111 |  2005-03-18  | 00000000023523ED | 14050471 |  SHA1  |
  |             |   01:58:31   |                  |          |        |
  |  1111111111 |  2005-03-18  | 00000000023523ED | 67062674 | SHA256 |
  |             |   01:58:31   |                  |          |        |
  |  1111111111 |  2005-03-18  | 00000000023523ED | 99943326 | SHA512 |
  |             |   01:58:31   |                  |          |        |
  |  1234567890 |  2009-02-13  | 000000000273EF07 | 89005924 |  SHA1  |
  |             |   23:31:30   |                  |          |        |
  |  1234567890 |  2009-02-13  | 000000000273EF07 | 91819424 | SHA256 |
  |             |   23:31:30   |                  |          |        |
  |  1234567890 |  2009-02-13  | 000000000273EF07 | 93441116 | SHA512 |
  |             |   23:31:30   |                  |          |        |
  |  2000000000 |  2033-05-18  | 0000000003F940AA | 69279037 |  SHA1  |
  |             |   03:33:20   |                  |          |        |
  |  2000000000 |  2033-05-18  | 0000000003F940AA | 90698825 | SHA256 |
  |             |   03:33:20   |                  |          |        |
  |  2000000000 |  2033-05-18  | 0000000003F940AA | 38618901 | SHA512 |
  |             |   03:33:20   |                  |          |        |
  | 20000000000 |  2603-10-11  | 0000000027BC86AA | 65353130 |  SHA1  |
  |             |   11:33:20   |                  |          |        |
  | 20000000000 |  2603-10-11  | 0000000027BC86AA | 77737706 | SHA256 |
  |             |   11:33:20   |                  |          |        |
  | 20000000000 |  2603-10-11  | 0000000027BC86AA | 47863826 | SHA512 |
  |             |   11:33:20   |                  |          |        |
  +-------------+--------------+------------------+----------+--------+
*)
procedure TTOTPTest.TestRFCVectors;
const
  SECRET = 'GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ'; // TBase32.Encode('12345678901234567890');
  INPUT_VALUES: array [0 .. 5] of Int64 = ($1, $23523EC, $23523ED, $273EF07, $3F940AA, $27BC86AA);
  EXPECTED_8DIGIT_VALUES: array [0 .. 5] of string = ('94287082', '07081804', '14050471', '89005924', '69279037', '65353130');
  EXPECTED_7DIGIT_VALUES: array [0 .. 5] of string = ('4287082', '7081804', '4050471', '9005924', '9279037', '5353130');
  EXPECTED_6DIGIT_VALUES: array [0 .. 5] of string = ('287082', '081804', '050471', '005924', '279037', '353130');
var
  i:integer;
begin
  for i := low(INPUT_VALUES) to high(INPUT_VALUES) do
  begin
    CheckEquals(EXPECTED_8DIGIT_VALUES[i], TTOTP.GeneratePassword(SECRET, INPUT_VALUES[i], TOTPLength.EightDigits));
    CheckEquals(EXPECTED_7DIGIT_VALUES[i], TTOTP.GeneratePassword(SECRET, INPUT_VALUES[i], TOTPLength.SevenDigits));
    CheckEquals(EXPECTED_6DIGIT_VALUES[i], TTOTP.GeneratePassword(SECRET, INPUT_VALUES[i], TOTPLength.SixDigits));
  end;
end;
````

## Next Steps

Combined with the base32 encoding from the [**first article in the series**](https://www.ideasawakened.com/post/base32-encoding-in-delphi-for-google-authenticator-replacement-app-part-one-in-series), we now have everything needed to implement one-time passwords in Delphi! This code will continue to be used in this blog series dedicated to building a custom **RADAuthenticator** app intended to replace the Google Authenticator app. Part 3 will focus on extending the testing platform.

For proof of concept purposes, there is a sample VCL app provided which takes a Secret Key for input and outputs the One Time Password value using the TOTP routines defined above.

The code is released as Open Source under the [**Apache-2.0 license**](https://www.apache.org/licenses/LICENSE-2.0) and is found within the [**rad-authenticator**](https://github.com/radprogrammer/rad-authenticator) repository under my **RADProgrammer** organization on GitHub.

I have setup a RADProgrammer chat space on Discord dedicated to RADProgrammer projects such as this. Here is an [**invitation link**](https://discord.gg/GmZwcAmAT7) to join this Discord. **See you online!**