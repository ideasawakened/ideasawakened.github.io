---
layout: post
title: "Utility class for activating external VCL Style files in Delphi"
date: 2020-06-23 12:00:00 +0000
last_modified_at: 2020-07-21 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [VCL Styles, iaLib]
permalink: post/utility-class-for-activating-external-vcl-style-files-in-delphi
published: true
image: /assets/blog/VCL-Styles/VCL-Styles-square.png
description: Delphi VCL Styles utility. Simple utility class to present a list of *.vsf style files to a user allowing them to pick one at runtime.
---
The [**VCL Styles**](http://docwiki.embarcadero.com/RADStudio/en/Working_with_VCL_Styles) feature in Delphi has seen major improvements as of late, including High DPI and 4K monitors in [**RAD Studio 10.4 Sydney**](https://www.embarcadero.com/products/rad-studio/whats-new-in-10-4-sydney). It seems like a rather niche feature for many Delphi developers, but it has strong potential now that most of the quirks have been worked out. They were first introduced way back in Delphi XE2 in 2011. (See a [**VCL Styles overview video on YouTube**](https://youtu.be/D2wnmm5nnjs) from the XE2 release.)

You can embed styles in your executable within the **Project Options->Application->Appearance** configuration screen. If you would rather keep your executable smaller, you can also ship styles in separate .vsf files and load the styles dynamically. I was doing this task with a few executables and decided to put together a simple class to reduce the small bit of duplicate code. The source is available in the [**iaLib GitHub repository**](https://github.com/ideasawakened/iaLib).

The **TiaVCLStyleManager** class interface section follows:

````pascal
  /// <summary>
  /// Typical usage is to populate a combo box within the form's OnCreate event via:  TiaVCLStyleManager.PopulateStyleList(cboYourComboBox.Items);
  /// And then set the combo's "OnChange" event to call:  TiaVCLStyleManager.HandleSelectionChange(xx
  /// </summary>
  TiaVCLStyleManager = class
  private const
    defSubdirectoryName = 'Themes';
  private
    class var fStyleFileList:TiaVCLStyleFileList;
    class var fStyleSearchPath:string;
    class var fStyleSearchSpec:string;
    class function GetVCLStyleFileList():TiaVCLStyleFileList; static;
  public
    class constructor CreateClass();
    class destructor DestroyClass();

    // main methods for end user selecting from a list of styles
    class procedure HandleSelectionChange(const pStyleList:TStrings; const pItemIndex:Integer);
    class procedure PopulateStyleList(const pDestinationList:TStrings; const pListSystemStyleFirst:Boolean = True);

    // support
    class function IsStyleLoaded(const pStyleName:string):Boolean;
    class function TrySetStyleFile(const pStyleFile:TiaVCLStyleFile; const pShowErrorDialog:Boolean = True):Boolean;

    // customizations
    class property StyleFileList:TiaVCLStyleFileList read GetVCLStyleFileList write fStyleFileList;
    class property StyleSearchPath:string read fStyleSearchPath write fStyleSearchPath;
    class property StyleSearchSpec:string read fStyleSearchSpec write fStyleSearchSpec;
  end;
````

This support class is designed to look for **\*.vsf** files in a folder and allow you to populate a TStrings list (as used by TComboBox or TListBox) with a list of available styles for a user to select from. The list will include the system default style ("Windows"), any embedded styles, and all the \*.vsf files found. The external styles are only loaded once they are set by the user.

There is a simple demo available in the same repo. Below is a screen shot of the main form:

![VCL Styles manager screenshot](/assets/blog/VCL-Styles/VCL-Styles-Manager-Demo.png)

Below is the source from the demo form:

````pascal
procedure TDemoForm.FormCreate(Sender:TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  // Customize path to style files (if needed) before populating the list
  // Search path defaults to IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Themes'
  // A 'VCLStyles' directory was added to the root folder in this GitHub repo
  TiaVCLStyleManager.StyleSearchPath := '..\..\..\..\VCLStyles';

  PopulateStyleLists();
end;


procedure TDemoForm.ResetSelection();
begin
  // simply keeps the two lists in-sync
  cboStyle.ItemIndex := cboStyle.Items.IndexOf(TStyleManager.ActiveStyle.Name);
  lstStyle.ItemIndex := lstStyle.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;


procedure TDemoForm.PopulateStyleLists();
begin
  // demo ComboBox usage
  TiaVCLStyleManager.PopulateStyleList(cboStyle.Items);

  // demo ListBox usage
  TiaVCLStyleManager.PopulateStyleList(lstStyle.Items);

  ResetSelection();
end;


procedure TDemoForm.cboStyleChange(Sender:TObject);
begin
  // demo ComboBox usage
  TiaVCLStyleManager.HandleSelectionChange(cboStyle.Items, cboStyle.ItemIndex);
  ResetSelection();
end;


procedure TDemoForm.lstStyleClick(Sender:TObject);
begin
  // demo ListBox usage
  TiaVCLStyleManager.HandleSelectionChange(lstStyle.Items, lstStyle.ItemIndex);
  ResetSelection();
end;


procedure TDemoForm.Exit1Click(Sender:TObject);
begin
  Close;
end;

````

You can see that when the form is created, the **StyleSearchPath** is customized. By default, the search path is set to a **Themes** subdirectory based on the executable's file path. I have included the VCL styles from 10.4 Sydney's redistributable folder in the **VCLStyles** folder within the GitHub repository, so I am specifying this custom directory in the FormCreate event.

I have included both a TComboBox and a TListBox in the demo form and those lists of styles are populated by calling the **TiaVCLStyleManager.PopulateStyleList** method. My preference is to list the "Windows" system style first in the list, while having the rest of the styles sorted alphabetically. You can override this behavior by passing an optional parameter in this method.

When a VCL style is selected by the user, the style is activated via the **TiaVCLStyleManager.HandleSelectionChange** method. Simply pass in the list from the control and the ItemIndex and the style will be loaded from disk if needed and the style activated.

By including this unit in your project, you can allow user-selectable styles with little code. Simply customize the **StyleSearchPath**, call the **PopulateStyleList** and then respond to the user's selection with the **HandleSelectionChange** method. To be honest, it is fairly easy to do without using this class, but if you find yourself cutting-n-pasting the same bit of code into separate projects then it really should be refactored into a stand-alone class. If you have not completed that task yet, then you may want to clone the iaLib repo and check out the demo project. If you always embed your VCL styles within your project, then you may not see much benefit from this utility class.

If you would like to see this small utility class expanded in some way, simply let me know and I will see what I can do as I plan on spending more time with VCL Styles in the near future. If you have not used VCL Styles in a while, then perhaps you should take another look. There were a few major issues that prevented me from using this feature in the past, but as of 10.4 Sydney all of my blocking issues seem to have been corrected and the feature has greatly matured. However, after looking through Quality Portal, we may need to wait for a patch or two to address a few new items:

-   [**RSP-29619 Window title bar issue with inherited forms**](https://quality.embarcadero.com/browse/RSP-29619) **Update:** This works 'as-expected' and this issue is now closed. _You need to assign a Control and set CustomTitleBar.Enabled to True. If no control is associated, the undefined result is expected._
    
-   [**RSP-29356 TFindDialog incorrect when moved from monitors with different dpi settings when VCL Style is active**](https://quality.embarcadero.com/browse/RSP-29356)
    
-   [**RSP-29287 Wrong text positioning for TButton with custom style**](https://quality.embarcadero.com/browse/RSP-29287) **Update:** There is a work-around detailed in RSP-29287 comments for editing Vcl.StdCtrls.pas.
    

If you have any other blocking issues, I would like to hear about them. Please send me a message on [**Twitter**](https://twitter.com/ideasawakened) or [**LinkedIn**](https://www.linkedin.com/in/darianm/).