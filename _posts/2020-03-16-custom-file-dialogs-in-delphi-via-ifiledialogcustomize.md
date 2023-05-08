---
layout: post
title: "Custom File Dialogs in Delphi via IFileDialogCustomize"
date: 2020-03-16 12:00:00 +0000
last_modified_at: 2020-03-16 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Microsoft Windows, DelphiKB, Eric Harmon]
permalink: post/custom-file-dialogs-in-delphi-via-ifiledialogcustomize
published: true
image: /assets/blog/Microsoft-Windows/IFileDialogCustomize/IFileDialogCustomize_square.png
description: Blog post (along with sample GitHub project) demonstrating using Microsoft's IFileDialogCustomize interface to add custom controls to File Dialogs
---
While today's programmers are typically excited about new browser-based technology, the _old school_ Component Object Model (COM) in Windows is still leveraged by a very large portion of business software today. Over the years, Microsoft has developed a very large and extendable framework for Windows developers to use, and as one of its many core features, Delphi makes leveraging COM technology much simpler to use. In fact, it's probably easier to use COM in Delphi than any other dev tool on the market, including anything available from Microsoft.

In this article, we are going to quickly cover one of these published COM-based interfaces: **IFIleDialogCustomize**. According to the [**docs**](https://docs.microsoft.com/en-us/windows/win32/api/shobjidl_core/nn-shobjidl_core-ifiledialogcustomize) from Microsoft, this interface:

> Exposes methods that allow an application to add controls to a common file dialog.

> Controls are added to the dialog before the dialog is shown. Their layout is implied by the order in which they are added. Once the dialog is shown, controls cannot be added or removed, but the existing controls can be hidden or disabled at any time. Their labels can also be changed at any time.

By using Delphi, it's simple to leverage this interface to add custom controls to the standard system dialogs. I have added a new [**example project on GitHub**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Demos/IFileDialogCustomize) to demonstrate this. The project was built with Delphi 10.3.3 but the custom code should be backwards-compatible to most older versions.

This screenshot comes from running this sample project and clicking on the "Test OpenFile" button:

![Screenshot from example project demonstrating IFileDialogCustomize](/assets/blog/Microsoft-Windows/IFileDialogCustomize/IFileDialogCustomize_ScreenShot.png)

The typical pattern of usage for a TFileOpenDialog is to toss it on the form, set the desired FileTypes, and respond to some button OnClick event with simple code reacting to `if MyDialog.Execute()`

To customize the dialog, you continue to perform these same steps, but you also need to introduce a few new actions as described below.

### OnExecute Event:

You'll want to create an OnExecute event to add any custom controls to the File Dialog before it is displayed. In this example, I've added a single ComboBox populated with three choices. Note that most custom controls are 'containers' and they have a description when created. This description ends up being a label next to the container:

````pascal
//let's customize the FileOpen Dialog by adding a custom combobox
procedure TForm1.ExampleDialogExecute(Sender: TObject);
var
  iCustomize:IFileDialogCustomize;
  vItem:string;
  i:Integer;
begin
  if ExampleDialog.Dialog.QueryInterface(IFileDialogCustomize, iCustomize) = S_OK then
  begin
    iCustomize.StartVisualGroup(0, 'Custom description here');
    try
      //note other controls available: AddCheckButton, AddEditBox, AddPushButton, AddRadioButtonList...
      iCustomize.AddComboBox(CUSTOM_CONTROL_ID);

      for i := 0 to lstCustomItems.Items.Count - 1 do
      begin
        vItem := lstCustomItems.Items[i];
        //+1 note: If user doesn't select an item, the result is 0 so lets start indexed items with 1
        iCustomize.AddControlItem(CUSTOM_CONTROL_ID, i+1, PChar(vItem));
      end;

      if lstCustomItems.ItemIndex >= 0 then //let's optionally pre-select a custom item
      begin
        iCustomize.SetSelectedControlItem(CUSTOM_CONTROL_ID, lstCustomItems.ItemIndex+1);
      end;
    finally
      iCustomize.EndVisualGroup;
    end;
  end;
end;
````

### OnFileOkClick Event:

This event is executed when the user clicks on the OK button on the File Dialog. There is a boolean **CanClose** parameter that you can set to cancel the action, if desired.

You'll want to grab the custom control's selected item and cancel the event or save this value somewhere. Since there's only one custom control in this example, and it has a simple numeric result, I've chosen to use the Dialog's .Tag property to store the user's selection:

````pascal
//Grab the custom control's selection
procedure TForm1.ExampleDialogFileOkClick(Sender: TObject; var CanClose: Boolean);
var
  iCustomize: IFileDialogCustomize;
  vSelectedIndex:DWORD;
begin
  if ExampleDialog.Dialog.QueryInterface(IFileDialogCustomize, iCustomize) = S_OK then
  begin
    vSelectedIndex := 0;
    iCustomize.GetSelectedControlItem(CUSTOM_CONTROL_ID, vSelectedIndex);
    //simple way to store user's selection is to use the .Tag property
    ExampleDialog.Tag := vSelectedIndex;
  end;
end;
````

### Modified **Execute** response

With the two additional events above, we've added a custom combo box and set the user's selection to the .Tag property. Now you simply have to adjust your standard .Execute code to utilize this selected value. In this demo project, we are simply displaying the selection:

````pascal
//Standard OnClick event except manage custom control's selection result via the .Tag property
procedure TForm1.butOpenFileClick(Sender: TObject);
begin
  ExampleDialog.Tag := 0; //reset on use
  if ExampleDialog.Execute(self.Handle) then
  begin
    ShowMessage(Format('You selected custom item index %d' + sLineBreak
                       + 'When opening file: %s', [ExampleDialog.Tag, ExampleDialog.FileName]));
  end;
````

### Additional Notes from Microsoft's documentation:

> Container controls are controls that can have items added to them. Container controls include combo boxes, menus, the drop-down list attached to the Open button, and any option button groups. The order that items appear in a container is the order in which they were added. There is no facility for reordering them. IDs are scoped to the parent control. Container controls, with the exception of menus, have a selected item.

> Items with a container control cannot be changed after they have been created, except for their enabled and visible states. However, they can be added and removed at any time. For example, if you needed to change the text of a menu, you would have to remove the current menu and add another with the correct text.

**IFileDialogCustomize** is available for Windows Vista or later operating systems. The more precise developers will want to check each of the IFileDialogCustomize method calls for their [**HRESULT**](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/705fb797-2175-4a90-b5a3-3918024b10b8) return value (typically with **OleCheck**) I left that out to make the code easier to read.

### **Summary**

Using Delphi, you can easily add custom controls to the system File Dialogs via the IFileDialogCustomize COM interface. One example usage would be to request the Encoding choice from the user when opening text files. Delphi actually provides a component specifically for this task in [**TOpenTextFileDialog**](http://docwiki.embarcadero.com/Libraries/en/Vcl.ExtDlgs.TOpenTextFileDialog), but it can utilize the old pre-Vista file dialog. (See this [**StackOverflow discussion**](https://stackoverflow.com/questions/6236275/what-is-the-difference-between-the-new-tfileopendialog-and-the-old-topendialog) on the differences between the older TOpenDialog and newer TFileOpenDialog.)

### **For further reading**

[**Delphi COM Programming**](https://www.amazon.com/Delphi-COM-Programming-Eric-Harmon/dp/1578702216) is a book by **Eric Harmon** which is the defacto reference guide for COM and Delphi.

![Delphi COM Programming book cover](/assets/blog/Books/Delphi-COM-Programming-BookCover.jpg)
