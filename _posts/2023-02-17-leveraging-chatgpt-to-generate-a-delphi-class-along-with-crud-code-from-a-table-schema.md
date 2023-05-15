---
layout: post
title: "Leveraging ChatGPT to generate a Delphi class along with CRUD code from a table schema"
date: 2023-02-17 12:00:00 +0000
last_modified_at: 2023-02-17 12:00:00 +0000
categories: [Tooling, AI]
tags: [ChatGPT, Unit Testing]
permalink: post/leveraging-chatgpt-to-generate-a-delphi-class-along-with-crud-code-from-a-table-schema
published: true
image: /assets/blog/Delphi-Programming/Delphi-ChatGPT-small.png
description: 'Example of automatic Delphi code generation by ChatGPT.  Provide SQL schema and get a working Delphi class with CRUD functionality.'
---
The more we use ChatGPT, the more we find uses for it to solve common programming problems. In a [**previous post**](https://www.ideasawakened.com/post/creating-unit-tests-for-delphi-with-chatgpt), we saw how ChatGPT could generate Unit Tests for a known Delphi class. (You can also pass in a custom written Delphi class and have it generate the unit tests.)

In this post, we look into another useful code generation feature of ChatGPT. You can provide the schema of a database table and ask ChatGPT to generate the corresponding support class in Delphi. In this example, we provide ChatGPT with the following SQLite database schema for a TestTable:

````sql
CREATE TABLE "TestTable" (
	"MyID" INTEGER NOT NULL,
	"ItemName" TEXT NOT NULL,
	"ItemValue" TEXT NOT NULL,
	PRIMARY KEY ("MyID")
);
````

We then politely ask ChatGPT: "**Please create a Delphi class that represents this table's values.**" and include the schema above. This is the initial output:

>Certainly! Here is an example Delphi class that represents the field values of the "TestTable" table:

````pascal
unit TestTable;

interface

type
  TTestTable = class
  private
    FMyID: Integer;
    FItemName: string;
    FItemValue: string;
  public
    property MyID: Integer read FMyID write FMyID;
    property ItemName: string read FItemName write FItemName;
    property ItemValue: string read FItemValue write FItemValue;
  end;

implementation

end.
````

The response includes the following explanation:
>This class has private fields that correspond to the fields in the "TestTable" table, and public properties that provide access to those fields. Note that the "MyID" field has been marked as the primary key of the table.


This process took a few seconds and is handy with the example three fields and would be _very handy_ when you have a few dozen!

Now, we expand on the code generation by asking ChatGPT a follow-up: "**Please expand the TTestTable class to allow for inserting into the TestTable, assuming a SQLite table using FireDAC components.**" and this is the expanded output:

>Sure, here's an expanded version of the TTestTable class that includes a method to insert a new row into the "TestTable" table using FireDAC components:

````pascal
unit TestTable;

interface

uses
  FireDAC.Comp.Client;

type
  TTestTable = class
  private
    FMyID: Integer;
    FItemName: string;
    FItemValue: string;
  public
    property MyID: Integer read FMyID write FMyID;
    property ItemName: string read FItemName write FItemName;
    property ItemValue: string read FItemValue write FItemValue;

    function InsertIntoTable(Connection: TFDConnection): Boolean;
  end;

implementation

{ TTestTable }

function TTestTable.InsertIntoTable(Connection: TFDConnection): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'INSERT INTO TestTable(MyID, ItemName, ItemValue) ' +
      'VALUES(:MyID, :ItemName, :ItemValue)';
    Query.Params.ParamByName('MyID').AsInteger := FMyID;
    Query.Params.ParamByName('ItemName').AsString := FItemName;
    Query.Params.ParamByName('ItemValue').AsString := FItemValue;
    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

end.
````

>This version of the class includes a method called InsertIntoTable that takes a TFDConnection object as a parameter. The method creates a new TFDQuery object, sets its connection property to the provided connection object, and sets its SQL property to an INSERT INTO statement that inserts a new row into the "TestTable" table with the values of the class's fields. The method then sets the query's parameters to the values of the class's fields, executes the query using the ExecSQL method, and returns True to indicate success.

That is certainly a time saver! How about extending it for Update operations? "**Please extend the class to include an UpdateTable method.**"

>Sure, here's the updated TTestTable class with an UpdateTable method that updates an existing row in the "TestTable" table:

````pascal
unit TestTable;

interface

uses
  FireDAC.Comp.Client;

type
  TTestTable = class
  private
    FMyID: Integer;
    FItemName: string;
    FItemValue: string;
  public
    property MyID: Integer read FMyID write FMyID;
    property ItemName: string read FItemName write FItemName;
    property ItemValue: string read FItemValue write FItemValue;

    function InsertIntoTable(Connection: TFDConnection): Boolean;
    function UpdateTable(Connection: TFDConnection): Boolean;
  end;

implementation

{ TTestTable }

function TTestTable.InsertIntoTable(Connection: TFDConnection): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'INSERT INTO TestTable(MyID, ItemName, ItemValue) ' +
      'VALUES(:MyID, :ItemName, :ItemValue)';
    Query.Params.ParamByName('MyID').AsInteger := FMyID;
    Query.Params.ParamByName('ItemName').AsString := FItemName;
    Query.Params.ParamByName('ItemValue').AsString := FItemValue;
    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

function TTestTable.UpdateTable(Connection: TFDConnection): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'UPDATE TestTable ' +
      'SET ItemName = :ItemName, ' +
      '    ItemValue = :ItemValue ' +
      'WHERE MyID = :MyID';
    Query.Params.ParamByName('ItemName').AsString := FItemName;
    Query.Params.ParamByName('ItemValue').AsString := FItemValue;
    Query.Params.ParamByName('MyID').AsInteger := FMyID;
    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

end.
````

> The UpdateTable method is similar to the InsertIntoTable method, except that it uses an UPDATE statement to modify an existing row in the table instead of an INSERT INTO statement to add a new row. The method sets the query's parameters to the values of the class's fields, executes the query using the ExecSQL method, and returns True to indicate success.

We are getting closer, but lets see how it takes some criticism... "**ExecSQL returns a LongInt value and I think the UpdateTable function should return that instead of a Boolean.**"

>Yes, you are correct. The ExecSQL method returns the number of rows affected by the query as a LongInt value. So, the UpdateTable function should return that value instead of a boolean to indicate the number of rows updated.

> Here is the updated TTestTable class with the UpdateTable method returning the LongInt value:

````pascal
unit TestTable;

interface

uses
  FireDAC.Comp.Client;

type
  TTestTable = class
  private
    FMyID: Integer;
    FItemName: string;
    FItemValue: string;
  public
    property MyID: Integer read FMyID write FMyID;
    property ItemName: string read FItemName write FItemName;
    property ItemValue: string read FItemValue write FItemValue;

    function InsertIntoTable(Connection: TFDConnection): Boolean;
    function UpdateTable(Connection: TFDConnection): LongInt;
  end;

implementation

{ TTestTable }

function TTestTable.InsertIntoTable(Connection: TFDConnection): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'INSERT INTO TestTable(MyID, ItemName, ItemValue) ' +
      'VALUES(:MyID, :ItemName, :ItemValue)';
    Query.Params.ParamByName('MyID').AsInteger := FMyID;
    Query.Params.ParamByName('ItemName').AsString := FItemName;
    Query.Params.ParamByName('ItemValue').AsString := FItemValue;
    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

function TTestTable.UpdateTable(Connection: TFDConnection): LongInt;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'UPDATE TestTable ' +
      'SET ItemName = :ItemName, ' +
      '    ItemValue = :ItemValue ' +
      'WHERE MyID = :MyID';
    Query.Params.ParamByName('ItemName').AsString := FItemName;
    Query.Params.ParamByName('ItemValue').AsString := FItemValue;
    Query.Params.ParamByName('MyID').AsInteger := FMyID;
    Result := Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
````

>The UpdateTable method now returns the value returned by Query.ExecSQL, which is the number of rows affected by the query.


For now, this is free to use and we are not paying by the generated word... so why not take advantage by extending the class further? "**I think one more method is needed in this TTestTable class to make it more useful. Please add a method that you pass in the desired MyID value and it looks up and sets the class fields to the current values from the table. The return value of the new function call should be True if the value is found and False if not. If not found, the fields should be set to initial values.**"

>You're right, adding a method to fetch values from the table based on a given MyID would be useful. Here is an updated version of TTestTable with the new method GetValuesFromTable:

````pascal
unit TestTable;

interface

uses
  FireDAC.Comp.Client;

type
  TTestTable = class
  private
    FMyID: Integer;
    FItemName: string;
    FItemValue: string;
  public
    property MyID: Integer read FMyID write FMyID;
    property ItemName: string read FItemName write FItemName;
    property ItemValue: string read FItemValue write FItemValue;

    function InsertIntoTable(Connection: TFDConnection): Boolean;
    function UpdateTable(Connection: TFDConnection): LongInt;
    function GetValuesFromTable(Connection: TFDConnection; MyID: Integer): Boolean;
  end;

implementation

{ TTestTable }

function TTestTable.InsertIntoTable(Connection: TFDConnection): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'INSERT INTO TestTable(MyID, ItemName, ItemValue) ' +
      'VALUES(:MyID, :ItemName, :ItemValue)';
    Query.Params.ParamByName('MyID').AsInteger := FMyID;
    Query.Params.ParamByName('ItemName').AsString := FItemName;
    Query.Params.ParamByName('ItemValue').AsString := FItemValue;
    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

function TTestTable.UpdateTable(Connection: TFDConnection): LongInt;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'UPDATE TestTable ' +
      'SET ItemName = :ItemName, ' +
      '    ItemValue = :ItemValue ' +
      'WHERE MyID = :MyID';
    Query.Params.ParamByName('ItemName').AsString := FItemName;
    Query.Params.ParamByName('ItemValue').AsString := FItemValue;
    Query.Params.ParamByName('MyID').AsInteger := FMyID;
    Result := Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TTestTable.GetValuesFromTable(Connection: TFDConnection; MyID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'SELECT ItemName, ItemValue ' +
      'FROM TestTable ' +
      'WHERE MyID = :MyID';
    Query.Params.ParamByName('MyID').AsInteger := MyID;
    Query.Open;
    if not Query.IsEmpty then
    begin
      FMyID := MyID;
      FItemName := Query.FieldByName('ItemName').AsString;
      FItemValue := Query.FieldByName('ItemValue').AsString;
      Result := True;
    end
    else
    begin
      FMyID := 0;
      FItemName := '';
      FItemValue := '';
    end;
  finally
    Query.Free;
  end;
end;

end.
````

> The new method GetValuesFromTable takes in a MyID parameter and sets the class fields FMyID, FItemName, and FItemValue to the corresponding values from the table if a row with that `MyID

Being curious, I then asked "**Are there any other improvements that you would suggest adding to the TTestTable class?**"

````
Here are a few suggestions for further improvements to the TTestTable class:

Add a method to delete a row from the table. This can be done using the DELETE SQL statement, similarly to the UpdateTable method. The method could take the MyID value as an input parameter, and return a Boolean indicating whether the deletion was successful.

Add properties for each field that automatically update the corresponding SQL statement when the value is set. For example, setting the ItemName property would automatically update the UPDATE and INSERT statements to include the new value. This would make the code easier to maintain and reduce the risk of errors.

Add support for more data types. The current implementation only supports Integer and String values. You could extend the class to support other common data types, such as Boolean, DateTime, and Float.

Add support for more advanced SQL features, such as joins and subqueries. This would make the class more flexible and powerful, and would allow more complex database operations to be performed without leaving the Delphi application.

Add support for transactions. Transactions allow multiple SQL statements to be grouped together into a single atomic unit of work, so that either all the statements are executed successfully, or none of them are. This can be important in applications where data integrity is critical, as it helps to prevent data corruption and other problems.
````

Let's take ChatGPT on its offer and ask for a Delete method and also ask for even more functionality. "**Let's add two more features to the TTestClass. First, add your suggested Delete method. Second, add a "CreateTableIfNotFound" method that your pass in the TFDConnection and the table is created if it does not exist in the database. Those two items would help make this class very useful.**"

>Sure, here is the updated TTestTable class with the Delete method and the CreateTableIfNotFound method:

````pascal
unit TestTable;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Param, FireDAC.Comp.Client;

type
  TTestTable = class
  private
    FMyID: Integer;
    FItemName: string;
    FItemValue: string;
    FConnection: TFDConnection;
    FUpdateSQL: TFDQuery;
    FInsertSQL: TFDQuery;
    FSelectSQL: TFDQuery;
    FDeleteSQL: TFDQuery;
    function ExecuteSQL(SQL: TFDQuery): Boolean;
  public
    constructor Create(AConnection: TFDConnection);
    function InsertIntoTable: Boolean;
    function UpdateTable: Boolean;
    function Delete(MyID: Integer): Boolean;
    function LookupByID(MyID: Integer): Boolean;
    procedure CreateTableIfNotFound;
    property MyID: Integer read FMyID write FMyID;
    property ItemName: string read FItemName write FItemName;
    property ItemValue: string read FItemValue write FItemValue;
  end;

implementation

{ TTestTable }

constructor TTestTable.Create(AConnection: TFDConnection);
begin
  FMyID := 0;
  FItemName := '';
  FItemValue := '';
  FConnection := AConnection;
  FUpdateSQL := TFDQuery.Create(nil);
  FUpdateSQL.Connection := FConnection;
  FUpdateSQL.SQL.Text := 'UPDATE TestTable SET ItemName = :ItemName, ItemValue = :ItemValue WHERE MyID = :MyID';
  FUpdateSQL.ParamByName('ItemName').DataType := ftString;
  FUpdateSQL.ParamByName('ItemValue').DataType := ftString;
  FUpdateSQL.ParamByName('MyID').DataType := ftInteger;

  FInsertSQL := TFDQuery.Create(nil);
  FInsertSQL.Connection := FConnection;
  FInsertSQL.SQL.Text := 'INSERT INTO TestTable (ItemName, ItemValue) VALUES (:ItemName, :ItemValue)';
  FInsertSQL.ParamByName('ItemName').DataType := ftString;
  FInsertSQL.ParamByName('ItemValue').DataType := ftString;

  FSelectSQL := TFDQuery.Create(nil);
  FSelectSQL.Connection := FConnection;
  FSelectSQL.SQL.Text := 'SELECT ItemName, ItemValue FROM TestTable WHERE MyID = :MyID';
  FSelectSQL.ParamByName('MyID').DataType := ftInteger;

  FDeleteSQL := TFDQuery.Create(nil);
  FDeleteSQL.Connection := FConnection;
  FDeleteSQL.SQL.Text := 'DELETE FROM TestTable WHERE MyID = :MyID';
  FDeleteSQL.ParamByName('MyID').DataType := ftInteger;
end;

function TTestTable.ExecuteSQL(SQL: TFDQuery): Boolean;
begin
  SQL.Prepare;
  SQL.ParamByName('MyID').AsInteger := FMyID;
  SQL.ParamByName('ItemName').AsString := FItemName;
  SQL.ParamByName('ItemValue').AsString := FItemValue;
  try
    SQL.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

function TTestTable.InsertIntoTable: Boolean;
begin
  FSelectSQL.Prepare;
  FSelectSQL.ParamByName('MyID').AsInteger := FMyID;
  FSelectSQL.Open;
  if not FSelectSQL.Eof then
  begin
    Result := False;
  end
  else
  begin
    Result := ExecuteSQL(FInsertSQL);
  end;
end;

function T
````

Note that the code generation timed out after this request. One simple reaction to that situation that I have found to be useful is to type "**continue**" and hit enter which usually allows Chat GPT to keep generating its response. However, in this case, even after repeating the code generation, the response was again truncated... But, interestingly, the code generation was slightly different. The previous truncated codegen included **unsolicited optimizations** where the Insert/Update/Delete/Select queries were created within the constructor. This next truncated version included different unsolicited optimizations by including new "SetInitialValues" and "GetNextMyID" helper methods. It also added some validation on the "UpdateTable" method by checking to see if any of the fields were equal to their default values and prevented the update if that situation (which is wrong.)

````pascal
unit TestTableUnit;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client;

type
  TTestTable = class
  private
    FMyID: Integer;
    FItemName: string;
    FItemValue: string;
    FConnection: TFDConnection;
    function GetNextMyID: Integer;
    procedure SetInitialValues;
  public
    constructor Create(AConnection: TFDConnection);
    function InsertIntoTable: LongInt;
    function UpdateTable: LongInt;
    function DeleteFromTable: LongInt;
    function GetFromTable(MyID: Integer): Boolean;
    function CreateTableIfNotFound: Boolean;
    property MyID: Integer read FMyID write FMyID;
    property ItemName: string read FItemName write FItemName;
    property ItemValue: string read FItemValue write FItemValue;
  end;

implementation

constructor TTestTable.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
  SetInitialValues;
end;

function TTestTable.GetNextMyID: Integer;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT MAX(MyID) FROM TestTable';
    Query.Open;
    Result := Query.Fields[0].AsInteger + 1;
  finally
    Query.Free;
  end;
end;

function TTestTable.CreateTableIfNotFound: Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'CREATE TABLE IF NOT EXISTS TestTable (' +
      'MyID INTEGER NOT NULL,' +
      'ItemName TEXT NOT NULL,' +
      'ItemValue TEXT NOT NULL,' +
      'PRIMARY KEY (MyID)' +
      ')';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TTestTable.SetInitialValues;
begin
  FMyID := -1;
  FItemName := '';
  FItemValue := '';
end;

function TTestTable.InsertIntoTable: LongInt;
var
  Query: TFDQuery;
begin
  Result := -1;
  if (FItemName = '') or (FItemValue = '') then
    Exit;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'INSERT INTO TestTable (MyID, ItemName, ItemValue) ' +
      'VALUES (:MyID, :ItemName, :ItemValue)';
    Query.ParamByName('MyID').AsInteger := FMyID;
    Query.ParamByName('ItemName').AsString := FItemName;
    Query.ParamByName('ItemValue').AsString := FItemValue;
    Result := Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TTestTable.UpdateTable: LongInt;
var
  Query: TFDQuery;
begin
  Result := -1;
  if (FMyID = -1) or (FItemName = '') or (FItemValue = '') then
    Exit;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'UPDATE TestTable SET ItemName = :ItemName, ' +
      'ItemValue = :ItemValue WHERE MyID = :MyID';
    Query.ParamByName('ItemName').AsString := FItem
````

At this point, the free service would simply not complete my codegen request even after multiple attempts without truncating the results. There is certainly a throttle in place to prevent one free session from consuming too much of the AI's time which is likely fairly expensive.

However, it does not distract from the fact that Chat GPT can easily create Delphi code from a database schema which I didn't know it could do. A **lesson learned** tonight is to ask for smaller code generations and you will likely get less truncated responses when using the free service. (It is best to not keep asking to extend one class - ask to generate a new smaller classes with specific functionality and simply manually combine the results.)

![Delphi Logo combined with ChatGPT logo](/assets/blog/Delphi-Programming/Delphi-ChatGPT.png)