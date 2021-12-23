unit f14tomcatabout;

{$mode objfpc}{$H+}

{
   F14 Tomcat About Form
   ---------------------

   Author: Hamish A Williams

   Created: December 2021

   Copyright 2012 Hamish A Williams
   Licensed under the Apache License, Version 2.0
}


interface


uses
  Classes, Forms, Controls, StdCtrls,
  LCLTranslator;


type

  { TF14TomcatAboutForm }

  TF14TomcatAboutForm = class(TForm)
    DescriptionMemo: TMemo;
    CopyrightLabel: TLabel;
    LicenseLabel: TLabel;
    ProgramNameLabel: TLabel;
    OKButton: TButton;
    procedure CopyrightLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LicenseLabelClick(Sender: TObject);
  private

  public

  end;


implementation

{$R *.lfm}


{ TF14TomcatAboutForm }

uses
  LCLIntf;


resourcestring
  DescriptionFirstParagraph = 'A program for tricking other programs into thinking you are active at the keyboard.  To the casual observer of your screen, however, it looks like a program serving you chirpy messages to keep you feeling positive.';
  DescriptionSecondParagraph = 'Right-click on the images of the F14 planes to open the webpage of the associated wikimedia page.';


procedure TF14TomcatAboutForm.FormCreate(Sender: TObject);
begin
  DescriptionMemo.Lines.Clear;
  DescriptionMemo.Lines.Add(DescriptionFirstParagraph);
  DescriptionMemo.Lines.Add('');
  DescriptionMemo.Lines.Add(DescriptionSecondParagraph);
  CopyrightLabel.Caption := 'Copyright 2021 Hamish A Williams';
  LicenseLabel.Caption := 'Licensed under the Apache License, Version 2.0'
end;


procedure TF14TomcatAboutForm.LicenseLabelClick(Sender: TObject);
begin
  OpenURL('http://www.apache.org/licenses/LICENSE-2.0')
end;


procedure TF14TomcatAboutForm.CopyrightLabelClick(Sender: TObject);
begin
  OpenURL('mailto:f14tomcat@heuristicadvances.co.uk?subject=F14%20Tomcat')
end;


end.

