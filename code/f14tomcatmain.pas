unit f14tomcatmain;

{$mode objfpc}{$H+}

{
   F14 Tomcat
   ----------

   Author: Hamish A Williams

   Created: December 2021

   A utility that stops a computer from going to sleep (and stops Teams from
   recognising that you're "away") by simulating the pressing of the F14 key
   every four minutes (or other period specified in the command line).

   The utility is either "in flight", in which case the key press will be
   simulated or "landed" where it is doing nothing.

   The utility accepts two parameters when run:

   -interval <n>           Sets the number of minutes between each simulated
                           keypress.
   -quotefile <filename>   Overrides the quotes to be display with those in the
                           given text file.

   For example, the parameters...

      -interval 1 -quotefile "test quotes.txt"

   sets the utility to simulate a keystroke every 1 minute and to use quotes
   from the text file named "test quotes.txt" in the same folder as the utility.

   Copyright 2012 Hamish A Williams
   Licensed under the Apache License, Version 2.0
}


interface


uses
  Classes, SysUtils, Forms, Controls, Graphics, ComCtrls, ExtCtrls,
  StdCtrls, LCLTranslator, Menus;


type

  { TF14TomcatMainForm }

  TF14TomcatMainForm = class(TForm)
    AboutButton: TButton;
    ExitButton: TButton;
    FlightButton: TButton;
    LandedImage: TImage;            // Images should be landscape and 270 x 178
    FlyingImage: TImage;
    WebsiteLandedMenuItem: TMenuItem;
    WebsiteFlyingMenuItem: TMenuItem;
    PicturePanel: TPanel;
    ButtonPanel: TPanel;
    WebsitePopupmenu: TPopupMenu;
    StatusBar: TStatusBar;
    Timer: TTimer;

    procedure AboutButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FlightButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
    procedure WebsiteFlyingMenuItemClick(Sender: TObject);
    procedure WebsiteLandedMenuItemClick(Sender: TObject);

  private
    FInterval : Integer;
    FInflight : Boolean;
    FFlightTerms : TStringList;
    FFlightTermsFilename : String;

    procedure ReadCommandLineParameters;
    procedure SetFileTermsFilename(const ANewFilename : String);
    procedure SetInflight(const AValue : Boolean);
    procedure SetInterval(const ANewInterval : Integer);

  protected
    function  GetFlightTermsCount : Integer;

  protected
    procedure DisplayStatus(const AValue : String);
    procedure PressF14Key;              // Simulate the pressing of the F14 key
    procedure ShowAboutForm;

    property  FlightTerms : TStringList read FFlightTerms;
    property  FlightTermsCount : Integer read GetFlightTermsCount;
    property  FlightTermsFilename : String read FFlightTermsFilename
                                           write SetFileTermsFilename;
    property  Interval : Integer read FInterval write SetInterval;
    property  Inflight : Boolean read FInflight write SetInflight;

  end;


var
  F14TomcatMainForm: TF14TomcatMainForm;


implementation

{$R *.lfm}


uses
  Windows,            // Required for KeyBd_Event
  LCLIntf,
  f14tomcatabout;


resourcestring
  { Phrases to display while the program is "in flight" to give the user
    some re-assurance that it's working and also to give the program an
    appearance of benign purpose to an observer.  Best < 45 characters. }
  FT51 = 'A goal without a plan is just a wish';
  FT01 = 'Above the clouds...';
  FT38 = 'Accept change';
  FT28 = 'Act as if what you do makes a difference.';
  FT19 = 'Aircrew only...';
  FT17 = 'An airplane takes off against the wind...';
  FT34 = 'Be gentle and mild with foes';
  FT37 = 'Be part of something bigger than yourself';
  FT31 = 'Be polite';
  FT14 = 'Be precise in your speech';
  FT16 = 'Be the pilot of your own flight';
  FT57 = 'Be tolerent with others...';
  FT49 = 'Bring me sunshine...';
  FT36 = 'Cultivate a passion';
  FT43 = 'Do not limit yourself';
  FT47 = 'Don''t be ashamed to ask for help';
  FT11 = 'Ease back on the throttle...';
  FT00 = 'Flying high...';
  FT25 = 'Friends are therapists you drink with.';
  FT06 = 'George is flying the plane now...';
  FT30 = 'Happiness doesn''t come microwave ready';
  FT44 = 'Hold your tongue and think first...';
  FT41 = 'However it ends, it was an experience';
  FT20 = 'Imagine being lost at Mach 3...';
  FT22 = 'I feel the need.  The need for...';
  FT58 = 'If it''s not right, do not do it...';
  FT55 = 'If something is wrong, fix it...';
  FT03 = 'In the wire...';
  FT39 = 'I was not, I was, I am not, I care not.';
  FT23 = 'I''m not lazy.  I''m in power saving mode.';
  FT21 = 'Keep flying the mission';
  FT09 = 'Keep your eyes out for bandits';
  FT05 = 'Let''s kick the tyres and light the fires';
  FT52 = 'Life doesn''t have Ctrl-Z.';
  FT24 = 'Life happens.  Coffee helps.';
  FT02 = 'Looking down on you...';
  FT04 = 'Man must rise above the Earth';
  FT56 = 'Make the best of what is in your power...';
  FT53 = 'Monday is one seventh of your life';
  FT46 = 'Never stop dreaming';
  FT10 = 'Only bug out if you really must';
  FT08 = 'Pass the crew juice...';
  FT35 = 'Remember that everything is an impression';
  FT12 = 'Remember thou must die';
  FT32 = 'Remember, the world is your native city';
  FT33 = 'Say only what is necessary';
  FT54 = 'Set your house in order...';
  FT45 = 'Sometimes it is better to stay silent';
  FT27 = 'Straighten up and fly right...';
  FT18 = 'Take-off is optional. Landing in mandatory.';
  FT50 = 'Take responsibility for the passengers';
  FT15 = 'Tell the truth or, at least, don''t lie';
  FT26 = 'Tequila Tuesday!';
  FT48 = 'The metric measured is the one that changes';
  FT29 = 'The second best time to plant a tree is now';
  FT07 = 'We''re flying through an air pocket...';
  FT13 = 'What can happen any time can happen today';
  FT40 = 'What you cannot avoid, welcome';
  FT42 = 'You need very few things to be happy';

  FTNoTermToDisplay = 'Radio''s dead...';

  InTheAirStatus = 'In the air...';
  LandButtonCaption = '&Land';
  LandButtonHint = 'Have the program sit and do nothing';
  OnTheGroundStatus = 'On the ground...';
  TakeOffButtonCaption = '&Take off!';
  TakeOffButtonHint = 'Get this thing in the air!';


const
  { Default flight terms to be used if no file is specified in the command
    line or if that file can't be found, opened, or is empty. }
  DEFAULT_FLIGHT_TERMS_COUNT = 59;
  DEFAULT_FLIGHT_TERMS : array [0..DEFAULT_FLIGHT_TERMS_COUNT - 1] of string =
                   (FT00, FT01, FT02, FT03, FT04, FT05, FT06, FT07, FT08, FT09,
                    FT10, FT11, FT12, FT13, FT14, FT15, FT16, FT17, FT18, FT19,
                    FT20, FT21, FT22, FT23, FT24, FT25, FT26, FT27, FT28, FT29,
                    FT30, FT31, FT32, FT33, FT34, FT35, FT36, FT37, FT38, FT39,
                    FT40, FT41, FT42, FT43, FT44, FT45, FT46, FT47, FT48, FT49,
                    FT50, FT51, FT52, FT53, FT54, FT55, FT56, FT57, FT58);

  { Default interval in minutes between simulated key presses. }
  DEFAULT_INTERVAL = 4;


{ TF14TomcatMainForm }


procedure TF14TomcatMainForm.AboutButtonClick(Sender: TObject);
begin
  ShowAboutForm
end;


procedure TF14TomcatMainForm.DisplayStatus(const AValue : String);
begin
  StatusBar.Panels[0].Text := FormatDateTime('hh:nn', now) + ' - ' + AValue;
  StatusBar.Refresh
end;


procedure TF14TomcatMainForm.ExitButtonClick(Sender: TObject);
begin
  Close
end;


procedure TF14TomcatMainForm.FlightButtonClick(Sender: TObject);
begin
  Inflight := not Inflight;
end;


procedure TF14TomcatMainForm.FormCreate(Sender: TObject);
begin
  InFlight := False;
  Interval := DEFAULT_INTERVAL;
  FlightTermsFilename := '';

  ReadCommandLineParameters
end;


procedure TF14TomcatMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = []) and (Key = VK_F1) then
    ShowAboutForm
end;


function TF14TomcatMainForm.GetFlightTermsCount : Integer;
begin
  if Assigned(FlightTerms) then
    Result := FlightTerms.Count
  else
    Result := 0
end;


procedure TF14TomcatMainForm.PressF14Key;
const
  KEYEVENTF_KEYDOWN   =   0;
var
  RandomFlightTerm : String;
begin
  { Simulate the keydown and keyup of the pressing of the F14 key. }
  KeyBd_Event(VK_F14, 0, KEYEVENTF_KEYDOWN, 0);
  KeyBd_Event(VK_F14, 0, KEYEVENTF_KEYUP, 0);

  { Update the status bar with some text to assure the user that the program
    is running and to give an appearance of purpose to an observer. }
  RandomFlightTerm := Trim(FlightTerms[Random(FlightTermsCount)]);
  if (RandomFlightTerm = '') then
    RandomFlightTerm := FTNoTermToDisplay;
  DisplayStatus(RandomFlightTerm)
end;


procedure TF14TomcatMainForm.ReadCommandLineParameters;
var
  i : Integer;
  Parameter : String;
  CommandLineInterval : String;
  ProposedInterval : Integer;
  ErrorLocation : Integer;
begin
  for i := 1 to ParamCount() do
  begin
    Parameter := Uppercase(ParamStr(i));
    if (Parameter = '-INTERVAL') or (Parameter = '-I') then
      if (i < ParamCount()) then
      begin
        CommandLineInterval := ParamStr(i + 1);
        Val(CommandLineInterval, ProposedInterval, ErrorLocation);
        if (ProposedInterval > 0) and (ErrorLocation = 0) then
          Interval := ProposedInterval
      end;
    if (Parameter = '-QUOTEFILE') or (Parameter = '-Q') then
      if (i < ParamCount()) then
        FlightTermsFilename := ParamStr(i + 1)
  end
end;


procedure TF14TomcatMainForm.SetFileTermsFilename(const ANewFilename : String);
var
  i : Integer;
begin
  { Ensure we've a clear list of flight terms. }
  if not Assigned(FlightTerms) then
    FFlightTerms := TStringList.Create;
  FFlightTerms.Clear;

  FFlightTermsFileName := Trim(ANewFilename);
  if (FFlightTermsFileName <> '') then
  begin
    { A file has been named.  Load it... }
    FlightTerms.LoadFromFile(FFlightTermsFilename);

    { ...and remove all blank lines. }
    for i := FlightTerms.Count - 1 downto 0 do
      if Trim(FlightTerms[i]) = '' then
        FlightTerms.Delete(i)
  end;
  if (FlightTermsCount = 0) then
    { Either a file hasn't been named or it can't be found or it's empty.
      Use the default flight terms. }
    for i := 0 to DEFAULT_FLIGHT_TERMS_COUNT - 1 do
      FlightTerms.Add(DEFAULT_FLIGHT_TERMS[i])
end;


procedure TF14TomcatMainForm.SetInflight(const AValue : Boolean);
begin
  Timer.Enabled := False;
  FInflight := AValue;
  if FInflight then
    begin
      FlightButton.Caption := LandButtonCaption;
      FlightButton.Hint := LandButtonHint
    end
  else
    begin
      FlightButton.Caption := TakeOffButtonCaption;
      FlightButton.Hint := TakeOffButtonHint
    end;
  StatusBar.Panels[0].Text := '';
  LandedImage.Visible := not Inflight;
  LandedImage.Align := alClient;
  FlyingImage.Visible := Inflight;
  FlyingImage.Align := alClient;
  if Inflight then
    DisplayStatus(InTheAirStatus)
  else
    DisplayStatus(OnTheGroundStatus);
  Timer.Enabled := Inflight
end;


procedure TF14TomcatMainForm.SetInterval(const ANewInterval : Integer);
begin
  if (ANewInterval > 0) then
  begin
    FInterval := ANewInterval;
    Timer.Interval := Interval * 60000
  end
end;


procedure TF14TomcatMainForm.ShowAboutForm;
var
  AboutForm : TF14TomcatAboutForm;
begin
  Application.CreateForm(TF14TomcatAboutForm, AboutForm);
  AboutForm.ShowModal;
  AboutForm.Free
end;


procedure TF14TomcatMainForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  PressF14Key;
  Timer.Enabled := True
end;


procedure TF14TomcatMainForm.WebsiteFlyingMenuItemClick(Sender: TObject);
begin
  OpenURL('https://commons.wikimedia.org/wiki/File:F-14_Tomcat_DF-SD-06-03497.jpg')
end;


procedure TF14TomcatMainForm.WebsiteLandedMenuItemClick(Sender: TObject);
begin
  OpenURL('https://commons.wikimedia.org/wiki/File:F-14_Tomcat_on_Carrier.jpeg')
end;


end.

