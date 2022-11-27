unit PK.HardInfo.SafeArea.Default;

interface

uses
  System.Types
  , FMX.Forms
  , FMX.Types
  ;

type
  ISafeArea = Interface
    ['{6F46CC6C-DD38-46CF-8438-81AE8E7AA3BE}']
    function GetPxRect: TRect;
    function GetDpRect: TRectF;

    function GetMarginRect(const AForm: TCommonCustomForm): TRectF;
    procedure Update(const AForm: TCommonCustomForm);

    property PxRect: TRect read GetPxRect;
    property DpRect: TRectF read GetDpRect;
  end;

  ISafeAreaFactory = interface
    ['{0239AA6D-1ED5-470E-8243-E369A82493F9}']
    function CreateSafeArea: ISafeArea;
  end;

  TSafeAreaFactory = class(TInterfacedObject, ISafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; virtual; abstract;
  end;

  TCustomSafeArea = class(TInterfacedObject, ISafeArea)
  protected var
    FDpRect: TRectF;
    FPxRect: TRect;
    FMarginRect: TRectF;
  protected
    function GetPxRect: TRect;
    function GetDpRect: TRectF;

    function GetMarginRect(const AForm: TCommonCustomForm): TRectF; virtual;
    procedure Update(const AForm: TCommonCustomForm); virtual; abstract;
  public
    constructor Create; reintroduce;
  end;

  TSafeAreaUtils = record
  public
    class function GetActiveForm(
      const AForm: TCommonCustomForm): TCommonCustomForm; static;
    class function GetFormDisplay(
      const AForm: TCommonCustomForm): TDisplay; static;
    class function GetScale(const AForm: TCommonCustomForm): Single; static;
  end;

implementation

uses
  FMX.Platform;

{ TSafeAreaUtils }

class function TSafeAreaUtils.GetActiveForm(
  const AForm: TCommonCustomForm): TCommonCustomForm;
begin
  Result := AForm;

  if Result = nil then
    Result := Screen.ActiveForm;

  if Result = nil then
    Result := Application.MainForm;
end;

class function TSafeAreaUtils.GetFormDisplay(
  const AForm: TCommonCustomForm): TDisplay;
begin
  var F := GetActiveForm(AForm);

  if F = nil then
  begin
    Result := Default(TDisplay);

    for var i := 0 to Screen.DisplayCount - 1 do
    begin
      var D := Screen.Displays[i];

      if D.Primary then
      begin
        Result := D;
        Break;
      end;
    end;
  end
  else
    Result := Screen.DisplayFromForm(F);
end;

class function TSafeAreaUtils.GetScale(const AForm: TCommonCustomForm): Single;
begin
  Result := GetFormDisplay(AForm).Scale;
  if Result = 0 then
    Result := 1;
end;

{ TCustomSafeArea }

constructor TCustomSafeArea.Create;
begin
  inherited Create;
  Update(nil);
end;

function TCustomSafeArea.GetDpRect: TRectF;
begin
  Result := FDpRect;
end;

function TCustomSafeArea.GetMarginRect(const AForm: TCommonCustomForm): TRectF;
begin
  Update(AForm);
  Result := FMarginRect;
end;

function TCustomSafeArea.GetPxRect: TRect;
begin
  Result := FPxRect;
end;

end.
