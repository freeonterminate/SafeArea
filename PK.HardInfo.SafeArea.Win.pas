(*
 * SafeArea Rect Utils Win
 *
 * LICENSE
 *   Copyright (c) 2022 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *)

unit PK.HardInfo.SafeArea.Win;

{$IFNDEF MSWINDOWS}
{$WARNINGS OFF 1011}
interface
implementation
end.
{$ENDIF}

interface

procedure RegisterSafeArea;

implementation

uses
  System.Types
  , FMX.Forms
  , FMX.Platform
  , FMX.Platform.Win
  , PK.HardInfo.SafeArea.Default
  ;

type
  TWinSafeArea = class(TCustomSafeArea)
  protected
    procedure Update(const AForm: TCommonCustomForm); override;
  end;

  TWinSafeAreaFactory = class(TSafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; override;
  end;

procedure RegisterSafeArea;
begin
  var Factory := TWinSafeAreaFactory.Create;
  TPlatformServices.Current.AddPlatformService(ISafeAreaFactory, Factory);
end;


{ TWinSafeAreaFactory }

function TWinSafeAreaFactory.CreateSafeArea: ISafeArea;
begin
  Result := TWinSafeArea.Create;
end;

{ TWinSafeArea }

procedure TWinSafeArea.Update(const AForm: TCommonCustomForm);
begin
  var D := TSafeAreaUtils.GetFormDisplay(AForm);

  FPxRect := D.PhysicalWorkarea;

  FDpRect.TopLeft := PxToDp(FPxRect.TopLeft);
  FDpRect.BottomRight := PxToDp(FPxRect.BottomRight);

  // Windows は OS が全部面倒を見てくれるので何もしない
  FMarginRect := TRectF.Empty;
end;

end.
