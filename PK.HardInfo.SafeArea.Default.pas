unit PK.HardInfo.SafeArea.Default;

interface

uses
  System.Types;

type
  ISafeArea = Interface
    ['{6F46CC6C-DD38-46CF-8438-81AE8E7AA3BE}']
    function GetPxRect: TRect;
    function GetDpRect: TRectF;
    function GetMarginRect: TRectF;
    procedure Update;

    property PxRect: TRect read GetPxRect;
    property DpRect: TRectF read GetDpRect;
    property MarginRect: TRectF read GetMarginRect;
  end;

  ISafeAreaFactory = interface
    ['{0239AA6D-1ED5-470E-8243-E369A82493F9}']
    function CreateSafeArea: ISafeArea;
  end;

  TSafeAreaFactory = class(TInterfacedObject, ISafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; virtual; abstract;
  end;

implementation

end.
