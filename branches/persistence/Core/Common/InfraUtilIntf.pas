unit InfraUtilIntf;

interface

type
  IInfraVisitor = interface(IInterface)
    ['{DFEE74AA-CB25-4272-8EC5-F08461F74E49}']
    procedure Visit(const Item: IInterface);
  end;
  
  IInfraVisited = interface(IInterface)
    ['{6700A013-9162-433C-9FBB-7C6CC86DCE54}']
    procedure Accept(const Visitor: IInfraVisitor);
  end;

implementation

end.
