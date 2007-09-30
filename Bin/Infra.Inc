{#################
  This diretive define if you need Gexpert Debug, so u can use SendDebug
  methods. GExperts can be found in: http://www.gexperts.org
 #################}
{$DEFINE USE_GXDEBUG}

{#################
  This diretive define if you would like use FastMM to seek by leaks on your
  code. FastMM can be found in:
  http://sourceforge.net/projects/fastmm/
 #################}
{.$DEFINE USE_FASTMM}

{#################
  This diretive define if you would like control reference counting of infra
  objects. This diretive need USE_GXDEBUG;
 #################}
{.$DEFINE TEST_REFCOUNT}
{$IFDEF TEST_REFCOUNT}
  {$DEFINE USE_GXDEBUG}
{$ENDIF}

{#################
  This diretive define if you would like see object's reference count.
 #################}
{.$DEFINE SEE_REFCOUNT}

{#################
  This diretive define if you would like store the object into a list
  to verify if all objects was destoyed.
 #################}
{.$DEFINE INFRA_LEAKOBJECTS}

{#################
  This diretive define if you would like see objects's destroyed name.
 #################}
{.$DEFINE INFRA_CLASSNAMEDESTROYED}
{$IFDEF INFRA_CLASSNAMEDESTROYED}
  {$DEFINE USE_GXDEBUG}
{$ENDIF}

{#################
  This define is used on InfraAspect to you can debug the stub.
  You should be intercept just a method to can debug the stub.
 #################}
{.$DEFINE USE_DEBUG_STUB}
