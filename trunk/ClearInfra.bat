@echo off
@echo Protegendo o ZEOS de exclusões
attrib +R Packages\D7\Build\z*.*

@echo Apagando pacotes compilados no build
del Packages\D7\Build\*.exe
del Packages\D7\Build\*.dcp
del Packages\D7\Build\*.bpl

@echo Copiando os dfm da guibuilder para o build
copy Core\GUIBuilder\*.dfm Packages\D7\Build

@echo Apagando arquivos temporários
del *.~* /s
del *.dcu /s
del *.ddp /s
del Demos\GUIBuilder\*.exe
del UnitTests\bin\*.exe