���������� ������������� ���� ������������ Grace:

1. ������������ ��������������� �� ������������� ���
   ��� �� ������ �� ������:
   
      program.asm

2. ����� ��� ����������������� (assembler):

      masm /Mx program.asm;

3. ����� ��� ������� (linker):

      link /tiny /noignorecase program.obj,program.com,nul,grace.lib;
